(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Fabrice Le Fessant, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Cmi_format
open Typedtree

(* Note that in Typerex, there is an awful hack to save a cmt file
   together with the interface file that was generated by ocaml (this
   is because the installed version of ocaml might differ from the one
   integrated in Typerex).
*)



let read_magic_number ic =
  let len_magic_number = String.length Config.cmt_magic_number in
  really_input_string ic len_magic_number

type binary_annots =
  | Packed of Types.signature * string list
  | Implementation of structure
  | Interface of signature
  | Partial_implementation of binary_part array
  | Partial_interface of binary_part array

and binary_part =
  | Partial_structure of structure
  | Partial_structure_item of structure_item
  | Partial_expression of expression
  | Partial_pattern : 'k pattern_category * 'k general_pattern -> binary_part
  | Partial_class_expr of class_expr
  | Partial_signature of signature
  | Partial_signature_item of signature_item
  | Partial_module_type of module_type

type uid_fragment =
  | Class_declaration of class_declaration
  | Class_description of class_description
  | Class_type_declaration of class_type_declaration
  | Extension_constructor of extension_constructor
  | Module_binding of module_binding
  | Module_declaration of module_declaration
  | Module_type_declaration of module_type_declaration
  | Type_declaration of type_declaration
  | Value_binding of value_binding
  | Value_description of value_description

type cmt_infos = {
  cmt_modname : string;
  cmt_annots : binary_annots;
  cmt_value_dependencies :
    (Types.value_description * Types.value_description) list;
  cmt_comments : (string * Location.t) list;
  cmt_args : string array;
  cmt_sourcefile : string option;
  cmt_builddir : string;
  cmt_loadpath : string list;
  cmt_source_digest : Digest.t option;
  cmt_initial_env : Env.t;
  cmt_imports : (string * Digest.t option) list;
  cmt_interface_digest : Digest.t option;
  cmt_use_summaries : bool;
  cmt_uid_to_loc : uid_fragment Shape.Uid.Tbl.t;
  cmt_impl_shape : Shape.t option; (* None for mli *)
}

type error =
    Not_a_typedtree of string

let need_to_clear_env =
  try ignore (Sys.getenv "OCAML_BINANNOT_WITHENV"); false
  with Not_found -> true

let keep_only_summary = Env.keep_only_summary


let uid_to_loc : uid_fragment Types.Uid.Tbl.t ref =
  Local_store.s_table Types.Uid.Tbl.create 16

let register_uid uid fragment =
  Types.Uid.Tbl.add !uid_to_loc uid fragment

let rec tast_map =
  let open Tast_mapper in
  let env_mapper =
    { default with env =
        if need_to_clear_env then (fun _sub env -> keep_only_summary env)
        else Tast_mapper.default.env }
  in
  fun ~env () -> { env_mapper with

  signature = (fun _sub ({sig_final_env; _} as str) ->
    (* Update the mapper's environment *)
    let sub = tast_map ~env:sig_final_env () in
    env_mapper.signature sub str);

  structure = (fun _sub ({str_final_env; _} as str) ->
    (* Update the mapper's environment *)
    let sub = tast_map ~env:str_final_env () in
    env_mapper.structure sub str);

  value_bindings = (fun sub bindings ->
    let bindings = env_mapper.value_bindings sub bindings in
    let ((_, vbs) as bindings) =
      env_mapper.value_bindings sub bindings
    in
    let bound_idents = let_bound_idents_full_with_bindings vbs in
    List.iter (fun (vb, (id, _loc, _typ)) ->
      try
        let vd = Env.find_value (Pident id) env in
        register_uid vd.val_uid
          (Value_binding vb)
      with Not_found -> ())
      bound_idents;
    bindings);

  module_binding = (fun sub mb ->
    let mb = env_mapper.module_binding sub mb in
    Option.iter
      (fun decl -> register_uid decl.Types.md_uid (Module_binding mb))
      mb.mb_decl;
    mb);

  module_declaration = (fun sub md ->
    let md = env_mapper.module_declaration sub md in
    Option.iter
      (fun decl -> register_uid decl.Types.md_uid (Module_declaration md))
      md.md_decl;
    md);

  module_type_declaration = (fun sub mtd ->
    let mtd = env_mapper.module_type_declaration sub mtd in
    register_uid mtd.mtd_decl.mtd_uid (Module_type_declaration mtd);
    mtd);

  value_description = (fun sub vd ->
    let vd = env_mapper.value_description sub vd in
    register_uid vd.val_val.val_uid (Value_description vd);
    vd);

  type_declaration = (fun sub td ->
    let td = env_mapper.type_declaration sub td in
    (* compiler-generated "row_names" share the uid of their corresponding
       class declaration, so we ignore them to prevent duplication *)
    if not (Btype.is_row_name (Ident.name td.typ_id)) then
      register_uid td.typ_type.type_uid (Type_declaration td);
    td);

  extension_constructor = (fun sub ec ->
    let ec = env_mapper.extension_constructor sub ec in
    register_uid ec.ext_type.ext_uid (Extension_constructor ec);
    ec);

  class_declaration = (fun sub cd ->
    let cd = env_mapper.class_declaration sub cd in
    register_uid cd.ci_decl.cty_uid (Class_declaration cd);
    cd);

  class_type_declaration = (fun sub ctd ->
    let ctd = env_mapper.class_type_declaration sub ctd in
    register_uid ctd.ci_decl.cty_uid (Class_type_declaration ctd);
    ctd);

  class_description =(fun sub cd ->
    let cd = env_mapper.class_description sub cd in
    register_uid cd.ci_decl.cty_uid (Class_description cd);
    cd);
  }

let clear_part =
  function
  | Partial_structure s ->
      let tast_map = tast_map ~env:s.str_final_env () in
      Partial_structure (tast_map.structure tast_map s)
  | Partial_structure_item s ->
        let tast_map = tast_map ~env:s.str_env () in
        Partial_structure_item (tast_map.structure_item tast_map s)
  | Partial_expression e ->
      let tast_map = tast_map ~env:e.exp_env () in
      Partial_expression (tast_map.expr tast_map e)
  | Partial_pattern (category, p) ->
      let tast_map = tast_map ~env:p.pat_env () in
      Partial_pattern (category, tast_map.pat tast_map p)
  | Partial_class_expr ce ->
      let tast_map = tast_map ~env:ce.cl_env () in
      Partial_class_expr (tast_map.class_expr tast_map ce)
  | Partial_signature s ->
      let tast_map = tast_map ~env:s.sig_final_env () in
      Partial_signature (tast_map.signature tast_map s)
  | Partial_signature_item s ->
      let tast_map = tast_map ~env:s.sig_env () in
      Partial_signature_item (tast_map.signature_item tast_map s)
  | Partial_module_type s ->
    let tast_map = tast_map ~env:s.mty_env () in
    Partial_module_type (tast_map.module_type tast_map s)

let clear_env binary_annots =
  match binary_annots with
  | Implementation s ->
      let tast_map = tast_map ~env:s.str_final_env () in
      Implementation (tast_map.structure tast_map s)
  | Interface s ->
      let tast_map = tast_map ~env:s.sig_final_env () in
      Interface (tast_map.signature tast_map s)
  | Packed _ -> binary_annots
  | Partial_implementation array ->
      Partial_implementation (Array.map clear_part array)
  | Partial_interface array ->
      Partial_interface (Array.map clear_part array)

exception Error of error

let input_cmt ic = (input_value ic : cmt_infos)

let output_cmt oc cmt =
  output_string oc Config.cmt_magic_number;
  Marshal.(to_channel oc (cmt : cmt_infos) [Compression])

let read filename =
(*  Printf.fprintf stderr "Cmt_format.read %s\n%!" filename; *)
  let ic = open_in_bin filename in
  Misc.try_finally
    ~always:(fun () -> close_in ic)
    (fun () ->
       let magic_number = read_magic_number ic in
       let cmi, cmt =
         if magic_number = Config.cmt_magic_number then
           None, Some (input_cmt ic)
         else if magic_number = Config.cmi_magic_number then
           let cmi = Cmi_format.input_cmi ic in
           let cmt = try
               let magic_number = read_magic_number ic in
               if magic_number = Config.cmt_magic_number then
                 let cmt = input_cmt ic in
                 Some cmt
               else None
             with _ -> None
           in
           Some cmi, cmt
         else
           raise(Cmi_format.Error(Cmi_format.Not_an_interface filename))
       in
       cmi, cmt
    )

let read_cmt filename =
  match read filename with
      _, None -> raise (Error (Not_a_typedtree filename))
    | _, Some cmt -> cmt

let read_cmi filename =
  match read filename with
      None, _ ->
        raise (Cmi_format.Error (Cmi_format.Not_an_interface filename))
    | Some cmi, _ -> cmi

let saved_types = ref []
let value_deps = ref []

let clear () =
  saved_types := [];
  value_deps := []

let add_saved_type b = saved_types := b :: !saved_types
let get_saved_types () = !saved_types
let set_saved_types l = saved_types := l

let record_value_dependency vd1 vd2 =
  if vd1.Types.val_loc <> vd2.Types.val_loc then
    value_deps := (vd1, vd2) :: !value_deps

let save_cmt filename modname binary_annots sourcefile initial_env cmi shape =
  if !Clflags.binary_annotations && not !Clflags.print_types then begin
    Misc.output_to_file_via_temporary
       ~mode:[Open_binary] filename
       (fun temp_file_name oc ->
         let this_crc =
           match cmi with
           | None -> None
           | Some cmi -> Some (output_cmi temp_file_name oc cmi)
         in
         let source_digest = Option.map Digest.file sourcefile in
         let cmt = {
           cmt_modname = modname;
           cmt_annots = clear_env binary_annots;
           cmt_value_dependencies = !value_deps;
           cmt_comments = Lexer.comments ();
           cmt_args = Sys.argv;
           cmt_sourcefile = sourcefile;
           cmt_builddir = Location.rewrite_absolute_path (Sys.getcwd ());
           cmt_loadpath = Load_path.get_paths ();
           cmt_source_digest = source_digest;
           cmt_initial_env = if need_to_clear_env then
               keep_only_summary initial_env else initial_env;
           cmt_imports = List.sort compare (Env.imports ());
           cmt_interface_digest = this_crc;
           cmt_use_summaries = need_to_clear_env;
           cmt_uid_to_loc = !uid_to_loc;
           cmt_impl_shape = shape;
         } in
         output_cmt oc cmt)
  end;
  clear ()
