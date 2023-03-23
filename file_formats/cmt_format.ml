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

type item_declaration =
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

type index_item =
  | Resolved of Uid.t
  | Unresolved of Shape.t

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
  cmt_uid_to_decl : item_declaration Shape.Uid.Tbl.t;
  cmt_impl_shape : Shape.t option; (* None for mli *)
  cmt_index : (index_item * Longident.t Location.loc) list
}

type error =
    Not_a_typedtree of string

let need_to_clear_env =
  try ignore (Sys.getenv "OCAML_BINANNOT_WITHENV"); false
  with Not_found -> true

let keep_only_summary = Env.keep_only_summary

let cenv =
  {Tast_mapper.default with env = fun _sub env -> keep_only_summary env}

let clear_decl = function
  | Class_declaration cd -> Class_declaration (cenv.class_declaration cenv cd)
  | Class_description cd -> Class_description (cenv.class_description cenv cd)
  | Class_type_declaration ctd ->
      Class_type_declaration (cenv.class_type_declaration cenv ctd)
  | Extension_constructor ec ->
      Extension_constructor (cenv.extension_constructor cenv ec)
  | Module_binding mb -> Module_binding (cenv.module_binding cenv mb)
  | Module_declaration md ->
      Module_declaration (cenv.module_declaration cenv md)
  | Module_type_declaration mtd ->
      Module_type_declaration (cenv.module_type_declaration cenv mtd)
  | Type_declaration td -> Type_declaration (cenv.type_declaration cenv td)
  | Value_binding vb -> Value_binding (cenv.value_binding cenv vb)
  | Value_description vd -> Value_description (cenv.value_description cenv vd)

let uid_to_decl : item_declaration Types.Uid.Tbl.t ref =
  Local_store.s_table Types.Uid.Tbl.create 16

let register_uid uid fragment =
  Types.Uid.Tbl.add !uid_to_decl uid (clear_decl fragment)

let shape_index : (index_item * Longident.t Location.loc) list ref =
  Local_store.s_ref []

module Local_reduce = struct
  let is_open = ref false

  include Shape.Make_reduce(struct
    type env = Env.t
    let fuel = 10

    let read_unit_shape ~unit_name:_ =
      is_open := true;
      None

    let find_shape env id =
      let namespace = Shape.Sig_component_kind.Module in
      Env.shape_of_path ~namespace env (Pident id)
  end)

  let weak env shape =
    is_open := false;
    weak_reduce env shape, !is_open
end

let add_loc_to_index env shape loc =
  let shape, is_open = Local_reduce.weak env shape in
  if is_open then
    shape_index := (Unresolved shape, loc) :: !shape_index
  else Option.iter
    (fun uid -> shape_index := (Resolved uid, loc) :: !shape_index)
    shape.Shape.uid


let iter_decl =
  Tast_iterator.{ default_iterator with

  value_bindings = (fun sub ((_, vbs) as bindings) ->
    let bound_idents = let_bound_idents_full_with_bindings vbs in
    List.iter
      (fun (vb, (_id, _loc, _typ, uid)) ->
        register_uid uid (Value_binding vb))
      bound_idents;
      default_iterator.value_bindings sub bindings);

  module_binding = (fun sub mb ->
    register_uid mb.mb_decl_uid (Module_binding mb);
    default_iterator.module_binding sub mb);

  module_declaration = (fun sub md ->
    register_uid md.md_uid (Module_declaration md);
    default_iterator.module_declaration sub md);

  module_type_declaration = (fun sub mtd ->
    register_uid mtd.mtd_uid (Module_type_declaration mtd);
    default_iterator.module_type_declaration sub mtd);

  value_description = (fun sub vd ->
    register_uid vd.val_val.val_uid (Value_description vd);
    default_iterator.value_description sub vd);

  type_declaration = (fun sub td ->
    (* compiler-generated "row_names" share the uid of their corresponding
       class declaration, so we ignore them to prevent duplication *)
    if not (Btype.is_row_name (Ident.name td.typ_id)) then
      register_uid td.typ_type.type_uid (Type_declaration td);
      default_iterator.type_declaration sub td);

  extension_constructor = (fun sub ec ->
    register_uid ec.ext_type.ext_uid (Extension_constructor ec);
    default_iterator.extension_constructor sub ec);

  class_declaration = (fun sub cd ->
    register_uid cd.ci_decl.cty_uid (Class_declaration cd);
    default_iterator.class_declaration sub cd);

  class_type_declaration = (fun sub ctd ->
    register_uid ctd.ci_decl.cty_uid (Class_type_declaration ctd);
    default_iterator.class_type_declaration sub ctd);

  class_description =(fun sub cd ->
    register_uid cd.ci_decl.cty_uid (Class_description cd);
    default_iterator.class_description sub cd);

  expr = (fun sub ({ exp_desc; exp_env; _ } as e) ->
      (match exp_desc with
      | Texp_ident (path, ({ loc = { loc_ghost = false; _ }; _ } as lid), _)
        -> (
          try
            let shape = Env.shape_of_path ~namespace:Value exp_env path in
            add_loc_to_index exp_env shape lid
          with Not_found -> ())
            (* Log.warn "No shape for expr %a at %a" Path.print path
              Location.print_loc lid.loc) *)
      | _ -> ());
      default_iterator.expr sub e);

  typ =
    (fun sub ({ ctyp_desc; ctyp_env; _ } as ct) ->
      (match ctyp_desc with
      | Ttyp_constr
        (path, ({ loc = { loc_ghost = false; _ }; _ } as lid), _ctyps) -> (
          try
            let shape = Env.shape_of_path ~namespace:Type ctyp_env path in
            add_loc_to_index ctyp_env shape lid
          with Not_found -> ())
      | _ -> ());
      default_iterator.typ sub ct);

  module_expr =
    (fun sub ({ mod_desc; mod_env; _ } as me) ->
      (match mod_desc with
      | Tmod_ident (path, lid) -> (
          try
            let shape = Env.shape_of_path ~namespace:Module mod_env path in
            add_loc_to_index mod_env shape lid
          with Not_found -> ())
      | _ -> ());
      default_iterator.module_expr sub me);
}

let clear_part = function
  | Partial_structure s -> Partial_structure (cenv.structure cenv s)
  | Partial_structure_item s ->
      Partial_structure_item (cenv.structure_item cenv s)
  | Partial_expression e -> Partial_expression (cenv.expr cenv e)
  | Partial_pattern (category, p) -> Partial_pattern (category, cenv.pat cenv p)
  | Partial_class_expr ce -> Partial_class_expr (cenv.class_expr cenv ce)
  | Partial_signature s -> Partial_signature (cenv.signature cenv s)
  | Partial_signature_item s ->
      Partial_signature_item (cenv.signature_item cenv s)
  | Partial_module_type s -> Partial_module_type (cenv.module_type cenv s)

let clear_env binary_annots =
  if need_to_clear_env then
    match binary_annots with
    | Implementation s -> Implementation (cenv.structure cenv s)
    | Interface s -> Interface (cenv.signature cenv s)
    | Packed _ -> binary_annots
    | Partial_implementation array ->
        Partial_implementation (Array.map clear_part array)
    | Partial_interface array ->
        Partial_interface (Array.map clear_part array)

  else binary_annots

let gather_declarations_in_part = function
  | Partial_structure s -> iter_decl.structure iter_decl s
  | Partial_structure_item s -> iter_decl.structure_item iter_decl s
  | Partial_expression e -> iter_decl.expr iter_decl e
  | Partial_pattern (_category, p) -> iter_decl.pat iter_decl p
  | Partial_class_expr ce -> iter_decl.class_expr iter_decl ce
  | Partial_signature s -> iter_decl.signature iter_decl s
  | Partial_signature_item s -> iter_decl.signature_item iter_decl s
  | Partial_module_type s -> iter_decl.module_type iter_decl s

let gather_declarations binary_annots =
  match binary_annots with
  | Implementation s -> iter_decl.structure iter_decl s
  | Interface s -> iter_decl.signature iter_decl s
  | Packed _ -> ()
  | Partial_implementation array -> Array.iter gather_declarations_in_part array
  | Partial_interface array -> Array.iter gather_declarations_in_part array

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
         gather_declarations binary_annots;
         let cmt_annots = clear_env binary_annots in
         let source_digest = Option.map Digest.file sourcefile in
         let cmt = {
           cmt_modname = modname;
           cmt_annots;
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
           cmt_uid_to_decl = !uid_to_decl;
           cmt_impl_shape = shape;
           cmt_index = !shape_index;
         } in
         output_cmt oc cmt)
  end;
  clear ()
