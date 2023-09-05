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
  | Constructor_declaration of constructor_declaration
  | Extension_constructor of extension_constructor
  | Label_declaration of label_declaration
  | Module_binding of module_binding
  | Module_declaration of module_declaration
  | Module_substitution of module_substitution
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
  cmt_usages_index : (index_item * Longident.t Location.loc) list
}

type error =
    Not_a_typedtree of string

let iter_on_parts (it : Tast_iterator.iterator) = function
  | Partial_structure s -> it.structure it s
  | Partial_structure_item s -> it.structure_item it s
  | Partial_expression e -> it.expr it e
  | Partial_pattern (_category, p) -> it.pat it p
  | Partial_class_expr ce -> it.class_expr it ce
  | Partial_signature s -> it.signature it s
  | Partial_signature_item s -> it.signature_item it s
  | Partial_module_type s -> it.module_type it s

let iter_on_annots (it : Tast_iterator.iterator) = function
  | Implementation s -> it.structure it s
  | Interface s -> it.signature it s
  | Packed _ -> ()
  | Partial_implementation array -> Array.iter (iter_on_parts it) array
  | Partial_interface array -> Array.iter (iter_on_parts it) array

module Local_reduce = Shape.Make_reduce(struct
    type env = Env.t
    let fuel = 10

    let read_unit_shape ~unit_name:_ = None

    let find_shape env id =
      let namespace = Shape.Sig_component_kind.Module in
      Env.shape_of_path ~namespace env (Pident id)
  end)

let iter_on_declarations ~(f: Shape.Uid.t -> item_declaration -> unit) =
  Tast_iterator.{ default_iterator with

  value_bindings = (fun sub ((_, vbs) as bindings) ->
    let bound_idents = let_bound_idents_full_with_bindings vbs in
    List.iter
      (fun (vb, (_id, _loc, _typ, uid)) -> f uid (Value_binding vb))
      bound_idents;
      default_iterator.value_bindings sub bindings);

  module_binding = (fun sub mb ->
    f mb.mb_decl_uid (Module_binding mb);
    default_iterator.module_binding sub mb);

  module_declaration = (fun sub md ->
    f md.md_uid (Module_declaration md);
    default_iterator.module_declaration sub md);

  module_type_declaration = (fun sub mtd ->
    f mtd.mtd_uid (Module_type_declaration mtd);
    default_iterator.module_type_declaration sub mtd);

  module_substitution = (fun sub ms ->
    f ms.ms_uid (Module_substitution ms);
    default_iterator.module_substitution sub ms);

  value_description = (fun sub vd ->
    f vd.val_val.val_uid (Value_description vd);
    default_iterator.value_description sub vd);

  type_declaration = (fun sub td ->
    (* compiler-generated "row_names" share the uid of their corresponding
       class declaration, so we ignore them to prevent duplication *)
    if not (Btype.is_row_name (Ident.name td.typ_id)) then begin
      f td.typ_type.type_uid (Type_declaration td);
      (* We also register records labels and constructors *)
      match td.typ_kind with
      | Ttype_variant constrs ->
          List.iter (fun ({ cd_uid; _ } as cd) ->
            f cd_uid (Constructor_declaration cd)) constrs
      | Ttype_record labels ->
          List.iter (fun ({ ld_uid; _ } as ld) ->
            f ld_uid (Label_declaration ld)) labels
      | _ -> ()
    end;
    default_iterator.type_declaration sub td);

  extension_constructor = (fun sub ec ->
    f ec.ext_type.ext_uid (Extension_constructor ec);
    default_iterator.extension_constructor sub ec);

  class_declaration = (fun sub cd ->
    f cd.ci_decl.cty_uid (Class_declaration cd);
    default_iterator.class_declaration sub cd);

  class_type_declaration = (fun sub ctd ->
    f ctd.ci_decl.cty_uid (Class_type_declaration ctd);
    default_iterator.class_type_declaration sub ctd);

  class_description =(fun sub cd ->
    f cd.ci_decl.cty_uid (Class_description cd);
    default_iterator.class_description sub cd);
}

let need_to_clear_env =
  try ignore (Sys.getenv "OCAML_BINANNOT_WITHENV"); false
  with Not_found -> true

let keep_only_summary = Env.keep_only_summary

let cenv =
  {Tast_mapper.default with env = fun _sub env -> keep_only_summary env}

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

let iter_on_usages ~index =
  let f ~namespace env path lid =
    let not_ghost { Location.loc = { loc_ghost; _ }; _ } = not loc_ghost in
    if not_ghost lid then
      try
        let shape = Env.shape_of_path ~namespace env path in
        let shape = Local_reduce.weak_reduce env shape in
        if not (Shape.is_closed shape) then
          index := (Unresolved shape, lid) :: !index
        else Option.iter
          (fun uid -> index := (Resolved uid, lid) :: !index)
          shape.Shape.uid
      with Not_found -> ()
  in
  let add_constructor_description env lid =
    function
    | { Types.cstr_tag = Cstr_extension (path, _); _ } ->
        f ~namespace:Extension_constructor env path lid
    | { Types.cstr_uid = Predef _; _ } -> ()
    | { Types.cstr_uid; _ } ->
        index := (Resolved cstr_uid, lid) :: !index
  in
  let add_label lid { Types.lbl_uid; _ } =
      index := (Resolved lbl_uid, lid) :: !index
  in
  let with_constraint ~env (_path, _lid, with_constraint) =
    match with_constraint with
    | Twith_module (path', lid') | Twith_modsubst (path', lid') ->
        f ~namespace:Module env path' lid'
    | _ -> ()
  in
  Tast_iterator.{ default_iterator with

  expr = (fun sub ({ exp_desc; exp_env; _ } as e) ->
      (match exp_desc with
      | Texp_ident (path, lid, _) ->
          f ~namespace:Value exp_env path lid
      | Texp_construct (lid, constr_desc, _) ->
          add_constructor_description exp_env lid constr_desc
      | Texp_field (_, lid, label_desc)
      | Texp_setfield (_, lid, label_desc, _) ->
        add_label lid label_desc
      | Texp_new (path, lid, _) ->
          f ~namespace:Class exp_env path lid
      | Texp_record { fields; _ } ->
        Array.iter (fun (label_descr, record_label_definition) ->
          match record_label_definition with
          | Overridden (lid, _) -> add_label lid label_descr
          | Kept _ -> ()) fields
      | _ -> ());
      default_iterator.expr sub e);

  typ =
    (fun sub ({ ctyp_desc; ctyp_env; _ } as ct) ->
      (match ctyp_desc with
      | Ttyp_constr (path, lid, _ctyps) ->
          f ~namespace:Type ctyp_env path lid
      | Ttyp_package {pack_path; pack_txt} ->
          f ~namespace:Module_type ctyp_env pack_path pack_txt
      | _ -> ());
      default_iterator.typ sub ct);

  pat =
    (fun (type a) sub
      ({ pat_desc; pat_extra; pat_env; _ } as pat : a general_pattern) ->
      (match pat_desc with
      | Tpat_construct (lid, constr_desc, _, _) ->
          add_constructor_description pat_env lid constr_desc
      | Tpat_record (fields, _) ->
          List.iter (fun (lid, label_descr, _) -> add_label lid label_descr)
          fields
      | _ -> ());
      List.iter  (fun (pat_extra, _, _) ->
        match pat_extra with
        | Tpat_open (path, lid, _) ->
            f ~namespace:Module pat_env path lid
        | Tpat_type (path, lid) ->
            f ~namespace:Type pat_env path lid
        | _ -> ())
        pat_extra;
      default_iterator.pat sub pat);

  binding_op = (fun sub ({bop_op_path; bop_op_name; bop_exp; _} as bop) ->
    let lid = { bop_op_name with txt = Longident.Lident bop_op_name.txt } in
    f ~namespace:Value bop_exp.exp_env bop_op_path lid;
    default_iterator.binding_op sub bop);

  module_expr =
    (fun sub ({ mod_desc; mod_env; _ } as me) ->
      (match mod_desc with
      | Tmod_ident (path, lid) -> f ~namespace:Module mod_env path lid
      | _ -> ());
      default_iterator.module_expr sub me);

  open_description =
    (fun sub ({ open_expr = (path, lid); open_env; _ } as od)  ->
      f ~namespace:Module open_env path lid;
      default_iterator.open_description sub od);

  module_type =
    (fun sub ({ mty_desc; mty_env; _ } as mty)  ->
      (match mty_desc with
      | Tmty_ident (path, lid) ->
          f ~namespace:Module_type mty_env path lid
      | Tmty_with (_mty, l) ->
          List.iter (with_constraint ~env:mty_env) l
      | Tmty_alias (path, lid) ->
          f ~namespace:Module mty_env path lid
      | _ -> ());
      default_iterator.module_type sub mty);

  class_expr =
    (fun sub ({ cl_desc; cl_env; _} as ce) ->
      (match cl_desc with
      | Tcl_ident (path, lid, _) -> f ~namespace:Class cl_env path lid
      | _ -> ());
      default_iterator.class_expr sub ce);

  class_type =
    (fun sub ({ cltyp_desc; cltyp_env; _} as ct) ->
      (match cltyp_desc with
      | Tcty_constr (path, lid, _) -> f ~namespace:Class_type cltyp_env path lid
      | _ -> ());
      default_iterator.class_type sub ct);

  signature_item =
    (fun sub ({ sig_desc; sig_env; _ } as sig_item) ->
      (match sig_desc with
      | Tsig_exception {
          tyexn_constructor = { ext_kind = Text_rebind (path, lid)}} ->
          f ~namespace:Extension_constructor sig_env path lid
      | Tsig_modsubst { ms_manifest; ms_txt } ->
          f ~namespace:Module sig_env ms_manifest ms_txt
      | Tsig_typext { tyext_path; tyext_txt } ->
          f ~namespace:Type sig_env tyext_path tyext_txt
      | _ -> ());
      default_iterator.signature_item sub sig_item);

  structure_item =
    (fun sub ({ str_desc; str_env; _ } as str_item) ->
      (match str_desc with
      | Tstr_exception {
          tyexn_constructor = { ext_kind = Text_rebind (path, lid)}} ->
          f ~namespace:Extension_constructor str_env path lid
      | Tstr_typext { tyext_path; tyext_txt } ->
          f ~namespace:Type str_env tyext_path tyext_txt
      | _ -> ());
      default_iterator.structure_item sub str_item)
}

let index_declarations binary_annots =
  let index : item_declaration Types.Uid.Tbl.t = Types.Uid.Tbl.create 16 in
  let f uid fragment = Types.Uid.Tbl.add index uid fragment in
  iter_on_annots (iter_on_declarations ~f) binary_annots;
  index

let index_usages binary_annots =
  let index : (index_item * Longident.t Location.loc) list ref = ref [] in
  iter_on_annots (iter_on_usages ~index) binary_annots;
  !index

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
         let cmt_usages_index = index_usages binary_annots in
         let cmt_annots = clear_env binary_annots in
         let cmt_uid_to_decl = index_declarations cmt_annots in
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
           cmt_uid_to_decl;
           cmt_impl_shape = shape;
           cmt_usages_index;
         } in
         output_cmt oc cmt)
  end;
  clear ()
