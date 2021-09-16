module Uid = struct
  type t =
    | Compilation_unit of string
    | Item of { comp_unit: string; id: int }
    | Internal
    | Predef of string

  include Identifiable.Make(struct
    type nonrec t = t

    let equal (x : t) y = x = y
    let compare (x : t) y = compare x y
    let hash (x : t) = Hashtbl.hash x

    let print fmt = function
      | Internal -> Format.pp_print_string fmt "<internal>"
      | Predef name -> Format.fprintf fmt "<predef:%s>" name
      | Compilation_unit s -> Format.pp_print_string fmt s
      | Item { comp_unit; id } -> Format.fprintf fmt "%s.%d" comp_unit id

    let output oc t =
      let fmt = Format.formatter_of_out_channel oc in
      print fmt t
  end)

  let id = ref (-1)

  let reinit () = id := (-1)

  let mk  ~current_unit =
      incr id;
      Item { comp_unit = current_unit; id = !id }

  let of_compilation_unit_id id =
    if not (Ident.persistent id) then
      Misc.fatal_errorf "Types.Uid.of_compilation_unit_id %S" (Ident.name id);
    Compilation_unit (Ident.name id)

  let of_predef_id id =
    if not (Ident.is_predef id) then
      Misc.fatal_errorf "Types.Uid.of_predef_id %S" (Ident.name id);
    Predef (Ident.name id)

  let internal_not_actually_unique = Internal

  let for_actual_declaration = function
    | Item _ -> true
    | _ -> false
end

module Sig_component_kind = struct
  type t =
    | Value
    | Type
    | Module
    | Module_type
    | Extension_constructor
    | Class
    | Class_type

  let to_string = function
    | Value -> "value"
    | Type -> "type"
    | Module -> "module"
    | Module_type -> "module type"
    | Extension_constructor -> "extension constructor"
    | Class -> "class"
    | Class_type -> "class type"

  let can_appear_in_types = function
    | Value
    | Extension_constructor ->
        false
    | Type
    | Module
    | Module_type
    | Class
    | Class_type ->
        true
end

module Item = struct
  module T = struct
    type t = string * Sig_component_kind.t
    let compare = compare

    let value id = Ident.name id, Sig_component_kind.Value
    let type_ id = Ident.name id, Sig_component_kind.Type
    let module_ id = Ident.name id, Sig_component_kind.Module
    let module_type id = Ident.name id, Sig_component_kind.Module_type
    let extension_constructor id =
      Ident.name id, Sig_component_kind.Extension_constructor
  end

  include T

  module Map = Map.Make(T)
end

type var = Ident.t
type t =
  | Var of var
  | Abs of var * t
  | App of t * t
  | Struct of t Item.Map.t
  | Leaf of Uid.t
  | Proj of t * Item.t
  | Comp_unit of string

let fresh_var =
  let unique_var_counter = ref 0 in
  fun () ->
    (unique_var_counter := !unique_var_counter + 1;
    Printf.sprintf "shape-var-%i" !unique_var_counter
    |> Ident.create_local)

let rec of_path ~find_shape ?(ns = Sig_component_kind.Module) =
  let ns_mod = Sig_component_kind.Module in
  function
  | Path.Pident id -> find_shape id
  | Path.Pdot (path, name) ->
    let t = of_path ~find_shape ~ns:ns_mod path in
    Proj (t, (name, ns))
  | Path.Papply (p1, p2) -> App(
      of_path ~find_shape ~ns:ns_mod p1,
      of_path ~find_shape ~ns:ns_mod p2
    )

let make_empty_sig () = Abs(fresh_var (), Struct Item.Map.empty)

let make_persistent s = Comp_unit s

let make_functor ~param body =
  match param with
  (* If the functor is generative or has nameless arg, shape is preserved *)
  | None -> body
  | Some id -> Abs(id, body)

let make_functor_app ~arg f = App(f, arg)

let make_structure shapes = Struct shapes

let make_coercion ~sig_ mod_ = App(sig_, mod_) (* TODO @ulysse and reduce ? *)

let reduce_one = function
  | App _ -> failwith "TODO @ulysse not implemented reduce"
  | t -> t

let unproj t =
  (* TODO @ulysse Write some examples !
      (for module typeof)
      Is it right not to go under lambdas ? *)
  let var = fresh_var () in
  let rec aux item = function
    | Struct shapes ->
      let shapes = Item.Map.mapi
        (fun item t -> aux (Some item) t) shapes
      in
      Struct shapes
    | Leaf _ as t -> (match item with
      | None -> t
      | Some item -> Proj(Var var, item))
    | (Comp_unit _ | Var _ | Proj _ | Abs _ | App _) as t -> t
  in
  Abs(var, aux None t)

module Map = struct
  type shape = t
  type nonrec t = t Item.Map.t

  let add_value t id uid = Item.Map.add (Item.value id) (Leaf uid) t
  let add_value_proj t id var =
    let item = Item.value id in
    Item.Map.add item (Proj(Var var, item)) t

  let add_type t id uid = Item.Map.add (Item.type_ id) (Leaf uid) t
  let add_type_proj t id var =
    let item = Item.type_ id in
    Item.Map.add item (Proj(Var var, item)) t

  let add_module t id uid = Item.Map.add (Item.module_ id) (Leaf uid) t
  let add_module_proj t id var =
    let item = Item.module_ id in
    Item.Map.add item (Proj(Var var, item)) t

  let add_module_type t id shape = Item.Map.add (Item.module_type id) shape t
  let add_module_type_proj t id var =
    let item = Item.module_type id in
    Item.Map.add item (Proj(Var var, item)) t

  let add_extcons t id shape =
    Item.Map.add (Item.extension_constructor id) shape t
  let add_extcons_proj t id var =
    let item = Item.extension_constructor id in
    Item.Map.add item (Proj(Var var, item)) t
end
