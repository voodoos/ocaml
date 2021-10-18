module Uid : sig
  type t =
    | Compilation_unit of string
    | Item of { comp_unit: string; id: int }
    | Internal
    | Predef of string

  val reinit : unit -> unit

  val mk : current_unit:string -> t
  val of_compilation_unit_id : Ident.t -> t
  val of_predef_id : Ident.t -> t
  val internal_not_actually_unique : t

  val for_actual_declaration : t -> bool

  include Identifiable.S with type t := t
end

module Sig_component_kind : sig
  type t =
    | Value
    | Type
    | Module
    | Module_type
    | Extension_constructor
    | Class
    | Class_type

  val to_string : t -> string

  (** Whether the name of a component of that kind can appear in a type. *)
  val can_appear_in_types : t -> bool
end

module Item : sig
  type t

  val make : string -> Sig_component_kind.t -> t

  val value : Ident.t -> t
  val type_ : Ident.t -> t
  val module_ : Ident.t -> t
  val module_type : Ident.t -> t
  val extension_constructor : Ident.t -> t
  val class_ : Ident.t -> t
  val class_type : Ident.t -> t

  module Map : Map.S with type key = t
end

type var = Ident.t
type t =
  | Var of var * Uid.t
  | Abs of var * Uid.t option * t
  | App of t * t
  | Struct of Uid.t option * t Item.Map.t
  | Leaf of Uid.t
  | Proj of t * Item.t
  | Comp_unit of string

val print : Format.formatter -> t -> unit

module Map : sig
  type shape = t
  type nonrec t = t Item.Map.t

  val empty : t

  val add_value : t -> Ident.t -> Uid.t -> t
  val add_value_proj : t -> Ident.t -> shape -> t

  val add_type : t -> Ident.t -> Uid.t -> t
  val add_type_proj : t -> Ident.t -> shape -> t

  val add_module : t -> Ident.t -> shape -> t
  val add_module_proj : t -> Ident.t -> shape -> t

  val add_module_type : t -> Ident.t -> Uid.t -> t
  val add_module_type_proj : t -> Ident.t -> shape -> t

  val add_extcons : t -> Ident.t -> Uid.t -> t
  val add_extcons_proj : t -> Ident.t -> shape -> t

  val add_class : t -> Ident.t -> Uid.t -> t
  val add_class_proj : t -> Ident.t -> shape -> t

  val add_class_type : t -> Ident.t -> Uid.t -> t
  val add_class_type_proj : t -> Ident.t -> shape -> t
end

val fresh_var : ?name:string -> Uid.t -> var * t

val dummy_mod : t

val of_path :
  find_shape:(Sig_component_kind.t -> Ident.t -> t) ->
  ?ns:Sig_component_kind.t -> Path.t -> t

val make_var : Ident.t -> Uid.t -> t
val make_abs : var -> Uid.t option -> t -> t
val make_app : arg:t -> t -> t
val make_proj : t -> Item.t -> t
val make_persistent : string -> t
val make_functor : param:(Ident.t option) -> t -> t
val make_structure : Uid.t option -> Map.t -> t
val make_leaf : Uid.t -> t

val set_uid : t -> Uid.t -> t
val get_struct_uid : t -> Uid.t option

(* TODO: doc *)
module Make_reduce(Params : sig
    val fuel : int
    val read_unit_shape : unit_name:string -> t option
    val find_shape : Ident.t -> t
  end) : sig
  val reduce : t -> t
end
