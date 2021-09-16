module Uid : sig
  type t

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

  module Map : Map.S with type key = t
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

val fresh_var : unit -> var

val of_path : find_shape:(Ident.t -> t) -> ?ns:Sig_component_kind.t -> Path.t -> t

val make_empty_sig : unit -> t
val make_persistent : string -> t
val make_functor : param:(Ident.t option) -> t -> t
val make_functor_app : arg:t -> t -> t
val make_structure : t Item.Map.t -> t
val make_coercion : sig_:t -> t -> t

val reduce_one : t -> t

(** "Reset" a module shape to be used as a module type shape *)
val unproj : t -> t

module Map : sig
  type shape = t
  type nonrec t = t Item.Map.t

  val add_value : t -> Ident.t -> Uid.t -> t
  val add_value_proj : t -> Ident.t -> var -> t

  val add_type : t -> Ident.t -> Uid.t -> t
  val add_type_proj : t -> Ident.t -> var -> t

  val add_module : t -> Ident.t -> Uid.t -> t
  val add_module_proj : t -> Ident.t -> var -> t

  val add_module_type : t -> Ident.t -> shape -> t
  val add_module_type_proj : t -> Ident.t -> var -> t

  val add_extcons : t -> Ident.t -> shape -> t
  val add_extcons_proj : t -> Ident.t -> var -> t
end
