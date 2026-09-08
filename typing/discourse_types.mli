module Lid_set : Set.S with type elt = Longident.t
module Lid_map : Map.S with type key = Longident.t

module Item :
  sig
    type t = Shape.Sig_component_kind.t * Path.t
    val compare : t -> t -> int
  end

module Paths : Set.S with type elt = Item.t
val pp_paths : Format.formatter -> Paths.t -> unit

module String_map : Map.S with type key = String.t
module Lid_trie :
  sig
    type t = Trie of Paths.t * t String_map.t
    val pp_paths : Format.formatter -> Paths.t -> unit
    val pp : Format.formatter -> t -> unit
    val empty : t
    val is_empty : t -> bool
    val node : ?children:t String_map.t -> Paths.t -> t
    val trie_of_lid : ?children:t String_map.t -> Longident.t -> Paths.t -> t
    val singleton : Longident.t -> Paths.elt -> t
    val union : t -> t -> t
    val add : Longident.t -> Paths.elt -> t -> t
    val take : String_map.key -> t -> t option * t
    val reach : t -> Longident.t -> t option
    val to_seq : t -> unit -> (Longident.t * Paths.t) Seq.node
    val size : t -> int
    val pp_lid_paths : Format.formatter -> Longident.t * Paths.t -> unit
    val pp_seq : Format.formatter -> t -> unit
  end
type t = Paths.t
val empty : Paths.t
val singleton : Paths.elt -> Paths.t
val add : Paths.elt -> Paths.t -> Paths.t
val union : Paths.t -> Paths.t -> Paths.t
val pp : Format.formatter -> Paths.t -> unit
type discourse = { paths : Lid_trie.t; substs : Lid_set.t Lid_map.t; }
val pp_map : Format.formatter -> Paths.t Lid_map.t -> unit
