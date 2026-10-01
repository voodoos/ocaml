(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Ulysse Gérard, Tarides                           *)
(*                                                                        *)
(*   Copyright 2025 Tarides                                               *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

val compare_longidents :
  ?compare_strings:(String.t -> String.t -> int) ->
  Longident.t -> Longident.t -> int

module Lid_set : Set.S with type elt = Longident.t
module Lid_map : Map.S with type key = Longident.t

module Item : sig
  type t = Shape.Sig_component_kind.t * Path.t

  include Set.OrderedType with type t := t
end

module Paths : Set.S with type elt = Item.t

val pp_paths : Format.formatter -> Paths.t -> unit

module String_map : Map.S with type key = string

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

type t = { local: Item.t array; extern: Item.t array }
(** The discourse of a declaration: the paths the user wrote in its description.
    Both arrays are sorted by [Item.compare] and free of duplicates. *)

val empty : t

val with_nesting : (unit -> 'a) -> 'a
(** Records the current ident stamp so that we can skip adding "siblings" into
    an item's discourse. These items should land in the discourse already via
    other rules. *)

val add : ?predef:bool -> Item.t -> t -> t
(** Adds a path to the discourse if it is not a predef (unless [predef] is set
     to [true]) or a direct child of the struct / sig being typed.

    It is crucial to keep the discourse as small as possible to reduce the cost
    of applying substitutions and of storing additional information in the cmi
    files. *)


val singleton : ?predef:bool -> Paths.elt -> t
(** [singleton ?predef i] is [add ?predef i empty] *)

val union : t -> t -> t

val fold : (Item.t -> 'a -> 'a) -> t -> 'a -> 'a
(** Folds over the [local] items, then the [extern] ones. *)

val filter : (Item.t -> bool) -> Item.t array -> Item.t array
val filter_map : (Item.t -> Item.t option) -> Item.t array -> Item.t array
(** [filter_map] sorts its result and removes duplicates *)

val pp : Format.formatter -> t -> unit

type discourse = { paths : Lid_trie.t; substs : Lid_set.t Lid_map.t; }
val pp_map : Format.formatter -> Paths.t Lid_map.t -> unit
