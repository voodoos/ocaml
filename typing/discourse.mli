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

val trie_of_paths : Discourse_types.Paths.t -> Discourse_types.Lid_trie.t
val pp_d : Format.formatter -> Discourse_types.discourse -> unit
module U :
  sig
    module Disambiguate_id :
      sig type t val get_id : unit -> t val compare : t -> t -> int end
    type u_item = {
      item : Discourse_types.Item.t;
      env : Env.t option;
      disambiguator : Disambiguate_id.t;
    }
    module ItemSet :
      sig
        type elt = u_item
        type t
        val empty : t
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
        val map : (elt -> elt) -> t -> t
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val split : elt -> t -> t * bool * t
        val is_empty : t -> bool
        val is_singleton : t -> bool
        val mem : elt -> t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val subset : t -> t -> bool
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val to_list : t -> elt list
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
      end
    type event =
        Used of { kind : Shape.Sig_component_kind.t;
          lid : Longident.t Location.loc; path : Path.t; env : Env.t;
        }
      | Used_constructor of { env : Env.t; lid : Longident.t Location.loc;
          discourse : Discourse_types.t;
        }
      | Used_label of { env : Env.t; lid : Longident.t Location.loc;
          discourse : Discourse_types.t;
        }
      | Initial_discourse of Discourse_types.t
      | Defined of { kind : Shape.Sig_component_kind.t; id : Ident.t; }
      | Defined_module of { decl : Types.module_declaration; id : Ident.t; }
      | Defined_signature of Types.signature
      | Opened of { env : Env.t; path : Path.t; }
    type u = {
      u_paths : ItemSet.t Discourse_types.Lid_map.t;
      substs : Discourse_types.Lid_set.t Discourse_types.Lid_map.t;
      discourse : Discourse_types.Lid_trie.t;
      pending : event list;
    }
    val paths_union :
      ItemSet.t Discourse_types.Lid_map.t ->
      ItemSet.t Discourse_types.Lid_map.t ->
      ItemSet.t Discourse_types.Lid_map.t
    val pp_u : Format.formatter -> u -> unit
    val add_item_set :
      Discourse_types.Lid_map.key ->
      ItemSet.elt ->
      ItemSet.t Discourse_types.Lid_map.t ->
      ItemSet.t Discourse_types.Lid_map.t
    val add_item : Discourse_types.Lid_map.key -> ItemSet.elt -> u -> u
    val empty_u : u
    val g : u ref
    val get : unit -> u
    val set : u -> unit
    val reset : unit -> unit
    val record_usages : bool
    val record : event -> unit
    val merge_discourse : Discourse_types.Paths.t -> u -> u
    val fold_on_common_lid_and_path_segments :
      init:'a ->
      kind:Shape.Sig_component_kind.t ->
      f:('a -> Shape.Sig_component_kind.t -> Longident.t * Path.t -> 'a) ->
      Longident.t * Path.t -> 'a
    val add_all_components :
      Discourse_types.Lid_trie.t ->
      Discourse_types.Lid_trie.t -> Discourse_types.Lid_trie.t
    val add_subst :
      Discourse_types.Lid_set.t Discourse_types.Lid_map.t ->
      Path.t ->
      Discourse_types.Lid_set.elt ->
      Discourse_types.Lid_set.t Discourse_types.Lid_map.t
    val add_subst_u : Path.t -> Discourse_types.Lid_set.elt -> u -> u
    val lid_and_path_of_ident :
      ?root_lid:Longident.t ->
      ?root_path:Path.t -> Ident.t -> Longident.t * Path.t
    val define :
      from:'a ->
      Shape.Sig_component_kind.t ->
      ?root_path:Path.t -> ?root_lid:Longident.t -> Ident.t -> u -> u
    val define_signature :
      ?from:[> `File ] ->
      ?root_path:Path.t -> ?root_lid:Longident.t -> Types.signature -> u -> u
    val define_component :
      ?from:[> `File ] ->
      ?root_path:Path.t ->
      ?root_lid:Longident.t -> Types.signature_item -> u -> u
    val define_type :
      ?from:[> `File ] ->
      ?root_path:Path.t -> ?root_lid:Longident.t -> Ident.t -> u -> u
    val define_value :
      ?from:[> `File ] ->
      ?root_path:Path.t -> ?root_lid:Longident.t -> Ident.t -> u -> u
    val define_module :
      ?from:[> `File ] ->
      ?root_path:Path.t ->
      ?root_lid:Longident.t -> Types.module_declaration -> Ident.t -> u -> u
    val define_modtype :
      ?from:[> `File ] ->
      ?root_path:Path.t -> ?root_lid:Longident.t -> Ident.t -> u -> u
    val define_signature_for_open :
      root_path:Path.t ->
      root_lid:Longident.t option -> Subst.Lazy.signature -> u -> u
    val open_module : Env.t -> Path.t -> u -> u
    val add_used :
      Env.t ->
      Shape.Sig_component_kind.t ->
      Longident.t Location.loc -> Path.t -> u -> u
    val use_module : Env.t -> Longident.t Location.loc -> Path.t -> u -> u
    val use_modtype : Env.t -> Longident.t Location.loc -> Path.t -> u -> u
    val use_type : Env.t -> Longident.t Location.loc -> Path.t -> u -> u
    val use_value : Env.t -> Longident.t Location.loc -> Path.t -> u -> u
    val use_constructor_or_label :
      Env.t -> Longident.t Location.loc -> Discourse_types.Paths.t -> u -> u
    val apply_event : u -> event -> u
    val force : u -> u
  end
module D :
  sig
    val follow_aliases_adding_subst :
      'a ->
      Env.t ->
      Discourse_types.Lid_set.t Discourse_types.Lid_map.t ->
      Path.t ->
      Discourse_types.Lid_set.elt ->
      Discourse_types.Lid_set.t Discourse_types.Lid_map.t
    val special_rule_for_aliases :
      Env.t ->
      Discourse_types.discourse ->
      U.u ->
      Path.t ->
      Discourse_types.Lid_set.elt Location.loc ->
      Path.t -> Discourse_types.discourse * U.u
    val d3_rule :
      Env.t ->
      Longident.t Location.loc ->
      Path.t ->
      Discourse_types.Lid_trie.t ->
      Discourse_types.Lid_set.t Discourse_types.Lid_map.t ->
      Subst.Lazy.signature ->
      Discourse_types.Lid_trie.t *
      Discourse_types.Lid_set.t Discourse_types.Lid_map.t
    val module_consequences :
      Discourse_types.discourse ->
      U.u ->
      Env.t ->
      Discourse_types.Lid_set.elt ->
      Path.t -> Discourse_types.discourse * U.u
    val consequences :
      Discourse_types.discourse ->
      U.u ->
      Discourse_types.Lid_set.elt * U.u_item ->
      Discourse_types.discourse * U.u
    val add_from_u_to_d :
      Discourse_types.discourse ->
      U.u -> Longident.t * U.u_item -> Discourse_types.discourse * U.u
    val of_U : U.u -> Discourse_types.discourse
    val pp : Format.formatter -> Discourse_types.discourse -> unit
  end
module Disambiguate_id = U.Disambiguate_id
type u_item =
  U.u_item = {
  item : Discourse_types.Item.t;
  env : Env.t option;
  disambiguator : Disambiguate_id.t;
}
module ItemSet = U.ItemSet
type event =
  U.event =
    Used of { kind : Shape.Sig_component_kind.t;
      lid : Longident.t Location.loc; path : Path.t; env : Env.t;
    }
  | Used_constructor of { env : Env.t; lid : Longident.t Location.loc;
      discourse : Discourse_types.t;
    }
  | Used_label of { env : Env.t; lid : Longident.t Location.loc;
      discourse : Discourse_types.t;
    }
  | Initial_discourse of Discourse_types.t
  | Defined of { kind : Shape.Sig_component_kind.t; id : Ident.t; }
  | Defined_module of { decl : Types.module_declaration; id : Ident.t; }
  | Defined_signature of Types.signature
  | Opened of { env : Env.t; path : Path.t; }
type u =
  U.u = {
  u_paths : ItemSet.t Discourse_types.Lid_map.t;
  substs : Discourse_types.Lid_set.t Discourse_types.Lid_map.t;
  discourse : Discourse_types.Lid_trie.t;
  pending : event list;
}
val paths_union :
  ItemSet.t Discourse_types.Lid_map.t ->
  ItemSet.t Discourse_types.Lid_map.t -> ItemSet.t Discourse_types.Lid_map.t
val pp_u : Format.formatter -> u -> unit
val add_item_set :
  Discourse_types.Lid_map.key ->
  ItemSet.elt ->
  ItemSet.t Discourse_types.Lid_map.t -> ItemSet.t Discourse_types.Lid_map.t
val add_item : Discourse_types.Lid_map.key -> ItemSet.elt -> u -> u
val empty_u : u
val g : u ref
val set : u -> unit
val reset : unit -> unit
val record_usages : bool
val record : event -> unit
val merge_discourse : Discourse_types.Paths.t -> u -> u
val fold_on_common_lid_and_path_segments :
  init:'a ->
  kind:Shape.Sig_component_kind.t ->
  f:('a -> Shape.Sig_component_kind.t -> Longident.t * Path.t -> 'a) ->
  Longident.t * Path.t -> 'a
val add_all_components :
  Discourse_types.Lid_trie.t ->
  Discourse_types.Lid_trie.t -> Discourse_types.Lid_trie.t
val add_subst :
  Discourse_types.Lid_set.t Discourse_types.Lid_map.t ->
  Path.t ->
  Discourse_types.Lid_set.elt ->
  Discourse_types.Lid_set.t Discourse_types.Lid_map.t
val add_subst_u : Path.t -> Discourse_types.Lid_set.elt -> u -> u
val lid_and_path_of_ident :
  ?root_lid:Longident.t ->
  ?root_path:Path.t -> Ident.t -> Longident.t * Path.t
val define :
  from:'a ->
  Shape.Sig_component_kind.t ->
  ?root_path:Path.t -> ?root_lid:Longident.t -> Ident.t -> u -> u
val define_component :
  ?from:[> `File ] ->
  ?root_path:Path.t ->
  ?root_lid:Longident.t -> Types.signature_item -> u -> u
val define_value :
  ?from:[> `File ] ->
  ?root_path:Path.t -> ?root_lid:Longident.t -> Ident.t -> u -> u
val define_signature_for_open :
  root_path:Path.t ->
  root_lid:Longident.t option -> Subst.Lazy.signature -> u -> u
val add_used :
  Env.t ->
  Shape.Sig_component_kind.t -> Longident.t Location.loc -> Path.t -> u -> u
val use_constructor_or_label :
  Env.t -> Longident.t Location.loc -> Discourse_types.Paths.t -> u -> u
val apply_event : u -> event -> u
val force : u -> u
val use_module : Env.t -> Longident.t Location.loc -> Path.t -> unit
val use_modtype : Env.t -> Longident.t Location.loc -> Path.t -> unit
val use_type : Env.t -> Longident.t Location.loc -> Path.t -> unit
val use_value : Env.t -> Longident.t Location.loc -> Path.t -> unit
val use_constructor :
  Env.t ->
  Longident.t Location.loc -> Data_types.constructor_description -> unit
val use_label :
  Env.t -> Longident.t Location.loc -> Data_types.label_description -> unit
val add_initial_discourse : unit -> unit
val define_type : Ident.t -> unit
val define_modtype : Ident.t -> unit
val define_module : Types.module_declaration -> Ident.t -> unit
val define_signature : Types.signature -> unit
val open_module : Env.t -> Path.t -> unit
val force_g : unit -> u
val get : unit -> Discourse_types.discourse
val debug_print : Format.formatter -> unit
