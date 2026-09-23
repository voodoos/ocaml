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

val record_usages : bool
val record : event -> unit

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

val get : unit -> Discourse_types.discourse
val debug_print : Format.formatter -> unit
