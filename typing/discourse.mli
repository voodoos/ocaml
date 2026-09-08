val add_initial_discourse : unit -> unit

val define_type : Ident.t -> unit
val define_module : Types.module_declaration -> Ident.t -> unit
val define_modtype : Ident.t -> unit
val define_signature : Types.signature -> unit

val open_module : Env.t -> Path.t -> unit
val use_module : Env.t -> Longident.t Location.loc -> Path.t -> unit
val use_modtype : Env.t -> Longident.t Location.loc -> Path.t -> unit
val use_type : Env.t -> Longident.t Location.loc -> Path.t -> unit
val use_value : Env.t -> Longident.t Location.loc -> Path.t -> unit
val use_constructor :
  Env.t ->
  Longident.t Location.loc -> Data_types.constructor_description -> unit
val use_label :
  Env.t -> Longident.t Location.loc -> Data_types.label_description -> unit
val get : unit -> Discourse_types.discourse
val debug_print : Format.formatter -> unit
