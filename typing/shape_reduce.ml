(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Ulysse Gérard, Thomas Refis, Tarides                   *)
(*                    Nathanaëlle Courant, OCamlPro                       *)
(*              Gabriel Scherer, projet Picube, INRIA Paris               *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Shape

type result =
  | Resolved of Uid.t
  | Resolved_alias of Uid.t * result
  | Unresolved of t
  | Approximated of Uid.t option
  | Internal_error_missing_uid

let rec print_result fmt result =
  match result with
  | Resolved uid ->
      Format.fprintf fmt "@[Resolved:@ %a@]" Uid.print uid
  | Resolved_alias (uid, r) ->
      Format.fprintf fmt "@[Alias:@ %a@] ->@ %a"
        Uid.print uid print_result r
  | Unresolved shape ->
      Format.fprintf fmt "@[Unresolved:@ %a@]" print shape
  | Approximated (Some uid) ->
      Format.fprintf fmt "@[Approximated:@ %a@]" Uid.print uid
  | Approximated None ->
      Format.fprintf fmt "Approximated: No uid"
  | Internal_error_missing_uid ->
      Format.fprintf fmt "Missing uid"


let find_shape env id =
  let namespace = Shape.Sig_component_kind.Module in
  Env.shape_of_path ~namespace env (Pident id)

let ghost_shape_of_module_type env (mty : Types.module_type) =
  let current_unit = Env.get_current_unit () in
  let new_definition_uid decl_uid =
    let uid = Uid.mk_param ~current_unit in
    Uid.Deps.record_declaration_dependency
      (Definition_to_declaration, uid, decl_uid);
    uid
  in
  let open Types in
  let shape_map_labels =
    List.fold_left (fun map { Types.ld_id; ld_uid; _} ->
      let uid = new_definition_uid ld_uid in
      Shape.Map.add_label map ld_id uid)
      Shape.Map.empty
  in
  let shape_map_cstrs =
    List.fold_left
      (fun map { Types.cd_id; cd_uid; cd_args; _ } ->
      let cstr_shape_map =
        let label_decls =
          match cd_args with
          | Cstr_tuple _ -> []
          | Cstr_record ldecls -> ldecls
        in
        shape_map_labels label_decls
      in
      let uid = new_definition_uid cd_uid in
      Shape.Map.add_constr map cd_id
        @@ Shape.str ~uid cstr_shape_map)
      (Shape.Map.empty)
  in
  let rec aux ?uid = function
    | Mty_ident path | Mty_alias path ->
        begin match Env.find_modtype path env with
        | exception _ | { mtd_type = None; _ } -> Shape.dummy_mod
        | { mtd_type = Some mt; mtd_uid; _ } -> aux ~uid:mtd_uid mt
        end
    | Mty_signature s ->
        Shape.str ?uid @@
        List.fold_left (fun map -> function
          | Sig_value (id, vd, _) ->
              let uid = new_definition_uid vd.val_uid in
              Shape.Map.add_value map id uid
          | Sig_type (id, td, _, _) ->
              let typ_shape =
                let uid = new_definition_uid td.type_uid in
                match td.type_kind with
                | Type_variant (cstrs, _) ->
                    Shape.str ~uid (shape_map_cstrs cstrs)
                | Type_record (labels, _) ->
                    Shape.str ~uid (shape_map_labels labels)
                | Type_abstract _ | Type_open | Type_external _ -> Shape.leaf uid
              in
              Shape.Map.add_type map id typ_shape
          | Sig_typext (id, ec, _, _) ->
              let uid = new_definition_uid ec.ext_uid in
                let shape =
                  let map =  match ec.ext_args with
                  | Cstr_record lbls -> shape_map_labels lbls
                  | _ -> Shape.Map.empty
                  in
                  Shape.str ~uid map
              in
              Shape.Map.add_extcons map id shape
          | Sig_module (id, _, md, _, _) ->
              let uid = new_definition_uid md.md_uid in
              let md_shape = aux ~uid md.md_type in
              Shape.Map.add_module map id md_shape
          | Sig_modtype (id, mtd, _) ->
              let uid = new_definition_uid mtd.mtd_uid in
              Shape.Map.add_module_type map id uid
          | Sig_class (id, cty, _, _) ->
              let uid = new_definition_uid cty.cty_uid in
              Shape.Map.add_class map id uid
          | Sig_class_type (id, clty, _, _) ->
              let uid = new_definition_uid clty.clty_uid in
              Shape.Map.add_class_type map id uid) Shape.Map.empty s
    | Mty_functor (_, _) -> (* TODO "not implemented" *)
       Shape.dummy_mod
  in
  aux mty

module Make(Params : sig
  val fuel : int
  val read_unit_shape : unit_name:string -> t option
end) = struct
  (* We implement a strong call-by-need reduction, following an
     evaluator from Nathanaelle Courant. *)

  type nf = { uid: Uid.t option; desc: nf_desc; approximated: bool }
  and nf_desc =
    | NVar of var * delayed_nf option
    | NApp of nf * nf
    | NAbs of local_env * var * t * delayed_nf
    | NStruct of delayed_nf Item.Map.t
    | NAlias of delayed_nf
    | NProj of nf * Item.t
    | NLeaf
    | NComp_unit of string
    | NError of string

  (* A type of normal forms for strong call-by-need evaluation.
     The normal form of an abstraction
       Abs(x, t)
     is a closure
       NAbs(env, x, t, dnf)
     when [env] is the local environment, and [dnf] is a delayed
     normal form of [t].

     A "delayed normal form" is morally equivalent to (nf Lazy.t), but
     we use a different representation that is compatible with
     memoization (lazy values are not hashable/comparable by default
     comparison functions): we represent a delayed normal form as
     just a not-yet-computed pair [local_env * t] of a term in a
     local environment -- we could also see this as a term under
     an explicit substitution. This delayed thunked is "forced"
     by calling the normalization function as usual, but duplicate
     computations are precisely avoided by memoization.
   *)
  and delayed_nf = Thunk of local_env * t

  and local_env = delayed_nf option Ident.Map.t
  (* When reducing in the body of an abstraction [Abs(x, body)], we
     bind [x] to [None] in the environment. [Some v] is used for
     actual substitutions, for example in [App(Abs(x, body), t)], when
     [v] is a thunk that will evaluate to the normal form of [t]. *)

  (* [_print_nf] is an (incomplete) printer for normal forms
     useful for debugging purposes *)
  let _print_nf fmt nf =
    let print_uid_opt =
      Format.pp_print_option (fun fmt -> Format.fprintf fmt "<%a>" Uid.print)
    in
    let rec aux fmt { uid; desc; _ }=
      match desc with
      | NVar (var, _ ) ->
          Format.fprintf fmt "%a%a" Ident.print var print_uid_opt uid
      | NProj (nf, item) ->
        Format.fprintf fmt "(%a.%a)%a" aux nf Item.print item print_uid_opt uid
      | _ -> ()
    in
    Format.fprintf fmt "@[%a@]@;" aux nf

  let approx_nf nf = { nf with approximated = true }

  let in_memo_table memo_table memo_key f arg =
    match Hashtbl.find memo_table memo_key with
    | res -> res
    | exception Not_found ->
        let res = f arg in
        Hashtbl.replace memo_table memo_key res;
        res

  type env = {
    fuel: int ref;
    global_env: Env.t;
    local_env: local_env;
    reduce_memo_table: (local_env * t, nf) Hashtbl.t;
    read_back_memo_table: (nf, t) Hashtbl.t;
  }

  let bind env var shape =
    { env with local_env = Ident.Map.add var shape env.local_env }

  let rec reduce_ env t =
    let local_env = env.local_env in
    let memo_key = (local_env, t) in
    in_memo_table env.reduce_memo_table memo_key (reduce__ env) t
  (* Memoization is absolutely essential for performance on this
     problem, because the normal forms we build can in some real-world
     cases contain an exponential amount of redundancy. Memoization
     can avoid the repeated evaluation of identical subterms,
     providing a large speedup, but even more importantly it
     implicitly shares the memory of the repeated results, providing
     much smaller normal forms (that blow up again if printed back
     as trees). A functor-heavy file from Irmin has its shape normal
     form decrease from 100Mio to 2.5Mio when memoization is enabled.

     Note: the local environment is part of the memoization key, while
     it is defined using a type Ident.Map.t of non-canonical balanced
     trees: two maps could have exactly the same items, but be
     balanced differently and therefore hash differently, reducing
     the effectivenss of memoization.
     This could in theory happen, say, with the two programs
       (fun x -> fun y -> ...)
     and
       (fun y -> fun x -> ...)
     having "the same" local environments, with additions done in
     a different order, giving non-structurally-equal trees. Should we
     define our own hash functions to provide robust hashing on
     environments?

     We believe that the answer is "no": this problem does not occur
     in practice. We can assume that identifiers are unique on valid
     typedtree fragments (identifier "stamps" distinguish
     binding positions); in particular the two program fragments above
     in fact bind *distinct* identifiers x (with different stamps) and
     different identifiers y, so the environments are distinct. If two
     environments are structurally the same, they must correspond to
     the evaluation evnrionments of two sub-terms that are under
     exactly the same scope of binders. So the two environments were
     obtained by the same term traversal, adding binders in the same
     order, giving the same balanced trees: the environments have the
     same hash.
  *)

  and force env (Thunk (local_env, t)) =
    reduce_ { env with local_env } t

  and reduce__
    ({fuel; global_env; local_env; _} as env) (t : t) =
    let reduce env t = reduce_ env t in
    let delay_reduce env t = Thunk (env.local_env, t) in
    let return desc = { uid = t.uid; desc; approximated = t.approximated } in
    let rec force_aliases nf = match nf.desc with
      | NAlias delayed_nf ->
          let nf = force env delayed_nf in
          force_aliases nf
      | _ -> nf
    in
    let reset_uid_if_new_binding t' =
      match t.uid with
      | None -> t'
      | Some _ as uid -> { t' with uid }
    in
    if !fuel < 0 then approx_nf (return (NError "NoFuelLeft"))
    else
      match t.desc with
      | Comp_unit unit_name ->
          begin match Params.read_unit_shape ~unit_name with
          | Some t -> reduce env t
          | None -> return (NComp_unit unit_name)
          end
      | App(f, arg) ->
          let f = reduce env f |> force_aliases in
          begin match f.desc with
          | NAbs(clos_env, var, body, _body_nf) ->
              let arg = delay_reduce env arg in
              let env = bind { env with local_env = clos_env } var (Some arg) in
              reduce env body |> reset_uid_if_new_binding
          | _ ->
              let arg = reduce env arg in
              return (NApp(f, arg))
          end
      | Proj(str, item) ->
          let str = reduce env str |> force_aliases in
          let nored () = return (NProj(str, item)) in
          begin match str.desc with
          | NStruct (items) ->
              begin match Item.Map.find item items with
              | exception Not_found -> nored ()
              | nf -> force env nf |> reset_uid_if_new_binding
              end
          | _ ->
              nored ()
          end
      | Abs(var, body) ->
          let body_nf = delay_reduce (bind env var None) body in
          return (NAbs(local_env, var, body, body_nf))
      | Var (id, param_shape) ->
          let param_nf =
            Option.map (delay_reduce env) param_shape
          in
          begin match Ident.Map.find id local_env with
          (* Note: instead of binding abstraction-bound variables to
             [None], we could unify it with the [Some v] case by
             binding the bound variable [x] to [NVar x].

             One reason to distinguish the situations is that we can
             provide a different [Uid.t] location; for bound
             variables, we use the [Uid.t] of the bound occurrence
             (not the binding site), whereas for bound values we use
             their binding-time [Uid.t]. *)
          | None -> return (NVar (id, param_nf))
          | Some def ->
              begin match force env def with
              | { uid = Some _; _  } as nf -> nf
                  (* This var already has a binding uid *)
              | { uid = None; _ } as nf -> { nf with uid = t.uid }
                  (* Set the var's binding uid *)
              end
          | exception Not_found ->
          match find_shape global_env id with
          | exception Not_found -> return (NVar (id, param_nf))
          | res when res = t -> return (NVar (id, param_nf))
          | res ->
              decr fuel;
              reduce env res
          end
      | Leaf -> return NLeaf
      | Struct m ->
          let mnf = Item.Map.map (delay_reduce env) m in
          return (NStruct mnf)
      | Alias t -> return (NAlias (delay_reduce env t))
      | Error s -> approx_nf (return (NError s))

  and read_back env (nf : nf) : t =
    in_memo_table env.read_back_memo_table nf (read_back_ env) nf
  (* The [nf] normal form we receive may contain a lot of internal
     sharing due to the use of memoization in the evaluator. We have
     to memoize here again, otherwise the sharing is lost by mapping
     over the term as a tree. *)

  and read_back_ env (nf : nf) : t =
    { uid = nf.uid ;
      desc = read_back_desc env nf.desc;
      approximated = nf.approximated }

  and read_back_desc env desc =
    let read_back nf = read_back env nf in
    let read_back_force dnf = read_back (force env dnf) in
    match desc with
    | NVar (id, _param_shape) ->
        Var (id, None)
    | NApp (nft, nfu) ->
        App(read_back nft, read_back nfu)
    | NAbs (_env, x, _t, nf) ->
        Abs(x, read_back_force nf)
    | NStruct nstr ->
        Struct (Item.Map.map read_back_force nstr)
    | NAlias nf -> Alias (read_back_force nf)
    | NProj (nf, item) ->
        Proj (read_back nf, item)
    | NLeaf -> Leaf
    | NComp_unit s -> Comp_unit s
    | NError s -> Error s

  (* Sharing the memo tables is safe at the level of a compilation unit since
    idents should be unique *)
  let reduce_memo_table = Local_store.s_table Hashtbl.create 42
  let read_back_memo_table = Local_store.s_table Hashtbl.create 42

  let reduce global_env t =
    let fuel = ref Params.fuel in
    let local_env = Ident.Map.empty in
    let env = {
      fuel;
      global_env;
      reduce_memo_table = !reduce_memo_table;
      read_back_memo_table = !read_back_memo_table;
      local_env;
    } in
    reduce_ env t |> read_back env

  let rec is_stuck_on_comp_unit (nf : nf) =
    match nf.desc with
    | NVar _ ->
        (* This should not happen if we only reduce closed terms *)
        false
    | NApp (nf, _) | NProj (nf, _) -> is_stuck_on_comp_unit nf
    | NStruct _ | NAbs _ -> false
    | NAlias _ -> false
    | NComp_unit _ -> true
    | NError _ -> false
    | NLeaf -> false

  (* POC *)
  let mty_memo : Shape.t Ident.Tbl.t ref = Local_store.s_table Ident.Tbl.create 16
  let unstuck env nf =
    let exception Noop in
    let rec aux env nf =
      match nf.desc with
      | NVar (id, _) -> begin
          match Ident.Tbl.find_opt !mty_memo id with
          | Some shape -> shape
          | None ->
              try
                let { Types.md_type; _ } =
                  Env.find_module (Path.Pident id) env.global_env
                in
                let shape = ghost_shape_of_module_type env.global_env md_type in
                Ident.Tbl.add !mty_memo id shape;
                shape

              with _ -> raise Noop
        end
      | NProj (nf1, item) ->  Shape.proj ?uid:nf.uid (aux env nf1) item
      | _ -> raise Noop
    in
    try
      aux env nf |> reduce_ env
    with Noop -> nf

  let rec reduce_aliases_for_uid ?(last = false) env (nf : nf) =
    match nf with
    | { uid = Some uid; desc = NAlias dnf; approximated = false; _ } ->
        let result = reduce_aliases_for_uid ~last env (force env dnf) in
        Resolved_alias (uid, result)
    | { uid = Some uid; approximated = false; _ } -> Resolved uid
    | { uid; approximated = true } -> Approximated uid
    | { uid = None; approximated = false; _ } ->
      if not last then
        (* Sometimes we are stuck on an free functor variable *)
        (* TODO this is not a robust way to detect that situation and first
           removing aliases might be wrong. *)
        reduce_aliases_for_uid ~last:true env (unstuck env nf)
      else
        (* A missing Uid after a complete reduction means the Uid was first
           missing in the shape which is a code error. Having the
           [Missing_uid] reported will allow Merlin (or another tool working
           with the index) to ask users to report the issue if it does happen.
        *)
        Internal_error_missing_uid

  let reduce_for_uid global_env t =
    let fuel = ref Params.fuel in
    let local_env = Ident.Map.empty in
    let env = {
      fuel;
      global_env;
      reduce_memo_table = !reduce_memo_table;
      read_back_memo_table = !read_back_memo_table;
      local_env;
    } in
    let nf = reduce_ env t in
    if is_stuck_on_comp_unit nf then
      Unresolved (read_back env nf)
    else
      reduce_aliases_for_uid env nf
end

module Local_reduce =
  Make(struct
    let fuel = 10
    let read_unit_shape ~unit_name:_ = None
  end)

let local_reduce = Local_reduce.reduce
let local_reduce_for_uid = Local_reduce.reduce_for_uid
