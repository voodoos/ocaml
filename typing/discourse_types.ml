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

(* This module exists to prevent a dependency cycle with Types. *)

let rec compare_longidents ?(compare_strings = String.compare)
    (l1 : Longident.t) (l2 : Longident.t) =
  match (l1, l2) with
  | Lident s1, Lident s2 -> compare_strings s1 s2
  | Lident _, _ -> -1
  | _, Lident _ -> 1
  | Ldot (l1, s1), Ldot (l2, s2) ->
    let c = compare_longidents l1.txt l2.txt in
    if c = 0 then compare_strings s1.txt s2.txt else c
  | Lapply (l1, l'1), Lapply (l2, l'2) ->
    let c = compare_longidents l1.txt l2.txt in
    if c = 0 then compare_longidents l'1.txt l'2.txt else c
  | Ldot _, Lapply _ -> -1
  | Lapply _, Ldot _ -> 1

module Lid_set = Set.Make (struct
  type t = Longident.t
  let compare a b = compare_longidents a b
end)

module Lid_map = Map.Make (struct
  type t = Longident.t
  let compare a b = compare_longidents a b
end)

module Item = struct
  type t = Shape.Sig_component_kind.t * Path.t

  (* Since we are versing these paths in a different structure (the
      priority queue) before shortening, it does not seems useful tu use a
      custom path comparison function here. *)

  let compare (_, p1) (_, p2) = Path.compare p1 p2
end

module Paths = Set.Make (Item)

let pp_item_list ppf items =
  let pp_sep ppf () = Format.fprintf ppf ";@;" in
  let paths = List.map (fun (_, p) -> p) items in
  Format.pp_print_list ~pp_sep (Format_doc.compat Path.print) ppf paths

let pp_paths ppf t = pp_item_list ppf (Paths.elements t)

module String_map = Map.Make (String)

module Lid_trie = struct
  type t = Trie of Paths.t * t String_map.t

  let pp_paths fmt paths =
    let open Format in
    fprintf fmt "[%a]" pp_paths paths

  let rec pp fmt (Trie (paths, tries)) =
    let open Format in
    let pp_map fmt (id, trie) =
      Format.fprintf fmt "@[<v 2>%s: %a@]" id pp trie
    in
    Format.fprintf fmt "%a :> %a" pp_paths paths (pp_print_seq pp_map)
      (String_map.to_seq tries)

  let empty = Trie (Paths.empty, String_map.empty)

  let is_empty (Trie (_, children)) = String_map.is_empty children

  let node ?(children = String_map.empty) paths = Trie (paths, children)

  let trie_of_lid ?children lid paths =
    let rec aux acc lid =
      match (lid : Longident.t) with
      | Lident id ->
        let map = String_map.singleton id acc in
        Trie (Paths.empty, map)
      | Ldot (lid, id) ->
        let acc = Trie (Paths.empty, String_map.singleton id.txt acc) in
        aux acc lid.txt
      | Lapply (lid, arg_lid) ->
        let arg =
          let acc = Trie (Paths.empty, String_map.singleton ")" acc) in
          aux acc arg_lid.txt
        in
        aux (Trie (Paths.empty, String_map.singleton "(" arg)) lid.txt
    in
    aux (node ?children paths) lid

  let singleton lid path = trie_of_lid lid (Paths.singleton path)

  let rec union (Trie (p1, m1)) (Trie (p2, m2)) =
    Trie
      ( Paths.union p1 p2,
        String_map.union (fun _key t1 t2 -> Some (union t1 t2)) m1 m2 )

  let add lid path t =
    let t' = trie_of_lid lid (Paths.singleton path) in
    union t t'

  let take name (Trie (paths, tries)) =
    let l, t, r = String_map.split name tries in
    let t = Option.map (fun t -> Trie (paths, String_map.singleton name t)) t in
    (t, Trie (paths, String_map.union (fun _ _ _ -> assert false) l r))

  let rec reach (Trie (_, tries) as t) lid =
    match (lid : Longident.t) with
    | Lident name -> String_map.find_opt name tries
    | Ldot (lid, name) ->
      let parent = reach t lid.txt in
      Option.bind parent (fun (Trie (_, tries)) ->
          String_map.find_opt name.txt tries)
    | Lapply (lid, arg_lid) ->
      let parent = reach t lid.txt in
      Option.bind parent (fun (Trie (_, tries)) ->
          let arg_trie = String_map.find_opt "(" tries in
          let arg_enc = Option.bind arg_trie (fun t -> reach t arg_lid.txt) in
          Option.bind arg_enc (fun (Trie (_, tries)) ->
              String_map.find_opt ")" tries))

  let to_seq t =
    let mknoloc = Location.mknoloc in
    let rec aux lid_acc (Trie (paths, tries)) seq =
      let seq () =
        String_map.fold
          (fun name t acc ->
            let lid =
              match (lid_acc, name) with
              | None :: tl, _ -> Some (Longident.Lident name) :: tl
              | l, "(" -> None :: l
              | Some arg :: Some lid :: tl, ")" ->
                Some (Longident.Lapply (mknoloc lid, mknoloc arg)) :: tl
              | Some lid :: tl, _ ->
                Some (Longident.Ldot (mknoloc lid, mknoloc name)) :: tl
              | _ -> assert false
            in
            aux lid t acc)
          tries seq
      in
      if not (Paths.is_empty paths) then
        Seq.Cons ((Option.get (List.hd lid_acc), paths), seq)
      else seq ()
    in
    fun () -> aux [ None ] t Seq.Nil

  let size t =
    let rec aux acc (Trie (paths, tries)) =
      String_map.fold
        (fun _ t acc -> aux (1 + acc) t)
        tries
        (Paths.cardinal paths + acc)
    in
    aux 0 t

  let pp_lid_paths ppf (lid, paths) =
    Format.fprintf ppf "@[<2>%a@ %a@]" Pprintast.longident lid pp_paths paths
  let pp_seq fmt t =
    let pp_sep fmt () = Format.fprintf fmt ";@ " in
    Format.fprintf fmt "%a"
      (Format.pp_print_seq ~pp_sep pp_lid_paths)
      (to_seq t)
end

(* The discourse of a declaration is stored in the declaration itself and
   marshalled into cmi files. Arrays are much more compact than balanced sets,
   both in memory and on disk: items are kept in sorted (by [Item.compare]),
   duplicate-free arrays. Discourses are small, so the linear insertion and
   merge below are cheap. Sets ([Paths]) are still used for the in-memory
   tries. *)
type t = { local: Item.t array; extern: Item.t array }
let empty = { local = [||]; extern = [||] }

let of_paths paths = Array.of_list (Paths.elements paths)

(* Given a sorted array [items], [search_sorted item items] returns the index at
   which [item] is, or should be inserted, and whether it is already there. *)
let search_sorted item items =
  let rec aux lo hi =
    if lo >= hi then lo, false
    else begin
      let mid = (lo + hi) / 2 in
      let c = Item.compare item items.(mid) in
      if c = 0 then mid, true
      else if c < 0 then aux lo mid
      else aux (mid + 1) hi
    end
  in
  aux 0 (Array.length items)

(* Already present items are not replaced. *)
let insert_uniq item items =
  let i, present = search_sorted item items in
  if present then items
  else begin
    let n = Array.length items in
    let result = Array.make (n + 1) item in
    Array.blit items 0 result 0 i;
    Array.blit items i result (i + 1) (n - i);
    result
  end

(* On ties the item of the first argument is kept. *)
let merge_sorted a b =
  let la = Array.length a and lb = Array.length b in
  if la = 0 then b
  else if lb = 0 then a
  else begin
    let result = Array.make (la + lb) a.(0) in
    let k = ref 0 in
    let push x = result.(!k) <- x; incr k in
    let i = ref 0 and j = ref 0 in
    while !i < la && !j < lb do
      let c = Item.compare a.(!i) b.(!j) in
      if c = 0 then begin push a.(!i); incr i; incr j end
      else if c < 0 then begin push a.(!i); incr i end
      else begin push b.(!j); incr j end
    done;
    for i = !i to la - 1 do push a.(i) done;
    for j = !j to lb - 1 do push b.(j) done;
    if !k = la + lb then result else Array.sub result 0 !k
  end

let filter f items = Array.of_list (List.filter f (Array.to_list items))

let filter_map f items =
  Array.to_list items |> List.filter_map f |> Paths.of_list |> of_paths

let fold f t acc =
  let fold items acc = Array.fold_left (fun acc item -> f item acc) acc items in
  fold t.extern (fold t.local acc)

let pp_items ppf items = pp_item_list ppf (Array.to_list items)

(* We record the stamp at the time of entering a new struct or sig during
   typing. This allows us to filter out siblings of the currently typed item
   from its discourse. *)
let current_nesting = Local_store.s_ref None

let with_nesting f =
  let saved = !current_nesting in
  current_nesting := Some (Ident.get_currentstamp ());
  Misc.try_finally f ~always:(fun () -> current_nesting := saved)

let is_sibling (path : Path.t) =
  match path with
  | Pident id ->
      (* A sibling must be an ident *)
      begin match !current_nesting with
      | None -> false
      | Some nesting_stamp ->
        (* Global and predef always have stamp 0 *)
        Ident.stamp id > nesting_stamp
      end
  | Pdot _ | Papply _ | Pextra_ty _ -> false

let add ?(predef = false) ((_, path) as item) t =
  let heads = Path.heads path in
  if not predef && List.for_all Ident.is_predef heads then t
  else if is_sibling path then t
  else if List.for_all Ident.global heads then
    { t with extern = insert_uniq item t.extern }
  else
    { t with local = insert_uniq item t.local }

let singleton ?predef i = add ?predef i empty

let union t t' = {
  local = merge_sorted t.local t'.local;
  extern = merge_sorted t.extern t'.extern }

let pp fmt t = pp_items fmt (merge_sorted t.local t.extern)

type discourse = { paths : Lid_trie.t; substs : Lid_set.t Lid_map.t }

let pp_map fmt t =
  let pp_sep fmt () = Format.fprintf fmt ";@ " in
  Format.fprintf fmt "%a"
    (Format.pp_print_seq ~pp_sep Lid_trie.pp_lid_paths)
    (Lid_map.to_seq t)
