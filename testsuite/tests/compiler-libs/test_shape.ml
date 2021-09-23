(* TEST
   flags = "-I ${ocamlsrcdir}/typing \
    -I ${ocamlsrcdir}/parsing"
   include ocamlcommon
   * expect
*)



(* Test todo:
 open ?

  open struct
  module M = struct
    let x = 3
  end
end

include M

let _ = x

+ test Typesubst et Modsubst

*)

open Types
module Ns = Shape.Sig_component_kind

(* A utility to find and print the shape of a module(type) *)
let print_shape_by_lid ?(ns = Ns.Module) env fmt lid =
  match ns with
  | Ns.Module ->
    Env.find_module_by_name lid env |> fst
    |> Env.shape_of_path ~ns env |> Shape.print fmt
  | Ns.Module_type ->
    Env.find_modtype_by_name lid env |> fst
    |> Env.shape_of_path ~ns env |> Shape.print fmt
  | _ -> Format.pp_print_string fmt "Only modules and modtypes have shapes."

[%%expect{|
module Ns = Types.Shape.Sig_component_kind
val print_shape_by_lid :
  ?ns:Ns.t -> Env.t -> Format.formatter -> Longident.t -> unit = <fun>
|}]

type kind = M | MT
let test_prog prog shapes_to_print =
  let ps = Parse.implementation (Lexing.from_string prog) in
  let ts, typs, sn, env = Typemod.type_structure (Env.initial_safe_string) ps in
  let print ?ns s =
    let lid = Parse.longident (Lexing.from_string s) in
    Format.fprintf Format.std_formatter "%s:@, %a"
    (Longident.flatten lid |> String.concat "; ")
    (print_shape_by_lid ?ns env) lid
  in
  List.iter
    (fun (kind, shape) -> match kind with
      | M -> print shape
      | MT -> print ~ns:Ns.Module_type shape)
    shapes_to_print

[%%expect{|
type kind = M | MT
val test_prog : string -> (kind * string) list -> unit = <fun>
|}]

let _ = let prog = {|
module type S = sig
  type t
end

module type Sx = sig
  include S
  val x : int
end

module M : Sx = struct
  type t
  let x = 42
end

module M' = struct
  include M
end

module MUnit = struct
  include Stdlib.Unit
end

module M'' (X : S) = struct
  include X
  type y = X.t
end

module type M''' = functor (X : S) -> sig
  include module type of X
  type u
end

|}
in test_prog prog
  [MT, "S"; MT, "Sx"; M, "M"; M, "M'"; M, "MUnit"; M, "M''"; MT, "M'''"]

[%%expect{|
S:
 Abs(shape-var-1/1418, Struct
     [
      ("t", type) -> Proj(Var shape-var-1/1418, ("t", type));
      ])
Sx:
 Abs(shape-var-2/1421, Struct
     [
      ("t", type) -> Proj(Var shape-var-2/1421, ("t", type));
      ("x", value) -> Proj(Var shape-var-2/1421, ("x", value));
      ])
M:
 Struct [
         ("t", type) -> Leaf .35;
         ("x", value) -> Leaf .36;
         ]
M':
 Struct [
         ("t", type) -> Leaf .35;
         ("x", value) -> Leaf .36;
         ]
MUnit:
 Struct
 [
  ("compare", value) -> Proj(Proj(Comp_unit Stdlib, ("Unit", module)),
  ("compare", value));
  ("equal", value) -> Proj(Proj(Comp_unit Stdlib, ("Unit", module)),
  ("equal", value));
  ("t", type) -> Proj(Proj(Comp_unit Stdlib, ("Unit", module)), ("t", type));
  ("to_string", value) -> Proj(Proj(Comp_unit Stdlib, ("Unit", module)),
  ("to_string", value));
  ]
M'':
 Abs(X/1440, Struct
     [
      ("t", type) -> Proj(Var X/1440, ("t", type));
      ("y", type) -> Leaf .41;
      ])
M''':
 Abs(X/1444,
     Abs(shape-var-3/1445, Struct
         [
          ("t", type) -> Proj(Var X/1444, ("t", type));
          ("u", type) -> Proj(Var shape-var-3/1445, ("u", type));
          ]))
- : unit = ()
|}]
