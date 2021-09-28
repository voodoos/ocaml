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

module type MFS = functor (X : S) (Y : S) -> sig
  include module type of X
  type u
end

module MF : MFS  = functor (X : S) (Y : S) -> struct
  type t = X.t
  type u
end

|}
in test_prog prog
  [ MT, "S"; MT, "Sx"; M, "M"; M, "M'";
    M, "MUnit"; M, "M''"; MT, "MFS"; M, "MF" ]

[%%expect{|
S:
 Abs(shape-var-1/1420, Struct
     [
      ("t", type) -> Proj(Var shape-var-1/1420, ("t", type));
      ])
Sx:
 Abs(shape-var-2/1423, Struct
     [
      ("t", type) -> Proj(Var shape-var-2/1423, ("t", type));
      ("x", value) -> Proj(Var shape-var-2/1423, ("x", value));
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
 Abs(X/1443, Struct
     [
      ("t", type) -> Proj(Var X/1443, ("t", type));
      ("y", type) -> Leaf .41;
      ])
MFS:
 Abs(shape-var-4/1447,
     Abs(X/1449,
         Abs(Y/1451, Struct
             [
              ("t", type) -> Proj(Var X/1449, ("t", type));
              ("u", type) -> Proj(App(App(Var shape-var-4/1447, Var X/1449),
                                      Var Y/1451),
              ("u", type));
              ])))
MF:
 Abs(X/1449,
     Abs(Y/1451, Struct
         [
          ("t", type) -> Proj(Var X/1449, ("t", type));
          ("u", type) -> Leaf .50;
          ]))
- : unit = ()
|}]


let _ = let prog = {|
module type S = sig
  type t
  val x : t
end

module type S1 = functor (X : S) -> sig
  include module type of X
end

module type S2 = functor (X : S) -> sig
  include S
end

module type S3 = functor (X : S) -> S

module F1 (X : S) = struct
  include X
end

module F3 = (F1 : S2)

module F4 = (F1 : S1)
|}
in test_prog prog
  [ MT, "S"; MT, "S1"; MT, "S2"; MT, "S3"; M, "F1"; M, "F3";
     M, "F4"]

[%%expect{|
S:
 Abs(shape-var-10/1464, Struct
     [
      ("t", type) -> Proj(Var shape-var-10/1464, ("t", type));
      ("x", value) -> Proj(Var shape-var-10/1464, ("x", value));
      ])
S1:
 Abs(shape-var-11/1468,
     Abs(X/1470, Struct
         [
          ("t", type) -> Proj(Var X/1470, ("t", type));
          ("x", value) -> Proj(Var X/1470, ("x", value));
          ]))
S2:
 Abs(shape-var-14/1475,
     Abs(X/1477, Struct
         [
          ("t", type) -> Proj(App(Var shape-var-14/1475, Var X/1477),
          ("t", type));
          ("x", value) -> Proj(App(Var shape-var-14/1475, Var X/1477),
          ("x", value));
          ]))
S3:
 Abs(shape-var-16/1481,
     Abs(X/1483, Struct
         [
          ("t", type) -> Proj(App(Var shape-var-16/1481, Var X/1483),
          ("t", type));
          ("x", value) -> Proj(App(Var shape-var-16/1481, Var X/1483),
          ("x", value));
          ]))
F1:
 Abs(X/1486, Struct
     [
      ("t", type) -> Proj(Var X/1486, ("t", type));
      ("x", value) -> Proj(Var X/1486, ("x", value));
      ])
F3:
 Abs(X/1477, Struct
     [
      ("t", type) -> Proj(Var X/1477, ("t", type));
      ("x", value) -> Proj(Var X/1477, ("x", value));
      ])
F4:
 Abs(X/1470, Struct
     [
      ("t", type) -> Proj(Var X/1470, ("t", type));
      ("x", value) -> Proj(Var X/1470, ("x", value));
      ])
- : unit = ()
|}]
