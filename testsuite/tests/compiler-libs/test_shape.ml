(* TEST
   flags = "-dshape"
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

module type S = sig
  type t
end
[%%expect{|
{
 ("S", module type) -> <.1>;
 }
module type S = sig type t end
|}]

module type Sx = sig
  include S
  val x : t
end
[%%expect{|
{
 ("Sx", module type) -> <.3>;
 }
module type Sx = sig type t val x : t end
|}]

module M = struct
  type t = int
  let x = 42
end
[%%expect{|
{
 ("M", module) -> {
                   ("t", type) -> <.4>;
                   ("x", value) -> <.5>;
                   };
 }
module M : sig type t = int val x : int end
|}]

module M : Sx = M
[%%expect{|
{
 ("M", module) -> {
                   ("t", type) -> <.4>;
                   ("x", value) -> <.5>;
                   };
 }
module M : Sx
|}]

module M' = struct
  include M
end
[%%expect{|
{
 ("M'", module) -> {
                    ("t", type) -> <.4>;
                    ("x", value) -> <.5>;
                    };
 }
module M' : sig type t = M.t val x : t end
|}]

module MS = (M' : S)
[%%expect{|
{
 ("MS", module) -> {
                    ("t", type) -> <.4>;
                    };
 }
module MS : S
|}]

module MUnit = struct
  include Stdlib.Unit
end
[%%expect{|
{
 ("MUnit", module) ->
     {
      ("compare", value) -> CU Stdlib . "Unit"[module] . "compare"[value];
      ("equal", value) -> CU Stdlib . "Unit"[module] . "equal"[value];
      ("t", type) -> CU Stdlib . "Unit"[module] . "t"[type];
      ("to_string", value) ->
          CU Stdlib . "Unit"[module] . "to_string"[value];
      };
 }
module MUnit :
  sig
    type t = unit = ()
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val to_string : t -> string
  end
|}]

module Simple_functor (X : S) = struct
  include X
  type y = X.t
end
[%%expect{|
{
 ("Simple_functor", module) ->
     Abs(X/116, {
                 ("t", type) -> X/116 . "t"[type];
                 ("y", type) -> <.12>;
                 });
 }
module Simple_functor : functor (X : S) -> sig type t = X.t type y = X.t end
|}]

module Simple_functor' (X : S) = struct
  type y = X.t
  include X
end
[%%expect{|
{
 ("Simple_functor'", module) ->
     Abs(X/124, {
                 ("t", type) -> X/124 . "t"[type];
                 ("y", type) -> <.15>;
                 });
 }
module Simple_functor' : functor (X : S) -> sig type y = X.t type t = X.t end
|}]

module type MFS = functor (X : S) (Y : S) -> sig
  include module type of X
  type u
end
[%%expect{|
{
 ("MFS", module type) -> <.20>;
 }
module type MFS = functor (X : S) (Y : S) -> sig type t type u end
|}]

module MF : MFS  = functor (X : S) (Y : S) -> struct
  type t = X.t
  type u
end
[%%expect{|
{
 ("MF", module) ->
     Abs(X/145, Abs(Y/147, {
                            ("t", type) -> <.23>;
                            ("u", type) -> <.24>;
                            }));
 }
module MF : MFS
|}]


module type S1 = functor (X : Sx) -> sig
  include module type of X
end
[%%expect{|
{
 ("S1", module type) -> <.27>;
 }
module type S1 = functor (X : Sx) -> sig type t val x : t end
|}]

module type S2 = functor (X : Sx) -> sig
  include Sx
end
[%%expect{|
{
 ("S2", module type) -> <.29>;
 }
module type S2 = functor (X : Sx) -> sig type t val x : t end
|}]

module type S3 = functor (X : Sx) -> Sx
[%%expect{|
{
 ("S3", module type) -> <.31>;
 }
module type S3 = functor (X : Sx) -> Sx
|}]

module F1 (X : Sx) = struct
  include X
end
[%%expect{|
{
 ("F1", module) ->
     Abs(X/179,
         {
          ("t", type) -> X/179 . "t"[type];
          ("x", value) -> X/179 . "x"[value];
          });
 }
module F1 : functor (X : Sx) -> sig type t = X.t val x : t end
|}]

module Arg = struct
  type t = int
  let x = 0
end
[%%expect{|
{
 ("Arg", module) -> {
                     ("t", type) -> <.34>;
                     ("x", value) -> <.35>;
                     };
 }
module Arg : sig type t = int val x : int end
|}]

include F1 (Arg)
[%%expect{|
{
 ("t", type) -> <.34>;
 ("x", value) -> <.35>;
 }
type t = Arg.t
val x : t = 0
|}]

module F3 = (F1 : S2)
[%%expect{|
{
 ("F3", module) ->
     Abs(X/179,
         {
          ("t", type) -> X/179 . "t"[type];
          ("x", value) -> X/179 . "x"[value];
          });
 }
module F3 : S2
|}]

include F3 (Arg)
[%%expect{|
{
 ("t", type) -> <.34>;
 ("x", value) -> <.35>;
 }
type t = F3(Arg).t
val x : t = <abstr>
|}]

module F4 = (F1 : S1)
[%%expect{|
{
 ("F4", module) ->
     Abs(X/179,
         {
          ("t", type) -> X/179 . "t"[type];
          ("x", value) -> X/179 . "x"[value];
          });
 }
module F4 : S1
|}]

include F4 (Arg)
[%%expect{|
{
 ("t", type) -> <.34>;
 ("x", value) -> <.35>;
 }
type t = F4(Arg).t
val x : t = <abstr>
|}]

module Coercion : sig
  val v : int
  type t
  module M : sig end
  exception E
end = struct
  let v = 3
  type t
  module M = struct end
  exception E
end
[%%expect{|
{
 ("Coercion", module) ->
     {
      ("E", extension constructor) -> <.42>;
      ("M", module) -> {
                        };
      ("t", type) -> <.40>;
      ("v", value) -> <.39>;
      };
 }
module Coercion : sig val v : int type t module M : sig end exception E end
|}]

module Fignore(_ : S) = struct let x = 3 end
[%%expect{|
{
 ("Fignore", module) -> Abs(shape-var/226, {
                                            ("x", value) -> <.48>;
                                            });
 }
module Fignore : S -> sig val x : int end
|}]

module Fgen() = struct let x = 3 end
[%%expect{|
{
 ("Fgen", module) -> {
                      ("x", value) -> <.50>;
                      };
 }
module Fgen : functor () -> sig val x : int end
|}]

module M = Fignore(struct type t = int end)
[%%expect{|
{
 ("M", module) -> {
                   ("x", value) -> <.48>;
                   };
 }
module M : sig val x : int end
|}]

module N = Fgen()
[%%expect{|
{
 ("N", module) -> {
                   ("x", value) -> <.50>;
                   };
 }
module N : sig val x : int end
|}]

class c = object
  val v = 3
end
[%%expect{|
{
 ("c", class) -> <.55>;
 }
class c : object val v : int end
|}]

class type ct = object
  val v : int
end
[%%expect{|
{
 ("ct", class type) -> <.59>;
 }
class type ct = object val v : int end
|}]


module type Fignore = functor (_ : S) -> sig
  val x : int
end
[%%expect{|
{
 ("Fignore", module type) -> <.61>;
 }
module type Fignore = S -> sig val x : int end
|}]

module F : Fignore = functor (_ : S) -> struct
  let x = 3
end
[%%expect{|
{
 ("F", module) -> Abs(shape-var/293, {
                                      ("x", value) -> <.62>;
                                      });
 }
module F : Fignore
|}]

module rec A : sig
   type t = Leaf of B.t
 end = struct
   type t = Leaf of B.t
 end
 and B
   : sig type t = int end
   = struct type t = int end

[%%expect{|
{
 ("A", module) -> {
                   ("t", type) -> <.72>;
                   };
 ("B", module) -> {
                   ("t", type) -> <.74>;
                   };
 }
module rec A : sig type t = Leaf of B.t end
and B : sig type t = int end
|}]

 module rec A : sig
   type t = Leaf of string | Node of ASet.t
   val compare: t -> t -> int
 end = struct
   type t = Leaf of string | Node of ASet.t
   let compare t1 t2 =
     match (t1, t2) with
     | (Leaf s1, Leaf s2) -> Stdlib.compare s1 s2
     | (Leaf _, Node _) -> 1
     | (Node _, Leaf _) -> -1
     | (Node n1, Node n2) -> ASet.compare n1 n2
 end
 and ASet
   : sig type t type elt = A.t val compare : t -> t -> int end
   = Set.Make(A)

[%%expect{|
{
 ("A", module) -> {
                   ("compare", value) -> <.94>;
                   ("t", type) -> <.91>;
                   };
 ("ASet", module) ->
     {
      ("compare", value) ->
          CU Stdlib . "Set"[module] . "Make"[module](A/325) .
          "compare"[value];
      ("elt", type) ->
          CU Stdlib . "Set"[module] . "Make"[module](A/325) . "elt"[type];
      ("t", type) ->
          CU Stdlib . "Set"[module] . "Make"[module](A/325) . "t"[type];
      };
 }
module rec A :
  sig
    type t = Leaf of string | Node of ASet.t
    val compare : t -> t -> int
  end
and ASet : sig type t type elt = A.t val compare : t -> t -> int end
|}]

module type Small = sig
  type t
end
[%%expect{|
{
 ("Small", module type) -> <.102>;
 }
module type Small = sig type t end
|}]

module type Big = sig
  type t
  val x : t
end
[%%expect{|
{
 ("Big", module type) -> <.105>;
 }
module type Big = sig type t val x : t end
|}]

module type BigToSmall = functor (X : Big) -> Small with type t = X.t
[%%expect{|
{
 ("BigToSmall", module type) -> <.108>;
 }
module type BigToSmall = functor (X : Big) -> sig type t = X.t end
|}]

module SmallToBig (X : Small) : Big with type t = X.t = struct
  type t = X.t
  let x : t = Obj.magic ()
end
[%%expect{|
{
 ("SmallToBig", module) ->
     Abs(X/849, {
                 ("t", type) -> <.110>;
                 ("x", value) -> <.111>;
                 });
 }
module SmallToBig : functor (X : Small) -> sig type t = X.t val x : t end
|}]

module F = (SmallToBig : BigToSmall)
[%%expect{|
{
 ("F", module) -> Abs(shape-var/933, {
                                      ("t", type) -> <.110>;
                                      });
 }
module F : BigToSmall
|}]


module type S = sig
  type t
  val x : t
end
module type Sres = sig
  type t
  val x : t
  val y : t
end
[%%expect{|
{
 ("S", module type) -> <.117>;
 }
module type S = sig type t val x : t end
{
 ("Sres", module type) -> <.121>;
 }
module type Sres = sig type t val x : t val y : t end
|}]

module LocalFunctor (X : S) : Sres with type t = X.t = struct
  include X
  let y = x
end
[%%expect{|
{
 ("LocalFunctor", module) ->
     Abs(X/949,
         {
          ("t", type) -> X/949 . "t"[type];
          ("x", value) -> X/949 . "x"[value];
          ("y", value) -> <.123>;
          });
 }
module LocalFunctor :
  functor (X : S) -> sig type t = X.t val x : t val y : t end
|}]

module rec Foo : sig
  type t = A | B of Bar.t
  val x : t
end = struct
  type t = A | B of Bar.t
  let x = A
end

and Bar : Sres with type t = Foo.t
    = LocalFunctor(Foo)
[%%expect{|
{
 ("Bar", module) ->
     {
      ("t", type) -> Foo/961 . "t"[type];
      ("x", value) -> Foo/961 . "x"[value];
      ("y", value) -> <.123>;
      };
 ("Foo", module) -> {
                     ("t", type) -> <.138>;
                     ("x", value) -> <.141>;
                     };
 }
module rec Foo : sig type t = A | B of Bar.t val x : t end
and Bar : sig type t = Foo.t val x : t val y : t end
|}]
