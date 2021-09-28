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
Struct
[
 ("S", module type) ->
     Abs(shape-var-1/88,
         Struct [
                 ("t", type) -> Proj(Var shape-var-1/88, ("t", type));
                 ]);
 ]
module type S = sig type t end
|}]

module type Sx = sig
  include S
  val x : int
end
[%%expect{|
Struct
[
 ("Sx", module type) ->
     Abs(shape-var-4/94,
         Struct
         [
          ("t", type) -> Proj(Var shape-var-4/94, ("t", type));
          ("x", value) -> Proj(Var shape-var-4/94, ("x", value));
          ]);
 ]
module type Sx = sig type t val x : int end
|}]

module M : Sx = struct
  type t
  let x = 42
end
[%%expect{|
Struct [
        ("M", module) -> Leaf .6;
        ]
module M : Sx
|}]

module M' = struct
  include M
end
[%%expect{|
Struct [
        ("M'", module) -> Leaf .7;
        ]
module M' : sig type t = M.t val x : int end
|}]

module MUnit = struct
  include Stdlib.Unit
end
[%%expect{|
Struct [
        ("MUnit", module) -> Leaf .8;
        ]
module MUnit :
  sig
    type t = unit = ()
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val to_string : t -> string
  end
|}]

module M'' (X : S) = struct
  include X
  type y = X.t
end
[%%expect{|
Struct [
        ("M''", module) -> Leaf .11;
        ]
module M'' : functor (X : S) -> sig type t = X.t type y = X.t end
|}]

module type MFS = functor (X : S) (Y : S) -> sig
  include module type of X
  type u
end
[%%expect{|
Struct
[
 ("MFS", module type) ->
     Abs(shape-var-8/127,
         Abs(X/129,
             Abs(Y/131,
                 Struct
                 [
                  ("t", type) -> Proj(Var X/129, ("t", type));
                  ("u", type) ->
                      Proj(App(App(Var shape-var-8/127, Var X/129), Var Y/131),
                      ("u", type));
                  ])));
 ]
module type MFS = functor (X : S) (Y : S) -> sig type t type u end
|}]

module MF : MFS  = functor (X : S) (Y : S) -> struct
  type t = X.t
  type u
end
[%%expect{|
Struct [
        ("MF", module) -> Leaf .20;
        ]
module MF : MFS
|}]

module type S = sig
  type t
  val x : t
end
[%%expect{|
Struct
[
 ("S", module type) ->
     Abs(shape-var-16/151,
         Struct
         [
          ("t", type) -> Proj(Var shape-var-16/151, ("t", type));
          ("x", value) -> Proj(Var shape-var-16/151, ("x", value));
          ]);
 ]
module type S = sig type t val x : t end
|}]

module type S1 = functor (X : S) -> sig
  include module type of X
end
[%%expect{|
Struct
[
 ("S1", module type) ->
     Abs(shape-var-19/159,
         Abs(X/161,
             Struct
             [
              ("t", type) -> Proj(Var X/161, ("t", type));
              ("x", value) -> Proj(Var X/161, ("x", value));
              ]));
 ]
module type S1 = functor (X : S) -> sig type t val x : t end
|}]

module type S2 = functor (X : S) -> sig
  include S
end
[%%expect{|
Struct
[
 ("S2", module type) ->
     Abs(shape-var-24/171,
         Abs(X/173,
             Struct
             [
              ("t", type) ->
                  Proj(App(Var shape-var-24/171, Var X/173),
                  ("t", type));
              ("x", value) ->
                  Proj(App(Var shape-var-24/171, Var X/173),
                  ("x", value));
              ]));
 ]
module type S2 = functor (X : S) -> sig type t val x : t end
|}]

module type S3 = functor (X : S) -> S
[%%expect{|
Struct
[
 ("S3", module type) ->
     Abs(shape-var-28/182,
         Abs(X/184,
             Struct
             [
              ("t", type) ->
                  Proj(App(Var shape-var-28/182, Var X/184),
                  ("t", type));
              ("x", value) ->
                  Proj(App(Var shape-var-28/182, Var X/184),
                  ("x", value));
              ]));
 ]
module type S3 = functor (X : S) -> S
|}]

module F1 (X : S) = struct
  include X
end
[%%expect{|
Struct [
        ("F1", module) -> Leaf .31;
        ]
module F1 : functor (X : S) -> sig type t = X.t val x : t end
|}]

module F3 = (F1 : S2)
[%%expect{|
Struct [
        ("F3", module) -> Leaf .32;
        ]
module F3 : S2
|}]

module F4 = (F1 : S1)
[%%expect{|
Struct [
        ("F4", module) -> Leaf .33;
        ]
module F4 : S1
|}]
