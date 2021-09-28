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
 ("S", module type) ->
     Abs(shape-var-1/88, {
                          ("t", type) -> shape-var-1/88 . "t"[type];
                          });
 }
module type S = sig type t end
|}]

module type Sx = sig
  include S
  val x : int
end
[%%expect{|
{
 ("Sx", module type) ->
     Abs(shape-var-4/94,
         {
          ("t", type) -> shape-var-4/94 . "t"[type];
          ("x", value) -> shape-var-4/94 . "x"[value];
          });
 }
module type Sx = sig type t val x : int end
|}]

module M : Sx = struct
  type t
  let x = 42
end
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
module M' : sig type t = M.t val x : int end
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

module M'' (X : S) = struct
  include X
  type y = X.t
end
[%%expect{|
{
 ("M''", module) ->
     Abs(X/121, {
                 ("t", type) -> X/121 . "t"[type];
                 ("y", type) -> <.10>;
                 });
 }
module M'' : functor (X : S) -> sig type t = X.t type y = X.t end
|}]

module type MFS = functor (X : S) (Y : S) -> sig
  include module type of X
  type u
end
[%%expect{|
{
 ("MFS", module type) ->
     Abs(shape-var-8/127,
         Abs(X/129,
             Abs(Y/131,
                 {
                  ("t", type) -> X/129 . "t"[type];
                  ("u", type) -> shape-var-8/127(X/129)(Y/131) . "u"[type];
                  })));
 }
module type MFS = functor (X : S) (Y : S) -> sig type t type u end
|}]

(* FIXME: include not handled properly *)
module type MFS_indir = functor (X : Set.OrderedType) (Y : S) -> sig
  include module type of X
  type u
end
[%%expect{|
{
 ("MFS_indir", module type) ->
     Abs(shape-var-14/142,
         Abs(X/335,
             Abs(Y/337,
                 {
                  ("u", type) -> shape-var-14/142(X/335)(Y/337) . "u"[type];
                  })));
 }
module type MFS_indir =
  functor (X : Set.OrderedType) (Y : S) ->
    sig type t val compare : t -> t -> int type u end
|}]

module MF : MFS  = functor (X : S) (Y : S) -> struct
  type t = X.t
  type u
end
[%%expect{|
{
 ("MF", module) ->
     Abs(X/129,
         Abs(Y/131, {
                     ("t", type) -> X/129 . "t"[type];
                     ("u", type) -> <.23>;
                     }));
 }
module MF : MFS
|}]

module type S = sig
  type t
  val x : t
end
[%%expect{|
{
 ("S", module type) ->
     Abs(shape-var-24/454,
         {
          ("t", type) -> shape-var-24/454 . "t"[type];
          ("x", value) -> shape-var-24/454 . "x"[value];
          });
 }
module type S = sig type t val x : t end
|}]

module type S1 = functor (X : S) -> sig
  include module type of X
end
[%%expect{|
{
 ("S1", module type) ->
     Abs(shape-var-27/462,
         Abs(X/464,
             {
              ("t", type) -> X/464 . "t"[type];
              ("x", value) -> X/464 . "x"[value];
              }));
 }
module type S1 = functor (X : S) -> sig type t val x : t end
|}]

module type S2 = functor (X : S) -> sig
  include S
end
[%%expect{|
{
 ("S2", module type) ->
     Abs(shape-var-32/474,
         Abs(X/476,
             {
              ("t", type) -> shape-var-32/474(X/476) . "t"[type];
              ("x", value) -> shape-var-32/474(X/476) . "x"[value];
              }));
 }
module type S2 = functor (X : S) -> sig type t val x : t end
|}]

module type S3 = functor (X : S) -> S
[%%expect{|
{
 ("S3", module type) ->
     Abs(shape-var-36/485,
         Abs(X/487,
             {
              ("t", type) -> shape-var-36/485(X/487) . "t"[type];
              ("x", value) -> shape-var-36/485(X/487) . "x"[value];
              }));
 }
module type S3 = functor (X : S) -> S
|}]

module F1 (X : S) = struct
  include X
end
[%%expect{|
{
 ("F1", module) ->
     Abs(X/493,
         {
          ("t", type) -> X/493 . "t"[type];
          ("x", value) -> X/493 . "x"[value];
          });
 }
module F1 : functor (X : S) -> sig type t = X.t val x : t end
|}]

module Arg = struct
  type t = int
  let x = 0
end
[%%expect{|
{
 ("Arg", module) -> {
                     ("t", type) -> <.36>;
                     ("x", value) -> <.37>;
                     };
 }
module Arg : sig type t = int val x : int end
|}]

include F1 (Arg)
[%%expect{|
{
 ("t", type) -> <.36>;
 ("x", value) -> <.37>;
 }
type t = Arg.t
val x : t = 0
|}]

module F3 = (F1 : S2)
[%%expect{|
{
 ("F3", module) ->
     Abs(X/476,
         {
          ("t", type) -> X/476 . "t"[type];
          ("x", value) -> X/476 . "x"[value];
          });
 }
module F3 : S2
|}]

include F3 (Arg)
[%%expect{|
{
 ("t", type) -> <.36>;
 ("x", value) -> <.37>;
 }
type t = F3(Arg).t
val x : t = <abstr>
|}]

module F4 = (F1 : S1)
[%%expect{|
{
 ("F4", module) ->
     Abs(X/464,
         {
          ("t", type) -> X/464 . "t"[type];
          ("x", value) -> X/464 . "x"[value];
          });
 }
module F4 : S1
|}]

include F4 (Arg)
[%%expect{|
{
 ("t", type) -> <.36>;
 ("x", value) -> <.37>;
 }
type t = F4(Arg).t
val x : t = <abstr>
|}]

module type Foo = sig
  module type Sig = sig type t end
  module type Sig_alias = Sig

  include Sig_alias
end
[%%expect{|
{
 ("Foo", module type) ->
     Abs(shape-var-41/527,
         {
          ("Sig", module type) -> shape-var-41/527 . "Sig"[module type];
          ("Sig_alias", module type) ->
              shape-var-41/527 . "Sig_alias"[module type];
          ("t", type) -> shape-var-41/527 . "t"[type];
          });
 }
module type Foo =
  sig module type Sig = sig type t end module type Sig_alias = Sig type t end
|}]
