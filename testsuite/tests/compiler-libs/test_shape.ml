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
                     ("u", type) -> <.19>;
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
     Abs(shape-var-16/151,
         {
          ("t", type) -> shape-var-16/151 . "t"[type];
          ("x", value) -> shape-var-16/151 . "x"[value];
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
     Abs(shape-var-19/159,
         Abs(X/161,
             {
              ("t", type) -> X/161 . "t"[type];
              ("x", value) -> X/161 . "x"[value];
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
     Abs(shape-var-24/171,
         Abs(X/173,
             {
              ("t", type) -> shape-var-24/171(X/173) . "t"[type];
              ("x", value) -> shape-var-24/171(X/173) . "x"[value];
              }));
 }
module type S2 = functor (X : S) -> sig type t val x : t end
|}]

module type S3 = functor (X : S) -> S
[%%expect{|
{
 ("S3", module type) ->
     Abs(shape-var-28/182,
         Abs(X/184,
             {
              ("t", type) -> shape-var-28/182(X/184) . "t"[type];
              ("x", value) -> shape-var-28/182(X/184) . "x"[value];
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
     Abs(X/190,
         {
          ("t", type) -> X/190 . "t"[type];
          ("x", value) -> X/190 . "x"[value];
          });
 }
module F1 : functor (X : S) -> sig type t = X.t val x : t end
|}]

module F3 = (F1 : S2)
[%%expect{|
{
 ("F3", module) ->
     Abs(X/173,
         {
          ("t", type) -> X/173 . "t"[type];
          ("x", value) -> X/173 . "x"[value];
          });
 }
module F3 : S2
|}]

module F4 = (F1 : S1)
[%%expect{|
{
 ("F4", module) ->
     Abs(X/161,
         {
          ("t", type) -> X/161 . "t"[type];
          ("x", value) -> X/161 . "x"[value];
          });
 }
module F4 : S1
|}]
