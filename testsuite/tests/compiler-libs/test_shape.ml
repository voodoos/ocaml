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
     Abs(shape-var/88, {
                        ("t", type) -> shape-var/88 . "t"[type];
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
     Abs(shape-var/94,
         {
          ("t", type) -> shape-var/94 . "t"[type];
          ("x", value) -> shape-var/94 . "x"[value];
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

(* FIXME: shape before the include is dropped. *)
module M3 (X : S) = struct
  type y = X.t
  include X
end
[%%expect{|
{
 ("M3", module) ->
     Abs(X/128, {
                 ("t", type) -> X/128 . "t"[type];
                 ("y", type) -> <.13>;
                 });
 }
module M3 : functor (X : S) -> sig type y = X.t type t = X.t end
|}]

module type MFS = functor (X : S) (Y : S) -> sig
  include module type of X
  type u
end
[%%expect{|
{
 ("MFS", module type) ->
     Abs(shape-var/134,
         Abs(X/136,
             Abs(Y/138,
                 {
                  ("t", type) -> shape-var/134(X/136)(Y/138) . "t"[type];
                  ("u", type) -> shape-var/134(X/136)(Y/138) . "u"[type];
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
     Abs(shape-var/149,
         Abs(X/342,
             Abs(Y/344,
                 {
                  ("compare", value) ->
                      shape-var/149(X/342)(Y/344) . "compare"[value];
                  ("t", type) -> shape-var/149(X/342)(Y/344) . "t"[type];
                  ("u", type) -> shape-var/149(X/342)(Y/344) . "u"[type];
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
     Abs(X/136, Abs(Y/138, {
                            ("t", type) -> <.25>;
                            ("u", type) -> <.26>;
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
     Abs(shape-var/461,
         {
          ("t", type) -> shape-var/461 . "t"[type];
          ("x", value) -> shape-var/461 . "x"[value];
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
     Abs(shape-var/469,
         Abs(X/471,
             {
              ("t", type) -> shape-var/469(X/471) . "t"[type];
              ("x", value) -> shape-var/469(X/471) . "x"[value];
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
     Abs(shape-var/481,
         Abs(X/483,
             {
              ("t", type) -> shape-var/481(X/483) . "t"[type];
              ("x", value) -> shape-var/481(X/483) . "x"[value];
              }));
 }
module type S2 = functor (X : S) -> sig type t val x : t end
|}]

module type S3 = functor (X : S) -> S
[%%expect{|
{
 ("S3", module type) ->
     Abs(shape-var/492,
         Abs(X/494,
             {
              ("t", type) -> shape-var/492(X/494) . "t"[type];
              ("x", value) -> shape-var/492(X/494) . "x"[value];
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
     Abs(X/500,
         {
          ("t", type) -> X/500 . "t"[type];
          ("x", value) -> X/500 . "x"[value];
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
                     ("t", type) -> <.39>;
                     ("x", value) -> <.40>;
                     };
 }
module Arg : sig type t = int val x : int end
|}]

include F1 (Arg)
[%%expect{|
{
 ("t", type) -> <.39>;
 ("x", value) -> <.40>;
 }
type t = Arg.t
val x : t = 0
|}]

module F3 = (F1 : S2)
[%%expect{|
{
 ("F3", module) ->
     Abs(X/483,
         {
          ("t", type) -> X/483 . "t"[type];
          ("x", value) -> X/483 . "x"[value];
          });
 }
module F3 : S2
|}]

include F3 (Arg)
[%%expect{|
{
 ("t", type) -> <.39>;
 ("x", value) -> <.40>;
 }
type t = F3(Arg).t
val x : t = <abstr>
|}]

module F4 = (F1 : S1)
[%%expect{|
{
 ("F4", module) ->
     Abs(X/471,
         {
          ("t", type) -> X/471 . "t"[type];
          ("x", value) -> X/471 . "x"[value];
          });
 }
module F4 : S1
|}]

include F4 (Arg)
[%%expect{|
{
 ("t", type) -> <.39>;
 ("x", value) -> <.40>;
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
     Abs(shape-var/534,
         {
          ("Sig", module type) -> shape-var/534 . "Sig"[module type];
          ("Sig_alias", module type) ->
              shape-var/534 . "Sig_alias"[module type];
          ("t", type) -> shape-var/534 . "t"[type];
          });
 }
module type Foo =
  sig module type Sig = sig type t end module type Sig_alias = Sig type t end
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
      ("E", extension constructor) -> <.51>;
      ("M", module) -> {
                        };
      ("t", type) -> <.49>;
      ("v", value) -> <.48>;
      };
 }
module Coercion : sig val v : int type t module M : sig end exception E end
|}]

(* FIXME: the shape of With_alias.A should probablay be the one of Coercion.
   I guess it's functionally equivalent with the one here, since any use of
   With_alias to coerce a module M is going to require M.A to be equal to (and
   share the shape of) Coercion.
   Still, it's not minimal. *)
module type With_alias = sig
  module A = Coercion
end
[%%expect{|
{
 ("With_alias", module type) ->
     Abs(shape-var/565, {
                         ("A", module) -> shape-var/565 . "A"[module];
                         });
 }
module type With_alias = sig module A = Coercion end
|}]
