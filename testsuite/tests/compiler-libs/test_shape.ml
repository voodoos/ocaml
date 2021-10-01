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
     Abs(X/120, {
                 ("t", type) -> X/120 . "t"[type];
                 ("y", type) -> <.10>;
                 });
 }
module M'' : functor (X : S) -> sig type t = X.t type y = X.t end
|}]

module M3 (X : S) = struct
  type y = X.t
  include X
end
[%%expect{|
{
 ("M3", module) ->
     Abs(X/127, {
                 ("t", type) -> X/127 . "t"[type];
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
         Abs(X/135,
             Abs(Y/137,
                 {
                  ("t", type) -> shape-var/134(X/135)(Y/137) . "t"[type];
                  ("u", type) -> shape-var/134(X/135)(Y/137) . "u"[type];
                  })));
 }
module type MFS = functor (X : S) (Y : S) -> sig type t type u end
|}]

module type MFS_indir = functor (X : Set.OrderedType) (Y : S) -> sig
  include module type of X
  type u
end
[%%expect{|
{
 ("MFS_indir", module type) ->
     Abs(shape-var/148,
         Abs(X/149,
             Abs(Y/342,
                 {
                  ("compare", value) ->
                      shape-var/148(X/149)(Y/342) . "compare"[value];
                  ("t", type) -> shape-var/148(X/149)(Y/342) . "t"[type];
                  ("u", type) -> shape-var/148(X/149)(Y/342) . "u"[type];
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
     Abs(X/135, Abs(Y/137, {
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
     Abs(shape-var/459,
         {
          ("t", type) -> shape-var/459 . "t"[type];
          ("x", value) -> shape-var/459 . "x"[value];
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
     Abs(shape-var/467,
         Abs(X/468,
             {
              ("t", type) -> shape-var/467(X/468) . "t"[type];
              ("x", value) -> shape-var/467(X/468) . "x"[value];
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
     Abs(shape-var/478,
         Abs(X/479,
             {
              ("t", type) -> shape-var/478(X/479) . "t"[type];
              ("x", value) -> shape-var/478(X/479) . "x"[value];
              }));
 }
module type S2 = functor (X : S) -> sig type t val x : t end
|}]

module type S3 = functor (X : S) -> S
[%%expect{|
{
 ("S3", module type) ->
     Abs(shape-var/489,
         Abs(X/490,
             {
              ("t", type) -> shape-var/489(X/490) . "t"[type];
              ("x", value) -> shape-var/489(X/490) . "x"[value];
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
     Abs(X/496,
         {
          ("t", type) -> X/496 . "t"[type];
          ("x", value) -> X/496 . "x"[value];
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
     Abs(X/479,
         {
          ("t", type) -> X/479 . "t"[type];
          ("x", value) -> X/479 . "x"[value];
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
     Abs(X/468,
         {
          ("t", type) -> X/468 . "t"[type];
          ("x", value) -> X/468 . "x"[value];
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
     Abs(shape-var/531,
         {
          ("Sig", module type) -> shape-var/531 . "Sig"[module type];
          ("Sig_alias", module type) ->
              shape-var/531 . "Sig_alias"[module type];
          ("t", type) -> shape-var/531 . "t"[type];
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

(* N.B.: the shape of With_alias.A could be the one of Coercion.
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
     Abs(shape-var/562, {
                         ("A", module) -> shape-var/562 . "A"[module];
                         });
 }
module type With_alias = sig module A = Coercion end
|}]

module Fignore(_ : S) = struct let x = 3 end
[%%expect{|
{
 ("Fignore", module) -> Abs(shape-var/568, {
                                            ("x", value) -> <.59>;
                                            });
 }
module Fignore : S -> sig val x : int end
|}]

module Fgen() = struct let x = 3 end
[%%expect{|
{
 ("Fgen", module) -> {
                      ("x", value) -> <.61>;
                      };
 }
module Fgen : functor () -> sig val x : int end
|}]

module M = Fignore(struct type t = int let x = 3 end)
[%%expect{|
{
 ("M", module) -> {
                   ("x", value) -> <.59>;
                   };
 }
module M : sig val x : int end
|}]

module N = Fgen()
[%%expect{|
{
 ("N", module) -> {
                   ("x", value) -> <.61>;
                   };
 }
module N : sig val x : int end
|}]

class c = object
  val v = 3
end
[%%expect{|
{
 ("c", class) -> <.67>;
 }
class c : object val v : int end
|}]

class type ct = object
  val v : int
end
[%%expect{|
{
 ("ct", class type) -> <.71>;
 }
class type ct = object val v : int end
|}]

module type SC = sig
  class c : object
    val v : int
  end
  class type ct = object
    val v : int
  end
end

[%%expect{|
{
 ("SC", module type) ->
     Abs(shape-var/629, {
                         ("c", class) -> <.72>;
                         ("ct", class type) -> <.73>;
                         });
 }
module type SC =
  sig
    class c : object val v : int end
    class type ct = object val v : int end
  end
|}]

module type Fignore = functor (_ : S) -> sig
  val x : int
end
[%%expect{|
{
 ("Fignore", module type) ->
     Abs(shape-var/648,
         Abs(shape-var/649,
             {
              ("x", value) -> shape-var/648(shape-var/649) . "x"[value];
              }));
 }
module type Fignore = S -> sig val x : int end
|}]

module F : Fignore = functor (_ : S) -> struct
  let x = 3
end
[%%expect{|
{
 ("F", module) -> Abs(shape-var/649, {
                                      ("x", value) -> <.77>;
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
                   ("t", type) -> CU  . "A"[module] . "t"[type];
                   };
 ("B", module) -> {
                   ("t", type) -> CU  . "B"[module] . "t"[type];
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
   : Set.S with type elt = A.t
   = Set.Make(A)

[%%expect{|
{
 ("A", module) ->
     {
      ("compare", value) -> CU  . "A"[module] . "compare"[value];
      ("t", type) -> CU  . "A"[module] . "t"[type];
      };
 ("ASet", module) ->
     CU Stdlib . "Set"[module] . "S"[module type](
     CU Stdlib . "Set"[module] . "S"[module type](CU  . "ASet"[module]));
 }
module rec A :
  sig
    type t = Leaf of string | Node of ASet.t
    val compare : t -> t -> int
  end
and ASet :
  sig
    type elt = A.t
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val to_rev_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
  end
|}]

module type Rec1 = sig
  module rec A : sig
    type t = Leaf of B.t
  end

  and B : sig type t = int end
end
[%%expect{|
{
 ("Rec1", module type) ->
     Abs(shape-var/1050,
         {
          ("A", module) -> {
                            ("t", type) -> CU  . "A"[module] . "t"[type];
                            };
          ("B", module) -> {
                            ("t", type) -> CU  . "B"[module] . "t"[type];
                            };
          });
 }
module type Rec1 =
  sig
    module rec A : sig type t = Leaf of B.t end
    and B : sig type t = int end
  end
|}]

module type Rec2 = sig
  module rec A : sig
    type t = Leaf of string | Node of ASet.t
    val compare: t -> t -> int
  end

  and ASet : Set.S with type elt = A.t
end
[%%expect{|
{
 ("Rec2", module type) ->
     Abs(shape-var/1070,
         {
          ("A", module) ->
              {
               ("compare", value) -> CU  . "A"[module] . "compare"[value];
               ("t", type) -> CU  . "A"[module] . "t"[type];
               };
          ("ASet", module) ->
              CU Stdlib . "Set"[module] . "S"[module type](
              CU Stdlib . "Set"[module] . "S"[module type](
              CU  . "ASet"[module]));
          });
 }
module type Rec2 =
  sig
    module rec A :
      sig
        type t = Leaf of string | Node of ASet.t
        val compare : t -> t -> int
      end
    and ASet :
      sig
        type elt = A.t
        type t
        val empty : t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val compare : t -> t -> int
        val equal : t -> t -> bool
        val subset : t -> t -> bool
        val iter : (elt -> unit) -> t -> unit
        val map : (elt -> elt) -> t -> t
        val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val split : elt -> t -> t * bool * t
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
      end
  end
|}]

module type Std = module type of Unit
[%%expect{|
{
 ("Std", module type) -> Abs(shape-var/1223, CU Stdlib . "Unit"[module]);
 }
module type Std =
  sig
    type t = unit = ()
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val to_string : t -> string
  end
|}]

module type Std = module type of Stdlib__Unit

[%%expect{|
{
 ("Std", module type) ->
     Abs(shape-var/1231,
         {
          ("compare", value) -> shape-var/1231 . "compare"[value];
          ("equal", value) -> shape-var/1231 . "equal"[value];
          ("t", type) -> shape-var/1231 . "t"[type];
          ("to_string", value) -> shape-var/1231 . "to_string"[value];
          });
 }
module type Std =
  sig
    type t = unit = ()
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val to_string : t -> string
  end
|}]


module type MX = module type of M3

[%%expect{|
Uncaught exception: Failure("TODO @ulysse shapeofmdtype functor")

|}]

module type Small = sig
  type t
end
[%%expect{|
{
 ("Small", module type) ->
     Abs(shape-var/1240, {
                          ("t", type) -> shape-var/1240 . "t"[type];
                          });
 }
module type Small = sig type t end
|}]

module type Big = sig
  type t
  val x : t
end
[%%expect{|
{
 ("Big", module type) ->
     Abs(shape-var/1246,
         {
          ("t", type) -> shape-var/1246 . "t"[type];
          ("x", value) -> shape-var/1246 . "x"[value];
          });
 }
module type Big = sig type t val x : t end
|}]

module type BigToSmall = functor (X : Big) -> Small with type t = X.t
[%%expect{|
{
 ("BigToSmall", module type) ->
     Abs(shape-var/1254,
         Abs(X/1255, {
                      ("t", type) -> shape-var/1254(X/1255) . "t"[type];
                      }));
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
     Abs(X/1263, {
                  ("t", type) -> <.145>;
                  ("x", value) -> <.146>;
                  });
 }
module SmallToBig : functor (X : Small) -> sig type t = X.t val x : t end
|}]

module F = (SmallToBig : BigToSmall)
[%%expect{|
{
 ("F", module) -> Abs(X/1255, {
                               ("t", type) -> <.145>;
                               });
 }
module F : BigToSmall
|}]
