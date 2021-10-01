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
  val x : int
end
[%%expect{|
{
 ("Sx", module type) -> <.3>;
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
     Abs(X/114, {
                 ("t", type) -> X/114 . "t"[type];
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
     Abs(X/122, {
                 ("t", type) -> X/122 . "t"[type];
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
 ("MFS", module type) -> <.18>;
 }
module type MFS = functor (X : S) (Y : S) -> sig type t type u end
|}]

module type MFS_indir = functor (X : Set.OrderedType) (Y : S) -> sig
  include module type of X
  type u
end
[%%expect{|
{
 ("MFS_indir", module type) -> <.22>;
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
     Abs(X/442, Abs(Y/444, {
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
 ("S", module type) -> <.30>;
 }
module type S = sig type t val x : t end
|}]

module type S1 = functor (X : S) -> sig
  include module type of X
end
[%%expect{|
{
 ("S1", module type) -> <.32>;
 }
module type S1 = functor (X : S) -> sig type t val x : t end
|}]

module type S2 = functor (X : S) -> sig
  include S
end
[%%expect{|
{
 ("S2", module type) -> <.34>;
 }
module type S2 = functor (X : S) -> sig type t val x : t end
|}]

module type S3 = functor (X : S) -> S
[%%expect{|
{
 ("S3", module type) -> <.36>;
 }
module type S3 = functor (X : S) -> S
|}]

module F1 (X : S) = struct
  include X
end
[%%expect{|
{
 ("F1", module) ->
     Abs(X/481,
         {
          ("t", type) -> X/481 . "t"[type];
          ("x", value) -> X/481 . "x"[value];
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
     Abs(X/481,
         {
          ("t", type) -> X/481 . "t"[type];
          ("x", value) -> X/481 . "x"[value];
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
     Abs(X/481,
         {
          ("t", type) -> X/481 . "t"[type];
          ("x", value) -> X/481 . "x"[value];
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
 ("Foo", module type) -> <.47>;
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
 ("With_alias", module type) -> <.58>;
 }
module type With_alias = sig module A = Coercion end
|}]

module Fignore(_ : S) = struct let x = 3 end
[%%expect{|
{
 ("Fignore", module) -> Abs(shape-var/542, {
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
 ("SC", module type) -> <.74>;
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
 ("Fignore", module type) -> <.76>;
 }
module type Fignore = S -> sig val x : int end
|}]

module F : Fignore = functor (_ : S) -> struct
  let x = 3
end
[%%expect{|
{
 ("F", module) -> Abs(shape-var/626, {
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
                   ("t", type) -> <.87>;
                   };
 ("B", module) -> {
                   ("t", type) -> <.89>;
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
 ("A", module) -> {
                   ("compare", value) -> <.105>;
                   ("t", type) -> <.102>;
                   };
 ("ASet", module) ->
     {
      ("add", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "add"[value];
      ("add_seq", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "add_seq"[value];
      ("cardinal", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "cardinal"[value];
      ("choose", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "choose"[value];
      ("choose_opt", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      })
          . "choose_opt"[value];
      ("compare", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "compare"[value];
      ("diff", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "diff"[value];
      ("disjoint", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "disjoint"[value];
      ("elements", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "elements"[value];
      ("elt", type) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "elt"[type];
      ("empty", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "empty"[value];
      ("equal", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "equal"[value];
      ("exists", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "exists"[value];
      ("filter", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "filter"[value];
      ("filter_map", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      })
          . "filter_map"[value];
      ("find", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "find"[value];
      ("find_first", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      })
          . "find_first"[value];
      ("find_first_opt", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      })
          . "find_first_opt"[value];
      ("find_last", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "find_last"[value];
      ("find_last_opt", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      })
          . "find_last_opt"[value];
      ("find_opt", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "find_opt"[value];
      ("fold", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "fold"[value];
      ("for_all", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "for_all"[value];
      ("inter", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "inter"[value];
      ("is_empty", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "is_empty"[value];
      ("iter", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "iter"[value];
      ("map", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "map"[value];
      ("max_elt", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "max_elt"[value];
      ("max_elt_opt", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      })
          . "max_elt_opt"[value];
      ("mem", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "mem"[value];
      ("min_elt", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "min_elt"[value];
      ("min_elt_opt", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      })
          . "min_elt_opt"[value];
      ("of_list", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "of_list"[value];
      ("of_seq", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "of_seq"[value];
      ("partition", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "partition"[value];
      ("remove", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "remove"[value];
      ("singleton", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "singleton"[value];
      ("split", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "split"[value];
      ("subset", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "subset"[value];
      ("t", type) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "t"[type];
      ("to_rev_seq", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      })
          . "to_rev_seq"[value];
      ("to_seq", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "to_seq"[value];
      ("to_seq_from", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      })
          . "to_seq_from"[value];
      ("union", value) ->
          CU Stdlib . "Set"[module] . "Make"[module]({
                                                      }) . "union"[value];
      };
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
 ("Rec1", module type) -> <.120>;
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
 ("Rec2", module type) -> <.133>;
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
 ("Std", module type) -> <.134>;
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
 ("Std", module type) -> <.135>;
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
{
 ("MX", module type) -> <.136>;
 }
module type MX = functor (X : S/2) -> sig type y = X.t type t = X.t end
|}]

module type Small = sig
  type t
end
[%%expect{|
{
 ("Small", module type) -> <.138>;
 }
module type Small = sig type t end
|}]

module type Big = sig
  type t
  val x : t
end
[%%expect{|
{
 ("Big", module type) -> <.141>;
 }
module type Big = sig type t val x : t end
|}]

module type BigToSmall = functor (X : Big) -> Small with type t = X.t
[%%expect{|
{
 ("BigToSmall", module type) -> <.144>;
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
     Abs(X/1220, {
                  ("t", type) -> <.146>;
                  ("x", value) -> <.147>;
                  });
 }
module SmallToBig : functor (X : Small) -> sig type t = X.t val x : t end
|}]

module F = (SmallToBig : BigToSmall)
[%%expect{|
{
 ("F", module) -> Abs(shape-var/1304, {
                                       ("t", type) -> <.146>;
                                       });
 }
module F : BigToSmall
|}]
