(* TEST
   flags = "-dshape"
   * expect
*)

(* FIXME *)
module type Make = functor (I : sig end) -> sig
  open I
end
;;

[%%expect{|
{("Make", module type) -> .1: ·;
 }
module type Make = functor (I : sig end) -> sig end
|}]

module Make (I : sig end) : sig
  open I
end = struct end
;;

[%%expect{|
{("Make", module) -> .3: Abs(I/94, {});
 }
module Make : functor (I : sig end) -> sig end
|}]

module type Make = functor (I : sig end) ->
module type of struct
  open I
end

[%%expect{|
{("Make", module type) -> .5: ·;
 }
module type Make = functor (I : sig end) -> sig end
|}]
