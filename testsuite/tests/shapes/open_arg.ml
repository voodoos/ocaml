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
Uncaught exception: Not_found

|}]
