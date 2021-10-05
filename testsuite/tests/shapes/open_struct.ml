(* TEST
   flags = "-dshape"
   * expect
*)

(* Everything that couldn't go anywhere else. *)

open struct
  module M = struct
    type t = A
  end
end
[%%expect{|
{
 }
module M : sig type t = A end
|}]

(* FIXME *)
include M
[%%expect{|
{
 }
type t = M.t = A
|}]

(* FIXME *)
module N = M
[%%expect{|
{
 ("N", module) -> {
                   };
 }
module N = M
|}]
