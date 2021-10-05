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
 ("N", module) -> {.3
                   };
 }
module N = M
|}]

(* Not open structs, but the code handling the following is currently very
   similar to the one for open struct (i.e. calls [Env.enter_signature]), and
   so we are likely to encounter the same bugs, if any. *)

include struct
  module M' = struct
    type t = A
  end
end
[%%expect{|
{
 ("M'", module) -> {.6
                    ("t", type) -> <.4>;
                    };
 }
module M' : sig type t = A end
|}]

(* FIXME *)
module N' = M'
[%%expect{|
{
 ("N'", module) -> {.7
                    };
 }
module N' = M'
|}]

module Test = struct
  module M = struct
    type t = A
  end
end
[%%expect{|
{
 ("Test", module) -> {.11
                      ("M", module) -> {.10
                                        ("t", type) -> <.8>;
                                        };
                      };
 }
module Test : sig module M : sig type t = A end end
|}]

include Test
[%%expect{|
{
 ("M", module) -> {.10
                   ("t", type) -> <.8>;
                   };
 }
module M = Test.M
|}]

(* FIXME *)
module N = M
[%%expect{|
{
 ("N", module) -> {.12
                   };
 }
module N = M
|}]
