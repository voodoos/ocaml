(* TEST
   flags = "-dshape"
   * expect
*)

module Foo : sig
  module Bar : sig
  end
end = struct
  module Bar = struct
  end
end
;;
[%%expect{|
{
 ("Foo", module) -> {.2
                     ("Bar", module) -> {
                                         };
                     };
 }
module Foo : sig module Bar : sig end end
|}]

(* FIXME *)
module type Extended = sig
  include module type of struct include Foo end
  module Bar : sig
    include module type of struct include Bar end
  end
end
;;
[%%expect{|
{
 ("Extended", module type) -> <.4>;
 }
module type Extended = sig module Bar : sig end end
|}]

module E : Extended = struct
  module Bar = struct end
end

[%%expect{|
{
 ("E", module) -> {.6
                   ("Bar", module) -> {
                                       };
                   };
 }
module E : Extended
|}]
