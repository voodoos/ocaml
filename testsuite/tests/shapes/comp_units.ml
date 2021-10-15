(* TEST
   flags = "-dshape"
   * expect
*)

(* Make sure that shapes of compilation units are never eagerly loaded,
   regardless of the context. *)

module Mdirect = Stdlib__Unit
[%%expect{|
{
 ("Mdirect", module) -> CU Stdlib__Unit;
 }
module Mdirect = Unit
|}]

module Mproj = Stdlib.Unit
[%%expect{|
{
 ("Mproj", module) -> CU Stdlib . "Unit"[module];
 }
module Mproj = Unit
|}]

module F (X : sig type t end) = X
[%%expect{|
{
 ("F", module) -> Abs(X/91(.4), X/91(.3));
 }
module F : functor (X : sig type t end) -> sig type t = X.t end
|}]

module App_direct = F (Stdlib__Unit)
[%%expect{|
{
 ("App_direct", module) -> CU Stdlib__Unit;
 }
module App_direct : sig type t = Unit.t end
|}]

module App_proj = F (Stdlib.Unit)
[%%expect{|
{
 ("App_proj", module) -> CU Stdlib . "Unit"[module];
 }
module App_proj : sig type t = Unit.t end
|}]

module App_direct_indir = F (Mdirect)
[%%expect{|
{
 ("App_direct_indir", module) -> CU Stdlib__Unit;
 }
module App_direct_indir : sig type t = Mdirect.t end
|}]

module App_proj_indir = F (Mproj)
[%%expect{|
{
 ("App_proj_indir", module) -> CU Stdlib . "Unit"[module];
 }
module App_proj_indir : sig type t = Mproj.t end
|}]

(* In the following the shape are not loaded, we just know what the signature
   are and build shapes from them. *)

include Stdlib__Unit
[%%expect{|
{
 ("compare", value) -> CU Stdlib__Unit . "compare"[value];
 ("equal", value) -> CU Stdlib__Unit . "equal"[value];
 ("t", type) -> CU Stdlib__Unit . "t"[type];
 ("to_string", value) -> CU Stdlib__Unit . "to_string"[value];
 }
type t = unit = ()
val equal : t -> t -> bool = <fun>
val compare : t -> t -> int = <fun>
val to_string : t -> string = <fun>
|}]

include Stdlib.Unit
[%%expect{|
{
 ("compare", value) -> CU Stdlib . "Unit"[module] . "compare"[value];
 ("equal", value) -> CU Stdlib . "Unit"[module] . "equal"[value];
 ("t", type) -> CU Stdlib . "Unit"[module] . "t"[type];
 ("to_string", value) -> CU Stdlib . "Unit"[module] . "to_string"[value];
 }
type t = unit = ()
val equal : t -> t -> bool = <fun>
val compare : t -> t -> int = <fun>
val to_string : t -> string = <fun>
|}]
