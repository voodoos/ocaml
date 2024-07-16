(* TEST

flags = "-bin-annot -bin-annot-occurrences";
compile_only = "true";
setup-ocamlc.byte-build-env;
all_modules = "link_intf_impl.mli link_intf_impl.ml";
ocamlc.byte;
check-ocamlc.byte-output;

program = "-quiet -uid-deps link_intf_impl.cmt";
output = "out_objinfo";
ocamlobjinfo;

check-program-output;
*)

let x (* 0 *) = 42

type t (* 1 *) = int

module type S (* 3 *) = sig
  val y (* 2 *) : t
end

module M (* 5 *) : S = struct
  let y (* 4 *) = 36
end

module N (* 8 *) : sig
  val y (* 7 *) : int
end = struct
  let y (* 6 *) = 2
end

let _ = (module N : S)

module P (* 10 *)= struct
  let y (* 9 *) = 12
end

module F (* 12 *) (X (* 11 *) : S) = X

module G (* 13 *) = F(P)

module type Initial (* 16 *) = sig
  module type Nested (* 15 *) = sig
    type t (* 14 *)
  end
end
