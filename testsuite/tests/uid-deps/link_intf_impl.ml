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

let x = 42

type t = int
