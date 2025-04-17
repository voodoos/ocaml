(* TEST

flags = "-bin-annot -bin-annot-occurrences";
compile_only = "true";
setup-ocamlc.byte-build-env;
all_modules = "index_funct_params.ml";
ocamlc.byte;
check-ocamlc.byte-output;

program = "-quiet -index -decls -uid-deps index_funct_params.cmt";
output = "out_objinfo";
ocamlobjinfo;

check-program-output;
*)

module type S = sig
  val x : unit
end

module M : S = struct let x = () end

module F (X : S) = struct
  include X
  let x = x
end

include F (struct let x = () end)
let () = M.x
