(* TEST
flags = "-bin-annot -bin-annot-occurrences"
compile_only = "true"
readonly_files = "index_constrs_decl_def.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
all_modules = "index_constrs_decl_def.ml"
*** check-ocamlc.byte-output
**** ocamlobjinfo
program = "-index -decls index_constrs_decl_def.cmt"
output = "out_objinfo"
***** run
program = "awk '/Indexed/,0' out_objinfo"
output = "out_awk"
****** check-program-output
*)

module M : sig
  type t = A of { la : int }
end = struct
  type t = A of { la : int }
  let _ = A { la = 42 }
end

let _ = M.A { la = 42 }

open M

let _ = A { la = 42 }
