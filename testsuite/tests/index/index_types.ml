(* TEST
flags = "-bin-annot -store-usage-index"
compile_only = "true"
readonly_files = "index_types.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
all_modules = "index_types.ml"
*** check-ocamlc.byte-output
**** ocamlobjinfo
program = "-index -decls index_types.cmt"
output = "out_objinfo"
***** run
program = "awk '/Indexed/,0' out_objinfo"
output = "out_awk"
****** check-program-output
*)

type t = int

let x : t = 42 (* FIXME? duplicate Tpat_extra_constraint / Texp_constraint *)

module M = struct end

let () = match 4 with
  | (_ : t) -> ()

type poly = [`A|`B]

let () = match `A with #poly -> ()

module type S = sig
  type t2 = ..
  type t2 += B
end

type t1 = ..
type t1 += B
