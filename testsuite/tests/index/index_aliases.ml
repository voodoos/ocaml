(* TEST
flags = "-bin-annot -store-usage-index"
compile_only = "true"
readonly_files = "index_aliases.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
all_modules = "index_aliases.ml"
*** check-ocamlc.byte-output
**** ocamlobjinfo
program = "-quiet -index -decls index_aliases.cmt"
output = "out_objinfo"
***** check-program-output



*)

module A = struct type t end
module B = A

module F (X : sig type t end) = X
module F' = F
module C = F'(A)

module C' = F(B)
module D = C

module G = B
include G
