(* TEST
flags = "-bin-annot"
compile_only = "true"
readonly_files = "aux.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
all_modules = "aux.ml index.ml"
*** check-ocamlc.byte-output
**** ocamlobjinfo
program = "-index index.cmt"
***** check-program-output
*)

module type AS = sig
  type t
  val x : t
end

module A = struct
  type t = int
  let (x : t) = 42
end

module B = A

module C : sig
  open A
  val c : t
end = struct
  include A
  let c = 42
end

open A

let y = A.x + Aux.z

let () = print_int y

let a = (module A : AS) (* FIXME: AS is missing*)
module _ = (val a)
