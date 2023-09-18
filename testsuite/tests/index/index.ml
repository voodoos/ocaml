(* TEST
flags = "-bin-annot -store-usage-index";
compile_only = "true";
readonly_files = "aux.ml";
setup-ocamlc.byte-build-env;
all_modules = "aux.ml index.ml";
ocamlc.byte;
check-ocamlc.byte-output;
program = "-index -decls index.cmt";
output = "out_objinfo";
ocamlobjinfo;
program = "awk '/Indexed/,0' out_objinfo";
output = "out_awk";
run;
check-program-output;
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

let a = (module A : AS)
module _ = (val a)

module F (P : AS) = struct include P end
module G = F (A)
type u = F (A).t;; (* FIXME F and A are missing*)

module type MS = sig
  module type MT
  module M : AS
  module X = A
  type u
end
module type MSA = MS with
  module M = A (* M, MT and u are missing *)
  and module type MT = AS
  and type u = B.t

let () = match 4 with
  | A.(0) | _ -> ()

module type MSB = sig
  type u
  include AS with type t := u
  module G := A
end
