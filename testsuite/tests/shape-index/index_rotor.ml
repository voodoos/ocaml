(* TEST

flags = "-bin-annot -bin-annot-occurrences";
compile_only = "true";
readonly_files = "index_rotor.ml";
setup-ocamlc.byte-build-env;
all_modules = "index_rotor.ml";
ocamlc.byte;
check-ocamlc.byte-output;

program = "-quiet -index -decls index_rotor.cmt";
output = "out_objinfo";
ocamlobjinfo;

check-program-output;
*)

module type Stringable = sig
  type t
  val to_string : t -> string
end
module type Toto = sig
  type t
  val to_string : t -> string
end

module Toto (X : Toto) = struct
  type t =  X.t
  let to_string = X.to_string
end

module Pair (X : Stringable) (Y : Stringable) = struct
  type t = X.t * Y.t
  let to_string (x, y) =
    X.to_string x ^ " " ^ Y.to_string y
end

module Int = struct
  type t = int
  let to_string i = string_of_int i
end

module String = struct
  type t = string
  let to_string s = s
end

module P = Pair(Int)(Int)

let _ = P.to_string (0, 1)

module P' = Pair(Int)(Pair(String)(Int))

let _ = P'.to_string (0, ("!=", 1))

module T = Toto(Pair(Int)(Int))

let _ = T.to_string (0, 1)
