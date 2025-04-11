(* TEST_BELOW
(* Blank lines added here to preserve locations. *)









*)

(* FIXME the following test should not generate warnings *)

module M : sig
  module F (_ : sig val test : int end) : sig end
end = struct
 module F (X: sig val test : int end) = struct let _ = X.test end
end


(* TEST
 flags = "-w +A-70";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
