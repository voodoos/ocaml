(* TEST

flags = "-bin-annot";
compile_only = "true";
setup-ocamlc.byte-build-env;
readonly_files = "intf_uids.mli intf_uids.ml";
all_modules = "intf_uids.mli intf_uids.ml";
ocamlc.byte;
check-ocamlc.byte-output;

program = "-quiet -decls intf_uids.cmti intf_uids.cmt";
output = "out_objinfo";
ocamlobjinfo;

check-program-output;
*)

(* This test illustrates the fact that uids are tagged to indicate if they
  originate from an interface of an implementation. *)

type u (* has uid Intf_uids.0 *)

type t (* has uid Intf_uids.1 *)
