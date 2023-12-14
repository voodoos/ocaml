(* TEST
flags = "-bin-annot -bin-annot-occurrences"
compile_only = "true"
readonly_files = "index_objects.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
all_modules = "index_objects.ml"
*** check-ocamlc.byte-output
**** ocamlobjinfo
program = "-quiet -index -decls index_objects.cmt"
output = "out_objinfo"
***** check-program-output



*)

let o = object
  method pop () = ()
end

(* FIXME: the Typedtree lacks required information to index method usages *)
let () = o#pop ()

class c = object
  method cpop () = ()
end

let _ : c = new c

class d = object
  inherit c
end

module type M = sig
  class ct : object
      method pop : unit
    end

  class dt : object inherit ct end
end

class ins_var = object (self)
  val mutable ins = 0
  method get_ins () = ins
  method set_ins i = ins <- i
  method other () = self#get_ins ()
end
