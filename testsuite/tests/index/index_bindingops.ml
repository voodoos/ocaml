(* TEST
flags = "-bin-annot -store-usage-index"
compile_only = "true"
readonly_files = "index_bindingops.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
all_modules = "index_bindingops.ml"
*** check-ocamlc.byte-output
**** ocamlobjinfo
program = "-index -decls index_bindingops.cmt"
output = "out_objinfo"
***** run
program = "awk '/Indexed/,0' out_objinfo"
output = "out_awk"
****** check-program-output
*)
let (let+) x f = Option.map f x

let (and+) x y =
  Option.bind x @@ fun x ->
  Option.map (fun y -> (x, y)) y

let minus_three =
  let+ foo = None
  and+ bar = None
  and+ man = None in
  foo + bar - man

let _ = (let+)
let _ = (and+)
