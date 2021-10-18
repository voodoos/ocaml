(* TEST
   flags = "-drawlambda -dlambda"
   * expect
*)

(* Note: the tests below contain *both* the -drawlambda and
   the -dlambda intermediate representations:
   -drawlambda is the Lambda code generated directly by the
     pattern-matching compiler; it contain "alias" bindings or static
     exits that are unused, and will be removed by simplification, or
     that are used only once, and will be inlined by simplification.
   -dlambda is the Lambda code resulting from simplification.

  The -drawlambda output more closely matches what the
  pattern-compiler produces, and the -dlambda output more closely
  matches the final generated code.

  In this test we decided to show both to notice that some allocations
  are "optimized away" during simplification (see "here flattening is
  an optimization" below).
*)

match (3, 2, 1) with
| (_, 3, _)
| (1, _, _) -> true
| _ -> false
;;
[%%expect{|
(let (*match*/89 = 3 *match*/90 = 2 *match*/91 = 1)
  (catch
    (catch
      (catch (if (!= *match*/90 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/89 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
(let (*match*/89 = 3 *match*/90 = 2 *match*/91 = 1)
  (catch (if (!= *match*/90 3) (if (!= *match*/89 1) 0 (exit 1)) (exit 1))
   with (1) 1))
- : bool = false
|}];;

(* This tests needs to allocate the tuple to bind 'x',
   but this is only done in the branches that use it. *)
match (3, 2, 1) with
| ((_, 3, _) as x)
| ((1, _, _) as x) -> ignore x; true
| _ -> false
;;
[%%expect{|
(let (*match*/94 = 3 *match*/95 = 2 *match*/96 = 1)
  (catch
    (catch
      (catch
        (if (!= *match*/95 3) (exit 6)
          (let (x/98 =a (makeblock 0 *match*/94 *match*/95 *match*/96))
            (exit 4 x/98)))
       with (6)
        (if (!= *match*/94 1) (exit 5)
          (let (x/97 =a (makeblock 0 *match*/94 *match*/95 *match*/96))
            (exit 4 x/97))))
     with (5) 0)
   with (4 x/92) (seq (ignore x/92) 1)))
(let (*match*/94 = 3 *match*/95 = 2 *match*/96 = 1)
  (catch
    (if (!= *match*/95 3)
      (if (!= *match*/94 1) 0
        (exit 4 (makeblock 0 *match*/94 *match*/95 *match*/96)))
      (exit 4 (makeblock 0 *match*/94 *match*/95 *match*/96)))
   with (4 x/92) (seq (ignore x/92) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function a/99 b/100 0)
(function a/99 b/100 0)
- : bool -> 'a -> unit = <fun>
|}];;

(* More complete tests.

   The test cases below compare the compiler output on alias patterns
   that are outside an or-pattern (handled during half-simplification,
   then flattened) or inside an or-pattern (handled during simplification).

   We used to have a Cannot_flatten exception that would result in fairly
   different code generated in both cases, but now the compilation strategy
   is fairly similar.
*)
let _ = fun a b -> match a, b with
| (true, _) as p -> p
| (false, _) as p -> p
(* outside, trivial *)
[%%expect {|
(function a/103 b/104 (let (p/105 =a (makeblock 0 a/103 b/104)) p/105))
(function a/103 b/104 (makeblock 0 a/103 b/104))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function a/107 b/108 (let (p/109 =a (makeblock 0 a/107 b/108)) p/109))
(function a/107 b/108 (makeblock 0 a/107 b/108))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function a/113 b/114
  (let (x/115 =a a/113 p/116 =a (makeblock 0 a/113 b/114))
    (makeblock 0 x/115 p/116)))
(function a/113 b/114 (makeblock 0 a/113 (makeblock 0 a/113 b/114)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function a/119 b/120
  (let (x/121 =a a/119 p/122 =a (makeblock 0 a/119 b/120))
    (makeblock 0 x/121 p/122)))
(function a/119 b/120 (makeblock 0 a/119 (makeblock 0 a/119 b/120)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function a/129 b/130
  (if a/129
    (let (x/131 =a a/129 p/132 =a (makeblock 0 a/129 b/130))
      (makeblock 0 x/131 p/132))
    (let (x/133 =a b/130 p/134 =a (makeblock 0 a/129 b/130))
      (makeblock 0 x/133 p/134))))
(function a/129 b/130
  (if a/129 (makeblock 0 a/129 (makeblock 0 a/129 b/130))
    (makeblock 0 b/130 (makeblock 0 a/129 b/130))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function a/135 b/136
  (catch
    (if a/135
      (let (x/143 =a a/135 p/144 =a (makeblock 0 a/135 b/136))
        (exit 10 x/143 p/144))
      (let (x/141 =a b/136 p/142 =a (makeblock 0 a/135 b/136))
        (exit 10 x/141 p/142)))
   with (10 x/137 p/138) (makeblock 0 x/137 p/138)))
(function a/135 b/136
  (catch
    (if a/135 (exit 10 a/135 (makeblock 0 a/135 b/136))
      (exit 10 b/136 (makeblock 0 a/135 b/136)))
   with (10 x/137 p/138) (makeblock 0 x/137 p/138)))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

(* here flattening is an optimisation: the allocation is moved as an
   alias within each branch, and in the first branch it is unused and
   will be removed by simplification, so the final code
   (see the -dlambda output) will not allocate in the first branch. *)
let _ = fun a b -> match a, b with
| (true as x, _) as _p -> x, (true, true)
| (false as x, _) as p -> x, p
(* outside, onecase *)
[%%expect {|
(function a/145 b/146
  (if a/145
    (let (x/147 =a a/145 _p/148 =a (makeblock 0 a/145 b/146))
      (makeblock 0 x/147 [0: 1 1]))
    (let (x/149 =a a/145 p/150 =a (makeblock 0 a/145 b/146))
      (makeblock 0 x/149 p/150))))
(function a/145 b/146
  (if a/145 (makeblock 0 a/145 [0: 1 1])
    (makeblock 0 a/145 (makeblock 0 a/145 b/146))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function a/151 b/152
  (let (x/153 =a a/151 p/154 =a (makeblock 0 a/151 b/152))
    (makeblock 0 x/153 p/154)))
(function a/151 b/152 (makeblock 0 a/151 (makeblock 0 a/151 b/152)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

type 'a tuplist = Nil | Cons of ('a * 'a tuplist)
[%%expect{|
0
0
type 'a tuplist = Nil | Cons of ('a * 'a tuplist)
|}]

(* another example where we avoid an allocation in the first case *)
let _ =fun a b -> match a, b with
| (true, Cons p) -> p
| (_, _) as p -> p
(* outside, tuplist *)
[%%expect {|
(function a/164 b/165
  (catch
    (if a/164 (if b/165 (let (p/166 =a (field 0 b/165)) p/166) (exit 12))
      (exit 12))
   with (12) (let (p/167 =a (makeblock 0 a/164 b/165)) p/167)))
(function a/164 b/165
  (catch (if a/164 (if b/165 (field 0 b/165) (exit 12)) (exit 12)) with (12)
    (makeblock 0 a/164 b/165)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function a/168 b/169
  (catch
    (catch
      (if a/168
        (if b/169 (let (p/173 =a (field 0 b/169)) (exit 13 p/173)) (exit 14))
        (exit 14))
     with (14) (let (p/172 =a (makeblock 0 a/168 b/169)) (exit 13 p/172)))
   with (13 p/170) p/170))
(function a/168 b/169
  (catch
    (catch
      (if a/168 (if b/169 (exit 13 (field 0 b/169)) (exit 14)) (exit 14))
     with (14) (exit 13 (makeblock 0 a/168 b/169)))
   with (13 p/170) p/170))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
