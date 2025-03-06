  $ dune exec manytests_inference < manytests_link/typed/001fac.ml
  val fac : int -> int
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/002fac.ml
  val fac_cps : int -> (int -> 'u) -> 'u
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/003fib.ml
  val fib_acc : int -> int -> int -> int
  val fib : int -> int
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/004manyargs.ml
  val wrap : 'h -> 'h
  val test3 : int -> int -> int -> int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/005fix.ml
  val fix : (('j -> 'm) -> 'j -> 'm) -> 'j -> 'm
  val fac : (int -> int) -> int -> int
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/006partial.ml
  val foo : bool -> int -> int
  val foo : int -> int
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/006partial2.ml
  val foo : int -> int -> int -> int
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/006partial3.ml
  val foo : int -> int -> int -> unit
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/007order.ml
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main : unit
  $ dune exec manytests_inference < manytests_link/typed/008ascription.ml
  val addi : ('j -> bool -> int) -> ('j -> bool) -> 'j -> int
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/009let_poly.ml
  val temp : (int * bool)
  $ dune exec manytests_inference < manytests_link/typed/010sukharev.ml
  Fatal error: exception Invalid_argument("List.combine")
  Raised at Stdlib.invalid_arg in file "stdlib.ml", line 30, characters 20-45
  Called from Stdlib__List.combine in file "list.ml", line 325, characters 36-49
  Called from Stdlib__List.combine in file "list.ml", line 325, characters 36-49
  Called from Typing__Subst.unify in file "lib/typing/subst.ml", line 59, characters 7-23
  Called from Typing__Inference.infer_let.(fun) in file "lib/typing/inference.ml", line 216, characters 22-40
  Called from Typing__Monads.(>>=) in file "lib/typing/monads.ml", line 13, characters 16-20
  Called from Typing__Monads.(>>=) in file "lib/typing/monads.ml", line 13, characters 16-20
  Called from Typing__Monads.(>>=) in file "lib/typing/monads.ml", line 13, characters 16-20
  Called from Typing__Monads.(>>=) in file "lib/typing/monads.ml", line 13, characters 16-20
  Called from Typing__Monads.run in file "lib/typing/monads.ml", line 49, characters 51-66
  Called from Typing__Inference.infer_program in file "lib/typing/inference.ml", line 416, characters 8-54
  Called from Test_types__Test_utils.(>>=) in file "test/typing/test_utils.ml" (inlined), line 9, characters 12-15
  Called from Test_types__Test_utils.test in file "test/typing/test_utils.ml", lines 14-15, characters 2-29
  Called from Dune__exe__Manytests_inference in file "test/manytests/manytests_inference.ml", line 9, characters 2-21
  [2]
  $ dune exec manytests_inference < manytests_link/typed/015tuples.ml
  val fix : (('j -> 'm) -> 'j -> 'm) -> 'j -> 'm
  val map : ('q -> 's) -> ('q * 'q) -> ('s * 's)
  val fixpoly : (('aa -> 'dd * 'aa -> 'dd) -> 'aa -> 'dd * ('aa -> 'dd * 'aa -> 'dd) -> 'aa -> 'dd) -> ('aa -> 'dd * 'aa -> 'dd)
  val feven : ('kk * int -> int) -> int -> int
  val fodd : (int -> int * 'uu) -> int -> int
  val tie : (int -> int * int -> int)
  val meven : int -> int
  val modd : int -> int
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/016lists.ml
  val length : 'k list -> int
  val length_tail : 't list -> int
  val map : ('hh -> 'jj) -> 'hh list -> 'jj list
  val append : 'ooo list -> 'ooo list -> 'ooo list
  val concat : 'ooo list list -> 'ooo list
  val iter : ('gggg -> unit) -> 'gggg list -> unit
  val cartesian : 'qqqq list -> 'ssss list -> ('qqqq * 'ssss) list
  val main : int
