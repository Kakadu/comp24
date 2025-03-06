  $ dune exec manytests_inference < manytests_link/typed/001fac.ml
  val fac : int -> int
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/002fac.ml
  val fac_cps : int -> (int -> 'v) -> 'v
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
  $ dune exec manytests_inference < manytests_link/typed/015tuples.ml
  val fix : (('j -> 'm) -> 'j -> 'm) -> 'j -> 'm
  val map : ('q -> 's) -> ('q * 'q) -> ('s * 's)
  val fixpoly : (('aa -> 'dd * 'aa -> 'dd) -> 'aa -> 'dd * ('aa -> 'dd * 'aa -> 'dd) -> 'aa -> 'dd) -> ('aa -> 'dd * 'aa -> 'dd)
  val feven : ('kk * int -> int) -> int -> int
  val fodd : (int -> int * 'vv) -> int -> int
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
