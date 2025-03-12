  $ dune exec manytests_inference < manytests_link/typed/001fac.ml
  val fac : int -> int
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/002fac.ml
  val fac_cps : int -> (int -> 'bb) -> 'bb
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/003fib.ml
  val fib_acc : int -> int -> int -> int
  val fib : int -> int
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/004manyargs.ml
  val wrap : 'n -> 'n
  val test3 : int -> int -> int -> int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/005fix.ml
  val fix : (('p -> 's) -> 'p -> 's) -> 'p -> 's
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
  val addi : ('p -> bool -> int) -> ('p -> bool) -> 'p -> int
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/009let_poly.ml
  val temp : (int * bool)
  $ dune exec manytests_inference < manytests_link/typed/015tuples.ml
  val fix : (('p -> 's) -> 'p -> 's) -> 'p -> 's
  val map : ('w -> 'y) -> ('w * 'w) -> ('y * 'y)
  val fixpoly : (('gb -> 'jb * 'gb -> 'jb) -> 'gb -> 'jb * ('gb -> 'jb * 'gb -> 'jb) -> 'gb -> 'jb) -> ('gb -> 'jb * 'gb -> 'jb)
  val feven : ('qb * int -> int) -> int -> int
  val fodd : (int -> int * 'cc) -> int -> int
  val tie : (int -> int * int -> int)
  val meven : int -> int
  val modd : int -> int
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/016lists.ml
  val length : 'q list -> int
  val length_tail : 'ab list -> int
  val map : ('nb -> 'pb) -> 'nb list -> 'pb list
  val append : 'uc list -> 'uc list -> 'uc list
  val concat : 'uc list list -> 'uc list
  val iter : ('md -> unit) -> 'md list -> unit
  val cartesian : 'wd list -> 'yd list -> ('wd * 'yd) list
  val main : int
