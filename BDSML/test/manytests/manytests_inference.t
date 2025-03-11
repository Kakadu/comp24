  $ dune exec manytests_inference < manytests_link/typed/001fac.ml
  val fac : int -> int
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/002fac.ml
  val fac_cps : int -> (int -> 'fb) -> 'fb
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/003fib.ml
  val fib_acc : int -> int -> int -> int
  val fib : int -> int
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/004manyargs.ml
  val wrap : 'q -> 'q
  val test3 : int -> int -> int -> int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/005fix.ml
  val fix : (('s -> 'v) -> 's -> 'v) -> 's -> 'v
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
  val addi : ('s -> bool -> int) -> ('s -> bool) -> 's -> int
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/009let_poly.ml
  val temp : (int * bool)
  $ dune exec manytests_inference < manytests_link/typed/015tuples.ml
  val fix : (('s -> 'v) -> 's -> 'v) -> 's -> 'v
  val map : ('ab -> 'cb) -> ('ab * 'ab) -> ('cb * 'cb)
  val fixpoly : (('jb -> 'mb * 'jb -> 'mb) -> 'jb -> 'mb * ('jb -> 'mb * 'jb -> 'mb) -> 'jb -> 'mb) -> ('jb -> 'mb * 'jb -> 'mb)
  val feven : ('tb * int -> int) -> int -> int
  val fodd : (int -> int * 'fc) -> int -> int
  val tie : (int -> int * int -> int)
  val meven : int -> int
  val modd : int -> int
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/016lists.ml
  val length : 't list -> int
  val length_tail : 'db list -> int
  val map : ('qb -> 'sb) -> 'qb list -> 'sb list
  val append : 'xc list -> 'xc list -> 'xc list
  val concat : 'xc list list -> 'xc list
  val iter : ('pd -> unit) -> 'pd list -> unit
  val cartesian : 'ae list -> 'ce list -> ('ae * 'ce) list
  val main : int
