  $ dune exec manytests_inference < manytests_link/typed/001fac.ml
  val fac : int -> int
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/002fac.ml
  val fac_cps : int -> (int -> 'eb) -> 'eb
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/003fib.ml
  val fib_acc : int -> int -> int -> int
  val fib : int -> int
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/004manyargs.ml
  val wrap : 'p -> 'p
  val test3 : int -> int -> int -> int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/005fix.ml
  val fix : (('r -> 'u) -> 'r -> 'u) -> 'r -> 'u
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
  val addi : ('r -> bool -> int) -> ('r -> bool) -> 'r -> int
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/009let_poly.ml
  val temp : (int * bool)
  $ dune exec manytests_inference < manytests_link/typed/015tuples.ml
  val fix : (('r -> 'u) -> 'r -> 'u) -> 'r -> 'u
  val map : ('y -> 'bb) -> ('y * 'y) -> ('bb * 'bb)
  val fixpoly : (('ib -> 'lb * 'ib -> 'lb) -> 'ib -> 'lb * ('ib -> 'lb * 'ib -> 'lb) -> 'ib -> 'lb) -> ('ib -> 'lb * 'ib -> 'lb)
  val feven : ('sb * int -> int) -> int -> int
  val fodd : (int -> int * 'ec) -> int -> int
  val tie : (int -> int * int -> int)
  val meven : int -> int
  val modd : int -> int
  val main : int
  $ dune exec manytests_inference < manytests_link/typed/016lists.ml
  val length : 's list -> int
  val length_tail : 'cb list -> int
  val map : ('pb -> 'rb) -> 'pb list -> 'rb list
  val append : 'wc list -> 'wc list -> 'wc list
  val concat : 'wc list list -> 'wc list
  val iter : ('od -> unit) -> 'od list -> unit
  val cartesian : 'yd list -> 'be list -> ('yd * 'be) list
  val main : int
