  $ ./inferencer_runner.exe < manytests/typed/001fac.ml
  val fac : int -> int
  val main : int

  $ ./inferencer_runner.exe < manytests/typed/002fac.ml
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int

  $ ./inferencer_runner.exe < manytests/typed/003fib.ml
  val fib_acc : int -> int -> int -> int
  val fib : int -> int
  val main : int

  $ ./inferencer_runner.exe < manytests/typed/004manyargs.ml
  val wrap : 'a -> 'a
  val test3 : int -> int -> int -> int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val main : int

  $ ./inferencer_runner.exe < manytests/typed/005fix.ml
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fac : (int -> int) -> int -> int
  val main : int

  $ ./inferencer_runner.exe < manytests/typed/006partial.ml
  val foo : int -> int
  val main : int

  $ ./inferencer_runner.exe < manytests/typed/006partial2.ml
  val foo : int -> int -> int -> int
  val main : int

  $ ./inferencer_runner.exe < manytests/typed/006partial3.ml
  val foo : int -> int -> int -> unit
  val main : int

  $ ./inferencer_runner.exe < manytests/typed/007order.ml
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main : unit

  $ ./inferencer_runner.exe < manytests/typed/008ascription.ml
  val addi : ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val main : int

  $ ./inferencer_runner.exe < manytests/typed/009let_poly.ml
  val temp : int * bool

  $ ./inferencer_runner.exe < manytests/typed/015tuples.ml
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val map : ('a -> 'b) -> 'a * 'a -> 'b * 'b
  val fixpoly : (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) * (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) -> ('a -> 'b) * ('a -> 'b)
  val feven : 'a * (int -> int) -> int -> int
  val fodd : (int -> int) * 'a -> int -> int
  val tie : (int -> int) * (int -> int)
  val meven : int -> int
  val modd : int -> int
  val main : int

  $ ./inferencer_runner.exe < manytests/typed/016lists.ml
  val length : 'a list -> int
  val length_tail : 'a list -> int
  val map : ('a -> 'b) -> 'a list -> 'b list
  val append : 'a list -> 'a list -> 'a list
  val concat : 'a list list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val cartesian : 'a list -> 'b list -> 'a * 'b list
  val main : int
