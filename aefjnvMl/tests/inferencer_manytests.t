  $ ./inferencer_runner.exe < manytests/do_not_type/001.ml
  Unbound value 'fac'

  $ ./inferencer_runner.exe < manytests/do_not_type/002if.ml
  This expression has type bool but an expression was expected of type int

  $ ./inferencer_runner.exe < manytests/do_not_type/003occurs.ml
  The type variable 'a occurs inside 'a -> 'b

  $ ./inferencer_runner.exe < manytests/do_not_type/004let_poly.ml
  This expression has type int but an expression was expected of type bool

  $ ./inferencer_runner.exe < manytests/do_not_type/015tuples.ml
  Only variables are allowed as left-side of 'let rec'

  $ ./inferencer_runner.exe < manytests/typed/001fac.ml
  val fac: int -> int
  val main: int

  $ ./inferencer_runner.exe < manytests/typed/002fac.ml
  val fac_cps: int -> (int -> 'a) -> 'a
  val main: int

  $ ./inferencer_runner.exe < manytests/typed/003fib.ml
  val fib: int -> int
  val fib_acc: int -> int -> int -> int
  val main: int

  $ ./inferencer_runner.exe < manytests/typed/004manyargs.ml
  val main: int
  val test10: int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val test3: int -> int -> int -> int
  val wrap: 'a -> 'a

  $ ./inferencer_runner.exe < manytests/typed/005fix.ml
  val fac: (int -> int) -> int -> int
  val fix: (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val main: int

  $ ./inferencer_runner.exe < manytests/typed/006partial.ml
  val foo: int -> int
  val main: int

  $ ./inferencer_runner.exe < manytests/typed/006partial2.ml
  val foo: int -> int -> int -> int
  val main: int

  $ ./inferencer_runner.exe < manytests/typed/006partial3.ml
  val foo: int -> int -> int -> unit
  val main: int

  $ ./inferencer_runner.exe < manytests/typed/007order.ml
  val _start: unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main: unit

  $ ./inferencer_runner.exe < manytests/typed/008ascription.ml
  val addi: ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val main: int

  $ ./inferencer_runner.exe < manytests/typed/009let_poly.ml
  val temp: int * bool

  $ ./inferencer_runner.exe < manytests/typed/015tuples.ml
  val feven: 'a * (int -> int) -> int -> int
  val fix: (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fixpoly: ((('a -> 'b) * ('a -> 'b)) -> ('a -> 'b)) * ((('a -> 'b) * ('a -> 'b)) -> ('a -> 'b)) -> ('a -> 'b) * ('a -> 'b)
  val fodd: (int -> int) * 'a -> int -> int
  val main: int
  val map: ('a -> 'b) -> 'a * 'a -> 'b * 'b
  val meven: int -> int
  val modd: int -> int
  val tie: (int -> int) * (int -> int)

  $ ./inferencer_runner.exe < manytests/typed/016lists.ml
  val append: 'a list -> 'a list -> 'a list
  val cartesian: 'a list -> 'b list -> 'a * 'b list
  val concat: 'a list list -> 'a list
  val iter: ('a -> unit) -> 'a list -> unit
  val length: 'a list -> int
  val length_tail: 'a list -> int
  val main: int
  val map: ('a -> 'b) -> 'a list -> 'b list

