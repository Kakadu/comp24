
  $ ./inferencer_demo.exe << EOF
  > let rec fib n =
  > if n<2
  > then n
  > else fib (n - 1) + fib (n - 2) 
  val fib: int -> int

  $ ./inferencer_demo.exe < manytests/typed/001fac.ml
  val fac: int -> int
  val main: int
  $ ./inferencer_demo.exe < manytests/typed/002fac.ml
  val fac_cps: int -> (int -> 'a) -> 'a
  val main: int
  $ ./inferencer_demo.exe < manytests/typed/003fib.ml
  val fib: int -> int
  val fib_acc: int -> int -> int -> int
  val main: int
  $ ./inferencer_demo.exe < manytests/typed/004manyargs.ml
  val main: int
  val test10: int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val test3: int -> int -> int -> int
  val wrap: 'a -> 'a
  $ ./inferencer_demo.exe < manytests/typed/005fix.ml
  val fac: (int -> int) -> int -> int
  val fix: (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val main: int
  $ ./inferencer_demo.exe < manytests/typed/006partial.ml
  val foo: int -> int
  val main: int
  $ ./inferencer_demo.exe < manytests/typed/006partial2.ml
  val foo: int -> int -> int -> int
  val main: int
  $ ./inferencer_demo.exe < manytests/typed/006partial3.ml
  val foo: int -> int -> int -> unit
  val main: int
  $ ./inferencer_demo.exe < manytests/typed/007order.ml
  val _start: unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main: unit
  $ ./inferencer_demo.exe < manytests/typed/008ascription.ml
  val addi: ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val main: int
  $ ./inferencer_demo.exe < manytests/typed/009let_poly.ml
  val temp: int * bool
  $ ./inferencer_demo.exe < manytests/typed/015tuples.ml
  val feven: 'a * (int -> int) -> int -> int
  val fix: (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fixpoly: (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) * (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) -> ('a -> 'b) * ('a -> 'b)
  val fodd: (int -> int) * 'a -> int -> int
  val main: int
  val map: ('a -> 'b) -> 'a * 'a -> 'b * 'b
  val meven: int -> int
  val modd: int -> int
  val tie: (int -> int) * (int -> int)
