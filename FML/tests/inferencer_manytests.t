  $ ./inferencer_runner.exe < manytests/typed/001fac.ml
  val fac : int -> int
  val main : int

  $ ./inferencer_runner.exe < manytests/typed/002fac.ml
  val fac_cps : int -> (int -> int) -> int
  val main : int

  $ ./inferencer_runner.exe < manytests/typed/003fib.ml
  Type error: unification failed - type int does not match expected type int -> int

  $ ./inferencer_runner.exe < manytests/typed/004manyargs.ml
  Type error: the type variable 'a occurs inside 'a

  $ ./inferencer_runner.exe < manytests/typed/005fix.ml
  val fix : ((int -> int) -> int -> int) -> int -> int
  val fac : (int -> int) -> int -> int
  val main : int

  $ ./inferencer_runner.exe < manytests/typed/006partial.ml
  val foo : int -> int
  val foo : int -> int
  val main : int

  $ ./inferencer_runner.exe < manytests/typed/006partial2.ml
  val foo : int -> int -> int -> int
  val main : int
  $ ./inferencer_runner.exe < manytests/typed/006partial3.ml
  val foo : int -> int -> int -> unit
  val main : int

  $ ./inferencer_runner.exe < manytests/typed/008ascription.ml
  val addi : ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val main : int

  $ ./inferencer_runner.exe < manytests/typed/009let_poly.ml
  val temp : int * bool

  $ ./inferencer_runner.exe < manytests/typed/015tuples.ml
  val fix : ((((int -> int) * (int -> int) -> int -> int) * ((int -> int) * (int -> int) -> int -> int) -> (int -> int) * (int -> int)) -> ((int -> int) * (int -> int) -> int -> int) * ((int -> int) * (int -> int) -> int -> int) -> (int -> int) * (int -> int)) -> ((int -> int) * (int -> int) -> int -> int) * ((int -> int) * (int -> int) -> int -> int) -> (int -> int) * (int -> int)
  val map : ('a -> 'b) -> 'a * 'a -> 'b * 'b
  val fixpoly : ((int -> int) * (int -> int) -> int -> int) * ((int -> int) * (int -> int) -> int -> int) -> (int -> int) * (int -> int)
  val feven : 'a * (int -> int) -> int -> int
  val fodd : (int -> int) * 'a -> int -> int
  val tie : (int -> int) * (int -> int)
  val meven : int -> int
  val modd : int -> int
  val main : int

  $ ./inferencer_runner.exe < manytests/typed/016lists.ml
  val length : int * int list -> int
  val length_tail : 'a list -> int
  val map : ('a -> 'b) -> 'a list -> 'b list
  val append : 'a list -> 'a list -> 'a list
  val concat : 'a list list -> 'a list
  val iter : (int -> unit) -> int list -> unit
  val cartesian : int list -> int list -> int * int list
  val main : int
