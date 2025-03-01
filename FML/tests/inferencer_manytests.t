  $ ./inferencer_runner.exe < manytests/typed/001fac.ml
  TODO: Not implemented.

  $ ./inferencer_runner.exe < manytests/typed/002fac.ml
  TODO: Not implemented.

  $ ./inferencer_runner.exe < manytests/typed/003fib.ml
  TODO: Not implemented.

  $ ./inferencer_runner.exe < manytests/typed/004manyargs.ml
  The type variable 'a occurs inside 'a

  $ ./inferencer_runner.exe < manytests/typed/005fix.ml
  TODO: Not implemented.

  $ ./inferencer_runner.exe < manytests/typed/006partial.ml
  val main : int
  val foo : int -> int
  val foo : int -> int

  $ ./inferencer_runner.exe < manytests/typed/006partial2.ml
  val main : int
  val foo : int -> int -> int -> int
  $ ./inferencer_runner.exe < manytests/typed/006partial3.ml
  val main : int
  val foo : int -> int -> int -> unit

  $ ./inferencer_runner.exe < manytests/typed/007order.ml
  Error: : end_of_input

  $ ./inferencer_runner.exe < manytests/typed/008ascription.ml
  val main : int
  val addi : ('a -> bool -> int) -> ('a -> bool) -> 'a -> int

  $ ./inferencer_runner.exe < manytests/typed/009let_poly.ml
  val temp : int * bool

  $ ./inferencer_runner.exe < manytests/typed/010sukharev.ml
  Error: : end_of_input

  $ ./inferencer_runner.exe < manytests/typed/015tuples.ml
  TODO: Not implemented.

  $ ./inferencer_runner.exe < manytests/typed/016lists.ml
  TODO: Not implemented.
