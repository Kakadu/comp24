MANYTESTS
  $ ./start_type_inference_demos.exe < manytests/do_not_type/001.ml
  Type error: unbound variable 'fac'

  $ ./start_type_inference_demos.exe < manytests/do_not_type/002if.ml
  Type error: unification failed - type int does not match expected type bool

  $ ./start_type_inference_demos.exe < manytests/do_not_type/003occurs.ml
  Type error: occurs check failed.

  $ ./start_type_inference_demos.exe < manytests/do_not_type/004let_poly.ml
  Type error: unification failed - type int does not match expected type bool

  $ ./start_type_inference_demos.exe < manytests/do_not_type/015tuples.ml
  Only variables are allowed as left-hand side of `let rec'

  $ ./start_type_inference_demos.exe < manytests/typed/001fac.ml
  val fac : int -> int
  val main : int

  $ ./start_type_inference_demos.exe < manytests/typed/002fac.ml
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int

  $ ./start_type_inference_demos.exe < manytests/typed/003fib.ml
  val fib_acc : int -> int -> int -> int
  val fib : int -> int
  val main : int

  $ ./start_type_inference_demos.exe < manytests/typed/004manyargs.ml
  val wrap : 'a -> 'a
  val test3 : int -> int -> int -> int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val main : int

  $ ./start_type_inference_demos.exe < manytests/typed/005fix.ml
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fac : (int -> int) -> int -> int
  val main : int

  $ ./start_type_inference_demos.exe < manytests/typed/006partial.ml
  val foo : int -> int
  val main : int

  $ ./start_type_inference_demos.exe < manytests/typed/006partial2.ml
  val foo : int -> int -> int -> int
  val main : int

  $ ./start_type_inference_demos.exe < manytests/typed/006partial3.ml
  val foo : int -> int -> int -> unit
  val main : int

  $ ./start_type_inference_demos.exe < manytests/typed/007order.ml
  Syntax error.

  $ ./start_type_inference_demos.exe < manytests/typed/008ascription.ml
  val addi : ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val main : int

  $ ./start_type_inference_demos.exe < manytests/typed/009let_poly.ml
  val temp : int * bool

  $ ./start_type_inference_demos.exe < manytests/typed/015tuples.ml
  Type error: unification failed - type 'a * 'b does not match expected type 'a

  $ ./start_type_inference_demos.exe < manytests/typed/016lists.ml
  Type error: unification failed - type unit does not match expected type 'a
