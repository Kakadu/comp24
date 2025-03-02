  $ dune exec many_inferencer < manytests/typed/001fac.ml
  fac: int -> int
  main: int

  $ dune exec many_inferencer < manytests/typed/002fac.ml
  fac_cps: int -> (int -> 'o) -> 'o
  main: int

$ dune exec many_inferencer < manytests/typed/003fib.ml

  $ dune exec many_inferencer < manytests/typed/004manyargs.ml
  wrap: 'a -> 'a
  test3: int -> int -> int -> int
  test10: int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  main: int

  $ dune exec many_inferencer < manytests/typed/005fix.ml
  fix: (('c -> 'f) -> 'c -> 'f) -> 'c -> 'f
  fac: (int -> int) -> int -> int
  main: int

  $ dune exec many_inferencer < manytests/typed/006partial.ml
  foo: bool -> int -> int
  foo: int -> int
  main: int

  $ dune exec many_inferencer < manytests/typed/006partial2.ml
  foo: int -> int -> int -> int
  main: int

  $ dune exec many_inferencer < manytests/typed/006partial3.ml
  foo: int -> int -> int -> ()
  main: int

  $ dune exec many_inferencer < manytests/typed/007order.ml
  _start: () -> () -> int -> () -> int -> int -> () -> int -> int -> int
  main: ()

$ dune exec many_inferencer < manytests/typed/008ascription.ml

$ dune exec many_inferencer < manytests/typed/015tuples.ml

  $ dune exec many_inferencer < manytests/typed/016lists.ml
  length: 'd list -> int
  length_tail: 'u list -> int
  map: ('ab -> 'ac) -> 'ab list -> 'ac list
  append: 'bt list -> 'bt list -> 'bt list
  concat: 'cm list list -> 'cm list
  iter: ('cs -> ()) -> 'cs list -> ()
  cartesian: 'dc list -> 'dj list -> ('dc * 'dj) list
  main: int
