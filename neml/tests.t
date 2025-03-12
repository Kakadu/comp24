  $ run () { ./bin/neml.exe $1 neml.ll && clang-16 -Wno-override-module neml.ll ./lib/back/runtime.c -lffi && ./a.out; }

  $ run manytests/typed/001fac.ml
  (): unit
  false: bool
  true: bool
  fac: int -> int
  main: int
  24

  $ run manytests/typed/002fac.ml 
  (): unit
  false: bool
  true: bool
  fac_cps: int -> (int -> 'a) -> 'a
  main: int
  24

  $ run manytests/typed/003fib.ml 
  (): unit
  false: bool
  true: bool
  fib_acc: int -> int -> int -> int
  fib: int -> int
  main: int
  3
  3

  $ run manytests/typed/004manyargs.ml 
  (): unit
  false: bool
  true: bool
  wrap: 'a -> 'a
  test3: int -> int -> int -> int
  test10: int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  main: int
  1111111111
  1
  10
  100


  $ run manytests/typed/005fix.ml 
  (): unit
  false: bool
  true: bool
  fix: (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  fac: (int -> int) -> int -> int
  main: int
  720


  $ run manytests/typed/006partial3.ml 
  (): unit
  false: bool
  true: bool
  foo: int -> int -> int -> unit
  main: int
  4
  8
  9


  $ run manytests/typed/007order.ml 
  (): unit
  false: bool
  true: bool
  (UnificationFail ((Arr ((Con ((I "int"), [])), (Var (V "gen11")))),
     (Con ((I "unit"), []))))
  [255]


  $ run manytests/typed/008ascription.ml 
  (): unit
  false: bool
  true: bool
  addi: ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  main: int
  8

  $ run manytests/typed/010sukharev.ml 
  (): unit
  false: bool
  true: bool
  _1: int -> int -> int * 'a -> bool
  (UnboundVariable (I "Some"))
  [255]


  $ run manytests/typed/011mapcps.ml 
  (): unit
  false: bool
  true: bool
  (UnboundVariable (I "::"))
  [255]


  $ run manytests/typed/012fibcps.ml 
  (): unit
  false: bool
  true: bool
  fib: int -> (int -> 'a) -> 'a
  main: unit
  8


  $ run manytests/typed/013foldfoldr.ml 
  (): unit
  false: bool
  true: bool
  id: 'a -> 'a
  (UnboundVariable (I "::"))
  [255]


  $ run manytests/typed/016lists.ml 
  (): unit
  false: bool
  true: bool
  (UnboundVariable (I "::"))
  [255]
