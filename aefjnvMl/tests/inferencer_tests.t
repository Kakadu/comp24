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
  var !=: 'a -> 'a -> bool
  var &&: bool -> bool -> bool
  var *: int -> int -> int
  var +: int -> int -> int
  var -: int -> int -> int
  var /: int -> int -> int
  var <: 'a -> 'a -> bool
  var <=: 'a -> 'a -> bool
  var =: 'a -> 'a -> bool
  var >: 'a -> 'a -> bool
  var >=: 'a -> 'a -> bool
  var fac: int -> int
  var main: int
  var print_int: int -> unit
  var ||: bool -> bool -> bool

  $ ./inferencer_runner.exe < manytests/typed/002fac.ml
  var !=: 'a -> 'a -> bool
  var &&: bool -> bool -> bool
  var *: int -> int -> int
  var +: int -> int -> int
  var -: int -> int -> int
  var /: int -> int -> int
  var <: 'a -> 'a -> bool
  var <=: 'a -> 'a -> bool
  var =: 'a -> 'a -> bool
  var >: 'a -> 'a -> bool
  var >=: 'a -> 'a -> bool
  var fac_cps: int -> (int -> 'a) -> 'a
  var main: int
  var print_int: int -> unit
  var ||: bool -> bool -> bool

  $ ./inferencer_runner.exe < manytests/typed/003fib.ml
  var !=: 'a -> 'a -> bool
  var &&: bool -> bool -> bool
  var *: int -> int -> int
  var +: int -> int -> int
  var -: int -> int -> int
  var /: int -> int -> int
  var <: 'a -> 'a -> bool
  var <=: 'a -> 'a -> bool
  var =: 'a -> 'a -> bool
  var >: 'a -> 'a -> bool
  var >=: 'a -> 'a -> bool
  var fib: int -> int
  var fib_acc: int -> int -> int -> int
  var main: int
  var print_int: int -> unit
  var ||: bool -> bool -> bool

  $ ./inferencer_runner.exe < manytests/typed/004manyargs.ml
  var !=: 'a -> 'a -> bool
  var &&: bool -> bool -> bool
  var *: int -> int -> int
  var +: int -> int -> int
  var -: int -> int -> int
  var /: int -> int -> int
  var <: 'a -> 'a -> bool
  var <=: 'a -> 'a -> bool
  var =: 'a -> 'a -> bool
  var >: 'a -> 'a -> bool
  var >=: 'a -> 'a -> bool
  var main: int
  var print_int: int -> unit
  var test10: int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  var test3: int -> int -> int -> int
  var wrap: 'a -> 'a
  var ||: bool -> bool -> bool

  $ ./inferencer_runner.exe < manytests/typed/005fix.ml
  var !=: 'a -> 'a -> bool
  var &&: bool -> bool -> bool
  var *: int -> int -> int
  var +: int -> int -> int
  var -: int -> int -> int
  var /: int -> int -> int
  var <: 'a -> 'a -> bool
  var <=: 'a -> 'a -> bool
  var =: 'a -> 'a -> bool
  var >: 'a -> 'a -> bool
  var >=: 'a -> 'a -> bool
  var fac: (int -> int) -> int -> int
  var fix: (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  var main: int
  var print_int: int -> unit
  var ||: bool -> bool -> bool

  $ ./inferencer_runner.exe < manytests/typed/006partial.ml
  var !=: 'a -> 'a -> bool
  var &&: bool -> bool -> bool
  var *: int -> int -> int
  var +: int -> int -> int
  var -: int -> int -> int
  var /: int -> int -> int
  var <: 'a -> 'a -> bool
  var <=: 'a -> 'a -> bool
  var =: 'a -> 'a -> bool
  var >: 'a -> 'a -> bool
  var >=: 'a -> 'a -> bool
  var foo: int -> int
  var main: int
  var print_int: int -> unit
  var ||: bool -> bool -> bool

  $ ./inferencer_runner.exe < manytests/typed/006partial2.ml
  var !=: 'a -> 'a -> bool
  var &&: bool -> bool -> bool
  var *: int -> int -> int
  var +: int -> int -> int
  var -: int -> int -> int
  var /: int -> int -> int
  var <: 'a -> 'a -> bool
  var <=: 'a -> 'a -> bool
  var =: 'a -> 'a -> bool
  var >: 'a -> 'a -> bool
  var >=: 'a -> 'a -> bool
  var foo: int -> int -> int -> int
  var main: int
  var print_int: int -> unit
  var ||: bool -> bool -> bool

  $ ./inferencer_runner.exe < manytests/typed/006partial3.ml
  var !=: 'a -> 'a -> bool
  var &&: bool -> bool -> bool
  var *: int -> int -> int
  var +: int -> int -> int
  var -: int -> int -> int
  var /: int -> int -> int
  var <: 'a -> 'a -> bool
  var <=: 'a -> 'a -> bool
  var =: 'a -> 'a -> bool
  var >: 'a -> 'a -> bool
  var >=: 'a -> 'a -> bool
  var foo: int -> int -> int -> unit
  var main: int
  var print_int: int -> unit
  var ||: bool -> bool -> bool

  $ ./inferencer_runner.exe < manytests/typed/007order.ml
  This expression has type int -> int but an expression was expected of type int

