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
  var fac: int -> int
  var main: int

  $ ./inferencer_runner.exe < manytests/typed/002fac.ml
  var fac_cps: int -> (int -> 'a) -> 'a
  var main: int

  $ ./inferencer_runner.exe < manytests/typed/003fib.ml
  var fib: int -> int
  var fib_acc: int -> int -> int -> int
  var main: int

  $ ./inferencer_runner.exe < manytests/typed/004manyargs.ml
  var main: int
  var test10: int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  var test3: int -> int -> int -> int
  var wrap: 'a -> 'a

  $ ./inferencer_runner.exe < manytests/typed/005fix.ml
  var fac: (int -> int) -> int -> int
  var fix: (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  var main: int

  $ ./inferencer_runner.exe < manytests/typed/006partial.ml
  var foo: int -> int
  var main: int

  $ ./inferencer_runner.exe < manytests/typed/006partial2.ml
  var foo: int -> int -> int -> int
  var main: int

  $ ./inferencer_runner.exe < manytests/typed/006partial3.ml
  var foo: int -> int -> int -> unit
  var main: int

  $ ./inferencer_runner.exe < manytests/typed/007order.ml
  var _start: unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  var main: unit

  $ ./inferencer_runner.exe < manytests/typed/008ascription.ml
  var addi: ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  var main: int

  $ ./inferencer_runner.exe < manytests/typed/009let_poly.ml
  var temp: int * bool

  $ ./inferencer_runner.exe < manytests/typed/015tuples.ml
  var feven: (int -> int) * 'a -> int -> int
  var fix: (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  var fixpoly: ((('a -> 'b) * ('a -> 'b)) -> ('a -> 'b)) * ((('a -> 'b) * ('a -> 'b)) -> ('a -> 'b)) -> ('a -> 'b) * ('a -> 'b)
  var fodd: 'a * (int -> int) -> int -> int
  var main: int
  var map: ('a -> 'b) -> 'a * 'a -> 'b * 'b
  var meven: int -> int
  var modd: int -> int
  var tie: (int -> int) * (int -> int)

  $ ./inferencer_runner.exe < manytests/typed/016lists.ml
  var append: 'a list -> 'a list -> 'a list
  var cartesian: 'a list -> 'b list -> 'a * 'b list
  var concat: 'a list list -> 'a list
  var iter: ('a -> unit) -> 'a list -> unit
  var length: 'a list -> int
  var length_tail: 'a list -> int
  var main: int
  var map: ('a -> 'b) -> 'a list -> 'b list

