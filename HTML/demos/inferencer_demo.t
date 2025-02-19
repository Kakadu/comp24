  $ ./inferencer_demo.exe < manytests/do_not_type/001.ml
  Typecheck error: Unbound value fac

  $ ./inferencer_demo.exe < manytests/do_not_type/002if.ml
  Typecheck error: This expression has type int but an expression was expected of type bool

  $ ./inferencer_demo.exe < manytests/do_not_type/003occurs.ml
  Typecheck error: Occurs check failed

  $ ./inferencer_demo.exe < manytests/do_not_type/004let_poly.ml
  Typecheck error: This expression has type bool but an expression was expected of type int

  $ ./inferencer_demo.exe < manytests/do_not_type/015tuples.ml
  Typecheck error: Only variables are allowed as left-hand side of `let rec'

  $ ./inferencer_demo.exe < manytests/typed/001fac.ml
  val fac : int -> int
  val main : int

  $ ./inferencer_demo.exe < manytests/typed/002fac.ml
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int

  $ ./inferencer_demo.exe < manytests/typed/003fib.ml
  val fib : int -> int
  val fib_acc : int -> int -> int -> int
  val main : int

  $ ./inferencer_demo.exe < manytests/typed/004manyargs.ml
  val main : int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val test3 : int -> int -> int -> int
  val wrap : 'a -> 'a

  $ ./inferencer_demo.exe < manytests/typed/005fix.ml
  val fac : (int -> int) -> int -> int
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val main : int

  $ ./inferencer_demo.exe < manytests/typed/006partial.ml
  val foo : int -> int
  val main : int

  $ ./inferencer_demo.exe < manytests/typed/006partial2.ml
  val foo : int -> int -> int -> int
  val main : int

  $ ./inferencer_demo.exe < manytests/typed/006partial3.ml
  val foo : int -> int -> int -> Unit
  val main : int

  $ ./inferencer_demo.exe < manytests/typed/007order.ml
  val _start : Unit -> Unit -> int -> Unit -> int -> int -> Unit -> int -> int -> int
  val main : Unit

  $ ./inferencer_demo.exe < manytests/typed/008ascription.ml
  val addi : ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val main : int

  $ ./inferencer_demo.exe < manytests/typed/009let_poly.ml
  val temp : int * bool

  $ ./inferencer_demo.exe < manytests/typed/015tuples.ml
  val feven : 'a * int -> int -> int -> int
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fixpoly : 'a -> 'b * 'a -> 'b -> 'a -> 'b * 'a -> 'b * 'a -> 'b -> 'a -> 'b -> 'a -> 'b * 'a -> 'b
  val fodd : int -> int * 'a -> int -> int
  val main : int
  val map : ('a -> 'b) -> 'a * 'a -> 'b * 'b
  val meven : int -> int
  val modd : int -> int
  val tie : int -> int * int -> int

  $ ./inferencer_demo.exe < manytests/typed/016lists.ml
  val append : 'a list -> 'a list -> 'a list
  val cartesian : 'a list -> 'b list -> ('a * 'b) list
  val concat : 'a list list -> 'a list
  val iter : ('a -> Unit) -> 'a list -> Unit
  val length : 'a list -> int
  val length_tail : 'a list -> int
  val main : int
  val map : ('a -> 'b) -> 'a list -> 'b list

