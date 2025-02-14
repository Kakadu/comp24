  $ ./infer_tests.exe < manytests/typed/001fac.ml
  val fac : int -> int
  val main : int

  $ ./infer_tests.exe < manytests/typed/002fac.ml
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int

  $ ./infer_tests.exe < manytests/typed/003fib.ml
  val fib : int -> int
  val fib_acc : int -> int -> int -> int
  val main : int

  $ ./infer_tests.exe < manytests/typed/004manyargs.ml
  val main : int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val test3 : int -> int -> int -> int
  val wrap : 'a -> 'a

  $ ./infer_tests.exe < manytests/typed/005fix.ml
  val fac : (int -> int) -> int -> int
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val main : int

  $ ./infer_tests.exe < manytests/typed/006partial.ml
  val foo : int -> int
  val main : int

  $ ./infer_tests.exe < manytests/typed/006partial2.ml
  val foo : int -> int -> int -> int
  val main : int

  $ ./infer_tests.exe < manytests/typed/006partial3.ml
  val foo : int -> int -> int -> unit
  val main : int

  $ ./infer_tests.exe < manytests/typed/007order.ml
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main : unit
  $ ./infer_tests.exe < manytests/typed/008ascription.ml
  val addi : ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val main : int

  $ ./infer_tests.exe < manytests/typed/009let_poly.ml
  val temp : int * bool

  $ ./infer_tests.exe < manytests/typed/015tuples.ml
  val feven : 'a * (int -> int) -> int -> int
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fixpoly : (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) * (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) -> ('a -> 'b) * ('a -> 'b)
  val fodd : (int -> int) * 'a -> int -> int
  val main : int
  val map : ('a -> 'b) -> 'a * 'a -> 'b * 'b
  val meven : int -> int
  val modd : int -> int
  val tie : (int -> int) * (int -> int)

  $ ./infer_tests.exe < manytests/typed/016lists.ml
  val append : 'a list -> 'a list -> 'a list
  val cartesian : 'a list -> 'b list -> ('a * 'b) list
  val concat : 'a list list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val length : 'a list -> int
  val length_tail : 'a list -> int
  val main : int
  val map : ('a -> 'b) -> 'a list -> 'b list

  $ ./infer_tests.exe < manytests/do_not_type/001.ml
  Infer: Unbound value "fac"

  $ ./infer_tests.exe < manytests/do_not_type/002if.ml
  Infer: Unification failed: int and bool

  $ ./infer_tests.exe < manytests/do_not_type/003occurs.ml
  Infer: Occurs check error

  $ ./infer_tests.exe < manytests/do_not_type/004let_poly.ml
  Infer: Unification failed: bool and int

  $ ./infer_tests.exe < manytests/do_not_type/015tuples.ml
  Infer: Only variables are allowed as left-hand side of `let rec'
