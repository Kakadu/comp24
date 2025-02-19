  $ dune exec TypingRunner < manytests/typed/001fac.ml
  val fac : int -> int
  val main : int

  $ dune exec TypingRunner < manytests/typed/002fac.ml
  val fac_cps : int -> int -> '13 -> '13
  val main : int

  $ dune exec TypingRunner < manytests/typed/003fib.ml
  val fib : int -> int
  val fib_acc : int -> int -> int -> int
  val main : int

  $ dune exec TypingRunner < manytests/typed/004manyargs.ml
  val main : int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val test3 : int -> int -> int -> int
  val wrap : '0 -> '0

  $ dune exec TypingRunner < manytests/typed/005fix.ml
  val fac : int -> int -> int -> int
  val fix : '2 -> '5 -> '2 -> '5 -> '2 -> '5
  val main : int

  $ dune exec TypingRunner < manytests/typed/006partial.ml
  val foo : int -> int
  val main : int

  $ dune exec TypingRunner < manytests/typed/006partial2.ml
  val foo : int -> int -> int -> int
  val main : int

  $ dune exec TypingRunner < manytests/typed/006partial3.ml
  val foo : int -> int -> int -> unit
  val main : int

  $ dune exec TypingRunner < manytests/typed/007order.ml
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main : unit

  $ dune exec TypingRunner < manytests/typed/008ascription.ml
  val addi : '2 -> bool -> int -> '2 -> bool -> '2 -> int
  val main : int

  $ dune exec TypingRunner < manytests/typed/009let_poly.ml
  val temp : (int * bool)

  $ dune exec TypingRunner < manytests/typed/015tuples.ml
  val feven : ('36 * '37) -> int -> int
  val fix : '2 -> '5 -> '2 -> '5 -> '2 -> '5
  val fixpoly : ('20 * '21) -> ('25 -> '28 * '25 -> '28)
  val fodd : ('47 * '48) -> int -> int
  val main : int
  val map : '10 -> '11 -> ('8 * '9) -> ('11 * '11)
  val meven : int -> int
  val modd : int -> int
  val tie : ('58 -> '59 * '58 -> '59)

  $ dune exec TypingRunner < manytests/typed/016lists.ml
  val append : '60 list -> '60 list -> '60 list
  val cartesian : '93 list -> '97 list -> ('93 * '97) list
  val concat : '75 list list -> '75 list
  val iter : '82 -> unit -> '82 list -> unit
  val length : '4 list -> int
  val length_tail : '20 list -> int
  val main : int
  val map : '47 -> '31 -> '47 list -> '31 list
