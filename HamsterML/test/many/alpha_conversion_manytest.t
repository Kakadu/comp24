  $ dune exec AlphaConversionRunner < manytests/typed/001fac.ml
  val var_0 : int -> int
  val var_2 : int

  $ dune exec AlphaConversionRunner < manytests/typed/002fac.ml
  val var_0 : int -> (int -> 'a) -> 'a
  val var_5 : int

  $ dune exec AlphaConversionRunner < manytests/typed/003fib.ml
  val var_0 : int -> int -> int -> int
  val var_6 : int -> int
  val var_8 : int

  $ dune exec AlphaConversionRunner < manytests/typed/004manyargs.ml
  val var_1 : 'a -> 'a
  val var_19 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val var_22 : int
  val var_8 : int -> int -> int -> int

  $ dune exec AlphaConversionRunner < manytests/typed/005fix.ml
  val var_0 : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val var_5 : (int -> int) -> int -> int
  val var_6 : int

  $ dune exec AlphaConversionRunner < manytests/typed/006partial.ml
  val var_3 : bool -> int -> int
  val var_5 : int -> int
  val var_6 : int

  $ dune exec AlphaConversionRunner < manytests/typed/006partial2.ml
  val var_3 : int -> int -> int -> int
  val var_7 : int

  $ dune exec AlphaConversionRunner < manytests/typed/006partial3.ml
  val var_3 : int -> int -> int -> unit
  val var_4 : int

  $ dune exec AlphaConversionRunner < manytests/typed/007order.ml
  val var_5 : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val var_6 : unit

  $ dune exec AlphaConversionRunner < manytests/typed/008ascription.ml
  val var_3 : ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val var_7 : int

  $ dune exec AlphaConversionRunner < manytests/typed/009let_poly.ml
  val var_2 : (int * bool)

  $ dune exec AlphaConversionRunner < manytests/typed/015tuples.ml
  val var_0 : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val var_13 : (('a -> 'b * 'a -> 'b) -> 'a -> 'b * ('a -> 'b * 'a -> 'b) -> 'a -> 'b) -> ('a -> 'b * 'a -> 'b)
  val var_18 : ('a * int -> int) -> int -> int
  val var_23 : (int -> int * 'a) -> int -> int
  val var_24 : (int -> int * int -> int)
  val var_25 : int -> int
  val var_26 : int -> int
  val var_31 : int
  val var_7 : ('a -> 'b) -> ('a * 'a) -> ('b * 'b)

  $ dune exec AlphaConversionRunner < manytests/typed/016lists.ml
  val var_0 : 'a list -> int
  val var_10 : ('a -> 'b) -> 'a list -> 'b list
  val var_24 : 'a list -> 'a list -> 'a list
  val var_32 : 'a list list -> 'a list
  val var_33 : ('a -> unit) -> 'a list -> unit
  val var_38 : 'a list -> 'b list -> ('a * 'b) list
  val var_44 : int
  val var_9 : 'a list -> int
