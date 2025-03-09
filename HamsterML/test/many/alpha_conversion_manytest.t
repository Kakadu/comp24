  $ dune exec AlphaConversionRunner < manytests/typed/001fac.ml
  val var_0 : int -> int
  val var_2 : int

  $ dune exec AlphaConversionRunner < manytests/typed/002fac.ml
  val var_0 : int -> (int -> '13) -> '13
  val var_5 : int

  $ dune exec AlphaConversionRunner < manytests/typed/003fib.ml
  val var_0 : int -> int -> int -> int
  val var_6 : int -> int
  val var_8 : int

  $ dune exec AlphaConversionRunner < manytests/typed/004manyargs.ml
  val var_1 : '0 -> '0
  val var_19 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val var_22 : int
  val var_8 : int -> int -> int -> int

  $ dune exec AlphaConversionRunner < manytests/typed/005fix.ml
  val var_0 : (('2 -> '5) -> '2 -> '5) -> '2 -> '5
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
  val var_3 : ('2 -> bool -> int) -> ('2 -> bool) -> '2 -> int
  val var_7 : int

  $ dune exec AlphaConversionRunner < manytests/typed/009let_poly.ml
  val var_2 : (int * bool)

  $ dune exec AlphaConversionRunner < manytests/typed/015tuples.ml
  val var_0 : (('2 -> '5) -> '2 -> '5) -> '2 -> '5
  val var_13 : (('21 -> '24 * '21 -> '24) -> '21 -> '24 * ('21 -> '24 * '21 -> '24) -> '21 -> '24) -> ('21 -> '24 * '21 -> '24)
  val var_18 : ('32 * int -> int) -> int -> int
  val var_23 : (int -> int * '43) -> int -> int
  val var_24 : (int -> int * int -> int)
  val var_25 : int -> int
  val var_26 : int -> int
  val var_31 : int
  val var_7 : ('8 -> '10) -> ('8 * '8) -> ('10 * '10)

  $ dune exec AlphaConversionRunner < manytests/typed/016lists.ml
  val var_0 : '4 list -> int
  val var_10 : ('47 -> '31) -> '47 list -> '31 list
  val var_24 : '60 list -> '60 list -> '60 list
  val var_32 : '75 list list -> '75 list
  val var_33 : ('82 -> unit) -> '82 list -> unit
  val var_38 : '93 list -> '97 list -> ('93 * '97) list
  val var_44 : int
  val var_9 : '20 list -> int
