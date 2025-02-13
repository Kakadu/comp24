  $ ./infer_tests.exe < manytests/typed/001fac.ml
  "fac": int -> int
  "main": int

  $ ./infer_tests.exe < manytests/typed/002fac.ml
  "fac_cps": '12 . int -> (int -> '12) -> '12
  "main": int

  $ ./infer_tests.exe < manytests/typed/003fib.ml
  "fib": int -> int
  "fib_acc": int -> int -> int -> int
  "main": int

  $ ./infer_tests.exe < manytests/typed/004manyargs.ml
  "main": int
  "test10": int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  "test3": int -> int -> int -> int
  "wrap": '0 . '0 -> '0

  $ ./infer_tests.exe < manytests/typed/005fix.ml
  "fac": (int -> int) -> int -> int
  "fix": '2 '3 . (('2 -> '3) -> '2 -> '3) -> '2 -> '3
  "main": int

  $ ./infer_tests.exe < manytests/typed/006partial.ml
  "foo": int -> int
  "main": int

  $ ./infer_tests.exe < manytests/typed/006partial2.ml
  "foo": int -> int -> int -> int
  "main": int

  $ ./infer_tests.exe < manytests/typed/006partial3.ml
  "foo": int -> int -> int -> unit
  "main": int

  $ ./infer_tests.exe < manytests/typed/007order.ml
  "_start": unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  "main": unit
  $ ./infer_tests.exe < manytests/typed/008ascription.ml
  "addi": '2 . ('2 -> bool -> int) -> ('2 -> bool) -> '2 -> int
  "main": int

  $ ./infer_tests.exe < manytests/typed/009let_poly.ml
  "temp": int * bool

  $ ./infer_tests.exe < manytests/typed/015tuples.ml
  "feven": '32 . '32 * (int -> int) -> int -> int
  "fix": '2 '3 . (('2 -> '3) -> '2 -> '3) -> '2 -> '3
  "fixpoly": '25 '26 . (('25 -> '26) * ('25 -> '26) -> '25 -> '26) * (('25 -> '26) * ('25 -> '26) -> '25 -> '26) -> ('25 -> '26) * ('25 -> '26)
  "fodd": '44 . (int -> int) * '44 -> int -> int
  "main": int
  "map": '8 '10 . ('8 -> '10) -> '8 * '8 -> '10 * '10
  "meven": int -> int
  "modd": int -> int
  "tie": (int -> int) * (int -> int)

  $ ./infer_tests.exe < manytests/typed/016lists.ml
  "append": '62 . '62 list -> '62 list -> '62 list
  "cartesian": '94 '105 . '94 list -> '105 list -> ('94 * '105) list
  "concat": '78 . ('78 list) list -> '78 list
  "iter": '84 . ('84 -> unit) -> '84 list -> unit
  "length": '3 . '3 list -> int
  "length_tail": '21 . '21 list -> int
  "main": int
  "map": '27 '28 . ('27 -> '28) -> '27 list -> '28 list

  $ ./infer_tests.exe < manytests/do_not_type/001.ml
  Unbound_variable: "fac"

  $ ./infer_tests.exe < manytests/do_not_type/002if.ml
  Unification_failed: int # bool

  $ ./infer_tests.exe < manytests/do_not_type/003occurs.ml
  Occurs_check

  $ ./infer_tests.exe < manytests/do_not_type/004let_poly.ml
  Unification_failed: bool # int

  $ ./infer_tests.exe < manytests/do_not_type/015tuples.ml
  "a": '0
  "b": '1
