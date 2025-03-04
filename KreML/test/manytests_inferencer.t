  $ dune exec inferencer  < manytests/typed/001fac.ml
  [ fac : int -> int
  , main : int
  , print_int : int -> unit
   ]

  $ dune exec inferencer < manytests/typed/002fac.ml
  [ fac_cps : int -> (int -> 6) -> 6
  , main : int
  , print_int : int -> unit
   ]

  $ dune exec inferencer < manytests/typed/003fib.ml
  [ fib : int -> int
  , fib_acc : int -> int -> int -> int
  , main : int
  , print_int : int -> unit
   ]

  $ dune exec inferencer < manytests/typed/004manyargs.ml
  [ main : int
  , print_int : int -> unit
  , test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  , test3 : int -> int -> int -> int
  , wrap : 0 -> 0
   ]

  $ dune exec inferencer < manytests/typed/005fix.ml
  [ fac : (int -> int) -> int -> int
  , fix : ((2 -> 3) -> 2 -> 3) -> 2 -> 3
  , main : int
  , print_int : int -> unit
   ]

  $ dune exec inferencer < manytests/typed/006partial.ml
  [ foo : int -> int
  , main : int
  , print_int : int -> unit
   ]

  $ dune exec inferencer < manytests/typed/006partial2.ml
  [ foo : int -> int -> int -> int
  , main : int
  , print_int : int -> unit
   ]

  $ dune exec inferencer < manytests/typed/006partial3.ml
  [ foo : int -> int -> int -> unit
  , main : int
  , print_int : int -> unit
   ]

  $ dune exec inferencer < manytests/typed/007order.ml
  [ _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  , main : unit
  , print_int : int -> unit
   ]

  $ dune exec inferencer < manytests/typed/008ascription.ml
  [ addi : (2 -> bool -> int) -> (2 -> bool) -> 2 -> int
  , main : int
  , print_int : int -> unit
   ]

  $ dune exec inferencer < manytests/typed/009let_poly.ml
  [ print_int : int -> unit
  , temp : int * bool
   ]

  $ dune exec inferencer < manytests/typed/015tuples.ml
  [ feven : 32 * (int -> int) -> int -> int
  , fix : ((2 -> 3) -> 2 -> 3) -> 2 -> 3
  , fixpoly : ((25 -> 26) * (25 -> 26) -> 25 -> 26) * ((25 -> 26) * (25 -> 26) -> 25 -> 26) -> (25 -> 26) * (25 -> 26)
  , fodd : (int -> int) * 43 -> int -> int
  , main : int
  , map : (9 -> 11) -> 9 * 9 -> 11 * 11
  , meven : int -> int
  , modd : int -> int
  , print_int : int -> unit
  , tie : (int -> int) * (int -> int)
   ]

  $ dune exec inferencer < manytests/typed/016lists.ml
  [ append : 62 list -> 62 list -> 62 list
  , cartesian : 94 list -> 103 list -> (94 * 103) list
  , concat : 78 list list -> 78 list
  , iter : (84 -> unit) -> 84 list -> unit
  , length : 3 list -> int
  , length_tail : 21 list -> int
  , main : int
  , map : (42 -> 52) -> 42 list -> 52 list
  , print_int : int -> unit
   ]

  $ dune exec inferencer <<- EOF
  > let f =
  >  let rec helper a b = a, b in
  >  let temp = helper 5 6 in
  >  let temp2 = helper true false in
  >  helper
  [ f : 13 -> 14 -> 13 * 14
  , print_int : int -> unit
   ]
