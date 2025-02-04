  $ dune exec typeinference_demo << EOF
  > let (-) = fun (a:int) (b:int)->  a
  > let (+) = fun (a:int) (b:int)->  a
  > 
  > let fibo = fun n ->
  >  let rec fiboCPS = fun n acc -> match n with
  >  | 0 -> acc 0
  >  | 1 -> acc 1
  >  | _ -> fiboCPS (n - 1) (fun x -> fiboCPS (n - 2) (fun y -> acc (x + y)))
  >    in fiboCPS n (fun x -> x)
  > 
  > let __ = 5
  > let __ = 5
  > let ___ = 1
  > let ___ = []
  > let six = __ + 1
  > EOF
  [""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_p2f -> (('_p2f list) -> ('_p2f list))),
   ""( < )"": ('_p30 -> ('_p30 -> bool)),
   ""( <= )"": ('_p31 -> ('_p31 -> bool)),
   ""( <> )"": ('_p32 -> ('_p32 -> bool)),
   ""( = )"": ('_p33 -> ('_p33 -> bool)),
   ""( == )"": ('_p34 -> ('_p34 -> bool)),
   ""( > )"": ('_p35 -> ('_p35 -> bool)),
   ""( >= )"": ('_p36 -> ('_p36 -> bool)),
   ""__"": int,
   ""___"": ('_p37 list),
   ""fibo"": (int -> int),
   ""print_int"": (int -> unit),
   ""six"": int,
   ]

  $ ./typeinference_demo.exe < manytests/do_not_type/001.ml
  Infer error: Unbound value: fac

  $ ./typeinference_demo.exe < manytests/do_not_type/002if.ml
  Infer error: Can not unify `TInt` and `TBool`

  $ ./typeinference_demo.exe < manytests/do_not_type/003occurs.ml
  Infer error: The type variable _p2 occurs inside (TFunction ((TPoly "_p2"), (TPoly "_p6")))

  $ ./typeinference_demo.exe < manytests/do_not_type/004let_poly.ml
  Infer error: Can not unify `TInt` and `TBool`

  $ ./typeinference_demo.exe < manytests/do_not_type/015tuples.ml
  Infer error:  Only variables are allowed as left-hand side of `let rec'

  $ ./typeinference_demo.exe < manytests/do_not_type/099.ml
  Parser error: : end_of_input

  $ ./typeinference_demo.exe < manytests/typed/001fac.ml
  [""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_pf -> (('_pf list) -> ('_pf list))),
   ""( < )"": ('_p10 -> ('_p10 -> bool)),
   ""( <= )"": ('_p11 -> ('_p11 -> bool)),
   ""( <> )"": ('_p12 -> ('_p12 -> bool)),
   ""( = )"": ('_p13 -> ('_p13 -> bool)),
   ""( == )"": ('_p14 -> ('_p14 -> bool)),
   ""( > )"": ('_p15 -> ('_p15 -> bool)),
   ""( >= )"": ('_p16 -> ('_p16 -> bool)),
   ""fac"": (int -> int),
   ""main"": int,
   ""print_int"": (int -> unit),
   ]

  $ ./typeinference_demo.exe < manytests/typed/002fac.ml
  [""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_p17 -> (('_p17 list) -> ('_p17 list))),
   ""( < )"": ('_p18 -> ('_p18 -> bool)),
   ""( <= )"": ('_p19 -> ('_p19 -> bool)),
   ""( <> )"": ('_p1a -> ('_p1a -> bool)),
   ""( = )"": ('_p1b -> ('_p1b -> bool)),
   ""( == )"": ('_p1c -> ('_p1c -> bool)),
   ""( > )"": ('_p1d -> ('_p1d -> bool)),
   ""( >= )"": ('_p1e -> ('_p1e -> bool)),
   ""fac_cps"": (int -> ((int -> '_p1f) -> '_p1f)),
   ""main"": int,
   ""print_int"": (int -> unit),
   ]

  $ ./typeinference_demo.exe < manytests/typed/003fib.ml
  [""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_p28 -> (('_p28 list) -> ('_p28 list))),
   ""( < )"": ('_p29 -> ('_p29 -> bool)),
   ""( <= )"": ('_p2a -> ('_p2a -> bool)),
   ""( <> )"": ('_p2b -> ('_p2b -> bool)),
   ""( = )"": ('_p2c -> ('_p2c -> bool)),
   ""( == )"": ('_p2d -> ('_p2d -> bool)),
   ""( > )"": ('_p2e -> ('_p2e -> bool)),
   ""( >= )"": ('_p2f -> ('_p2f -> bool)),
   ""fib"": (int -> int),
   ""fib_acc"": (int -> (int -> (int -> int))),
   ""main"": int,
   ""print_int"": (int -> unit),
   ]

  $ ./typeinference_demo.exe < manytests/typed/004manyargs.ml
  [""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_p43 -> (('_p43 list) -> ('_p43 list))),
   ""( < )"": ('_p44 -> ('_p44 -> bool)),
   ""( <= )"": ('_p45 -> ('_p45 -> bool)),
   ""( <> )"": ('_p46 -> ('_p46 -> bool)),
   ""( = )"": ('_p47 -> ('_p47 -> bool)),
   ""( == )"": ('_p48 -> ('_p48 -> bool)),
   ""( > )"": ('_p49 -> ('_p49 -> bool)),
   ""( >= )"": ('_p4a -> ('_p4a -> bool)),
   ""main"": int,
   ""print_int"": (int -> unit),
   ""test10"": (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> int)))))))))),
   ""test3"": (int -> (int -> (int -> int))),
   ""wrap"": ('_p4b -> '_p4b),
   ]

  $ ./typeinference_demo.exe < manytests/typed/005fix.ml
  [""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_p19 -> (('_p19 list) -> ('_p19 list))),
   ""( < )"": ('_p1a -> ('_p1a -> bool)),
   ""( <= )"": ('_p1b -> ('_p1b -> bool)),
   ""( <> )"": ('_p1c -> ('_p1c -> bool)),
   ""( = )"": ('_p1d -> ('_p1d -> bool)),
   ""( == )"": ('_p1e -> ('_p1e -> bool)),
   ""( > )"": ('_p1f -> ('_p1f -> bool)),
   ""( >= )"": ('_p20 -> ('_p20 -> bool)),
   ""fac"": ((int -> int) -> (int -> int)),
   ""fix"": ((('_p21 -> '_p22) -> ('_p21 -> '_p22)) -> ('_p21 -> '_p22)),
   ""main"": int,
   ""print_int"": (int -> unit),
   ]

  $ ./typeinference_demo.exe < manytests/typed/006partial.ml
  [""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_p17 -> (('_p17 list) -> ('_p17 list))),
   ""( < )"": ('_p18 -> ('_p18 -> bool)),
   ""( <= )"": ('_p19 -> ('_p19 -> bool)),
   ""( <> )"": ('_p1a -> ('_p1a -> bool)),
   ""( = )"": ('_p1b -> ('_p1b -> bool)),
   ""( == )"": ('_p1c -> ('_p1c -> bool)),
   ""( > )"": ('_p1d -> ('_p1d -> bool)),
   ""( >= )"": ('_p1e -> ('_p1e -> bool)),
   ""foo"": (int -> int),
   ""main"": int,
   ""print_int"": (int -> unit),
   ]

  $ ./typeinference_demo.exe < manytests/typed/006partial2.ml
  [""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_p17 -> (('_p17 list) -> ('_p17 list))),
   ""( < )"": ('_p18 -> ('_p18 -> bool)),
   ""( <= )"": ('_p19 -> ('_p19 -> bool)),
   ""( <> )"": ('_p1a -> ('_p1a -> bool)),
   ""( = )"": ('_p1b -> ('_p1b -> bool)),
   ""( == )"": ('_p1c -> ('_p1c -> bool)),
   ""( > )"": ('_p1d -> ('_p1d -> bool)),
   ""( >= )"": ('_p1e -> ('_p1e -> bool)),
   ""foo"": (int -> (int -> (int -> int))),
   ""main"": int,
   ""print_int"": (int -> unit),
   ]

  $ ./typeinference_demo.exe < manytests/typed/006partial3.ml
  [""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_pe -> (('_pe list) -> ('_pe list))),
   ""( < )"": ('_pf -> ('_pf -> bool)),
   ""( <= )"": ('_p10 -> ('_p10 -> bool)),
   ""( <> )"": ('_p11 -> ('_p11 -> bool)),
   ""( = )"": ('_p12 -> ('_p12 -> bool)),
   ""( == )"": ('_p13 -> ('_p13 -> bool)),
   ""( > )"": ('_p14 -> ('_p14 -> bool)),
   ""( >= )"": ('_p15 -> ('_p15 -> bool)),
   ""foo"": (int -> (int -> (int -> unit))),
   ""main"": int,
   ""print_int"": (int -> unit),
   ]

  $ ./typeinference_demo.exe < manytests/typed/007order.ml
  [""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_p25 -> (('_p25 list) -> ('_p25 list))),
   ""( < )"": ('_p26 -> ('_p26 -> bool)),
   ""( <= )"": ('_p27 -> ('_p27 -> bool)),
   ""( <> )"": ('_p28 -> ('_p28 -> bool)),
   ""( = )"": ('_p29 -> ('_p29 -> bool)),
   ""( == )"": ('_p2a -> ('_p2a -> bool)),
   ""( > )"": ('_p2b -> ('_p2b -> bool)),
   ""( >= )"": ('_p2c -> ('_p2c -> bool)),
   ""_start"": (unit -> (unit -> (int -> (unit -> (int -> (int -> (unit -> (int -> (int -> int))))))))),
   ""main"": unit,
   ""print_int"": (int -> unit),
   ]

  $ ./typeinference_demo.exe < manytests/typed/008ascription.ml
  [""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_p1b -> (('_p1b list) -> ('_p1b list))),
   ""( < )"": ('_p1c -> ('_p1c -> bool)),
   ""( <= )"": ('_p1d -> ('_p1d -> bool)),
   ""( <> )"": ('_p1e -> ('_p1e -> bool)),
   ""( = )"": ('_p1f -> ('_p1f -> bool)),
   ""( == )"": ('_p20 -> ('_p20 -> bool)),
   ""( > )"": ('_p21 -> ('_p21 -> bool)),
   ""( >= )"": ('_p22 -> ('_p22 -> bool)),
   ""addi"": (('_p23 -> (bool -> int)) -> (('_p23 -> bool) -> ('_p23 -> int))),
   ""main"": int,
   ""print_int"": (int -> unit),
   ]

  $ ./typeinference_demo.exe < manytests/typed/009let_poly.ml
  [""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_p7 -> (('_p7 list) -> ('_p7 list))),
   ""( < )"": ('_p8 -> ('_p8 -> bool)),
   ""( <= )"": ('_p9 -> ('_p9 -> bool)),
   ""( <> )"": ('_pa -> ('_pa -> bool)),
   ""( = )"": ('_pb -> ('_pb -> bool)),
   ""( == )"": ('_pc -> ('_pc -> bool)),
   ""( > )"": ('_pd -> ('_pd -> bool)),
   ""( >= )"": ('_pe -> ('_pe -> bool)),
   ""print_int"": (int -> unit),
   ""temp"": (int * bool),
   ]

  $ ./typeinference_demo.exe < manytests/typed/015tuples.ml
  [""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_p71 -> (('_p71 list) -> ('_p71 list))),
   ""( < )"": ('_p72 -> ('_p72 -> bool)),
   ""( <= )"": ('_p73 -> ('_p73 -> bool)),
   ""( <> )"": ('_p74 -> ('_p74 -> bool)),
   ""( = )"": ('_p75 -> ('_p75 -> bool)),
   ""( == )"": ('_p76 -> ('_p76 -> bool)),
   ""( > )"": ('_p77 -> ('_p77 -> bool)),
   ""( >= )"": ('_p78 -> ('_p78 -> bool)),
   ""feven"": (('_p79 * '_p7a) -> (int -> int)),
   ""fix"": ((('_p7b -> '_p7c) -> ('_p7b -> '_p7c)) -> ('_p7b -> '_p7c)),
   ""fixpoly"": (('_p7d * '_p7e) -> (('_p7f -> '_p80) * ('_p7f -> '_p80))),
   ""fodd"": (('_p81 * '_p82) -> (int -> int)),
   ""main"": int,
   ""map"": (('_p86 -> '_p85) -> (('_p83 * '_p84) -> ('_p85 * '_p85))),
   ""meven"": (int -> int),
   ""modd"": (int -> int),
   ""print_int"": (int -> unit),
   ""tie"": (('_p87 -> '_p88) * ('_p87 -> '_p88)),
   ]

  $ ./typeinference_demo.exe < manytests/typed/016lists.ml
  [""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_pb13 -> (('_pb13 list) -> ('_pb13 list))),
   ""( < )"": ('_pb14 -> ('_pb14 -> bool)),
   ""( <= )"": ('_pb15 -> ('_pb15 -> bool)),
   ""( <> )"": ('_pb16 -> ('_pb16 -> bool)),
   ""( = )"": ('_pb17 -> ('_pb17 -> bool)),
   ""( == )"": ('_pb18 -> ('_pb18 -> bool)),
   ""( > )"": ('_pb19 -> ('_pb19 -> bool)),
   ""( >= )"": ('_pb1a -> ('_pb1a -> bool)),
   ""append"": (('_pb1b list) -> (('_pb1c list) -> ('_pb1c list))),
   ""cartesian"": (('_pb1d list) -> (('_pb1e list) -> ('_pb1f list))),
   ""concat"": ((('_pb20 list) list) -> ('_pb21 list)),
   ""iter"": (('_pb22 -> '_pb23) -> (('_pb22 list) -> unit)),
   ""length"": (('_pb24 list) -> int),
   ""length_tail"": (('_pb25 list) -> int),
   ""ll_0"": (int -> (('_pb26 list) -> int)),
   ""ll_1"": ((('_pb27 list) list) -> ('_pb28 list)),
   ""ll_2"": ('_pb29 -> ('_pb2a -> ('_pb29 * '_pb2a))),
   ""main"": int,
   ""map"": (('_pb2b -> '_pb2c) -> (('_pb2b list) -> ('_pb2d list))),
   ""print_int"": (int -> unit),
   ]
