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
  [""( != )"": ('_p2e -> ('_p2e -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
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
   ""( || )"": (bool -> (bool -> bool)),
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
  [""( != )"": ('_pf -> ('_pf -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_p10 -> (('_p10 list) -> ('_p10 list))),
   ""( < )"": ('_p11 -> ('_p11 -> bool)),
   ""( <= )"": ('_p12 -> ('_p12 -> bool)),
   ""( <> )"": ('_p13 -> ('_p13 -> bool)),
   ""( = )"": ('_p14 -> ('_p14 -> bool)),
   ""( == )"": ('_p15 -> ('_p15 -> bool)),
   ""( > )"": ('_p16 -> ('_p16 -> bool)),
   ""( >= )"": ('_p17 -> ('_p17 -> bool)),
   ""( || )"": (bool -> (bool -> bool)),
   ""fac"": (int -> int),
   ""main"": int,
   ""print_int"": (int -> unit),
   ]

  $ ./typeinference_demo.exe < manytests/typed/002fac.ml
  [""( != )"": ('_p17 -> ('_p17 -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_p18 -> (('_p18 list) -> ('_p18 list))),
   ""( < )"": ('_p19 -> ('_p19 -> bool)),
   ""( <= )"": ('_p1a -> ('_p1a -> bool)),
   ""( <> )"": ('_p1b -> ('_p1b -> bool)),
   ""( = )"": ('_p1c -> ('_p1c -> bool)),
   ""( == )"": ('_p1d -> ('_p1d -> bool)),
   ""( > )"": ('_p1e -> ('_p1e -> bool)),
   ""( >= )"": ('_p1f -> ('_p1f -> bool)),
   ""( || )"": (bool -> (bool -> bool)),
   ""fac_cps"": (int -> ((int -> '_p20) -> '_p20)),
   ""main"": int,
   ""print_int"": (int -> unit),
   ]

  $ ./typeinference_demo.exe < manytests/typed/003fib.ml
  [""( != )"": ('_p28 -> ('_p28 -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_p29 -> (('_p29 list) -> ('_p29 list))),
   ""( < )"": ('_p2a -> ('_p2a -> bool)),
   ""( <= )"": ('_p2b -> ('_p2b -> bool)),
   ""( <> )"": ('_p2c -> ('_p2c -> bool)),
   ""( = )"": ('_p2d -> ('_p2d -> bool)),
   ""( == )"": ('_p2e -> ('_p2e -> bool)),
   ""( > )"": ('_p2f -> ('_p2f -> bool)),
   ""( >= )"": ('_p30 -> ('_p30 -> bool)),
   ""( || )"": (bool -> (bool -> bool)),
   ""fib"": (int -> int),
   ""fib_acc"": (int -> (int -> (int -> int))),
   ""main"": int,
   ""print_int"": (int -> unit),
   ]

  $ ./typeinference_demo.exe < manytests/typed/004manyargs.ml
  [""( != )"": ('_p43 -> ('_p43 -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_p44 -> (('_p44 list) -> ('_p44 list))),
   ""( < )"": ('_p45 -> ('_p45 -> bool)),
   ""( <= )"": ('_p46 -> ('_p46 -> bool)),
   ""( <> )"": ('_p47 -> ('_p47 -> bool)),
   ""( = )"": ('_p48 -> ('_p48 -> bool)),
   ""( == )"": ('_p49 -> ('_p49 -> bool)),
   ""( > )"": ('_p4a -> ('_p4a -> bool)),
   ""( >= )"": ('_p4b -> ('_p4b -> bool)),
   ""( || )"": (bool -> (bool -> bool)),
   ""main"": int,
   ""print_int"": (int -> unit),
   ""test10"": (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> int)))))))))),
   ""test3"": (int -> (int -> (int -> int))),
   ""wrap"": ('_p4c -> '_p4c),
   ]

  $ ./typeinference_demo.exe < manytests/typed/005fix.ml
  [""( != )"": ('_p19 -> ('_p19 -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_p1a -> (('_p1a list) -> ('_p1a list))),
   ""( < )"": ('_p1b -> ('_p1b -> bool)),
   ""( <= )"": ('_p1c -> ('_p1c -> bool)),
   ""( <> )"": ('_p1d -> ('_p1d -> bool)),
   ""( = )"": ('_p1e -> ('_p1e -> bool)),
   ""( == )"": ('_p1f -> ('_p1f -> bool)),
   ""( > )"": ('_p20 -> ('_p20 -> bool)),
   ""( >= )"": ('_p21 -> ('_p21 -> bool)),
   ""( || )"": (bool -> (bool -> bool)),
   ""fac"": ((int -> int) -> (int -> int)),
   ""fix"": ((('_p22 -> '_p23) -> ('_p22 -> '_p23)) -> ('_p22 -> '_p23)),
   ""main"": int,
   ""print_int"": (int -> unit),
   ]

  $ ./typeinference_demo.exe < manytests/typed/006partial.ml
  [""( != )"": ('_p17 -> ('_p17 -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_p18 -> (('_p18 list) -> ('_p18 list))),
   ""( < )"": ('_p19 -> ('_p19 -> bool)),
   ""( <= )"": ('_p1a -> ('_p1a -> bool)),
   ""( <> )"": ('_p1b -> ('_p1b -> bool)),
   ""( = )"": ('_p1c -> ('_p1c -> bool)),
   ""( == )"": ('_p1d -> ('_p1d -> bool)),
   ""( > )"": ('_p1e -> ('_p1e -> bool)),
   ""( >= )"": ('_p1f -> ('_p1f -> bool)),
   ""( || )"": (bool -> (bool -> bool)),
   ""foo"": (int -> int),
   ""main"": int,
   ""print_int"": (int -> unit),
   ]

  $ ./typeinference_demo.exe < manytests/typed/006partial2.ml
  [""( != )"": ('_p17 -> ('_p17 -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_p18 -> (('_p18 list) -> ('_p18 list))),
   ""( < )"": ('_p19 -> ('_p19 -> bool)),
   ""( <= )"": ('_p1a -> ('_p1a -> bool)),
   ""( <> )"": ('_p1b -> ('_p1b -> bool)),
   ""( = )"": ('_p1c -> ('_p1c -> bool)),
   ""( == )"": ('_p1d -> ('_p1d -> bool)),
   ""( > )"": ('_p1e -> ('_p1e -> bool)),
   ""( >= )"": ('_p1f -> ('_p1f -> bool)),
   ""( || )"": (bool -> (bool -> bool)),
   ""foo"": (int -> (int -> (int -> int))),
   ""main"": int,
   ""print_int"": (int -> unit),
   ]

  $ ./typeinference_demo.exe < manytests/typed/006partial3.ml
  [""( != )"": ('_pe -> ('_pe -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
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
   ""( || )"": (bool -> (bool -> bool)),
   ""foo"": (int -> (int -> (int -> unit))),
   ""main"": int,
   ""print_int"": (int -> unit),
   ]

  $ ./typeinference_demo.exe < manytests/typed/007order.ml
  [""( != )"": ('_p25 -> ('_p25 -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_p26 -> (('_p26 list) -> ('_p26 list))),
   ""( < )"": ('_p27 -> ('_p27 -> bool)),
   ""( <= )"": ('_p28 -> ('_p28 -> bool)),
   ""( <> )"": ('_p29 -> ('_p29 -> bool)),
   ""( = )"": ('_p2a -> ('_p2a -> bool)),
   ""( == )"": ('_p2b -> ('_p2b -> bool)),
   ""( > )"": ('_p2c -> ('_p2c -> bool)),
   ""( >= )"": ('_p2d -> ('_p2d -> bool)),
   ""( || )"": (bool -> (bool -> bool)),
   ""_start"": (unit -> (unit -> (int -> (unit -> (int -> (int -> (unit -> (int -> (int -> int))))))))),
   ""main"": unit,
   ""print_int"": (int -> unit),
   ]

  $ ./typeinference_demo.exe < manytests/typed/008ascription.ml
  [""( != )"": ('_p1b -> ('_p1b -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_p1c -> (('_p1c list) -> ('_p1c list))),
   ""( < )"": ('_p1d -> ('_p1d -> bool)),
   ""( <= )"": ('_p1e -> ('_p1e -> bool)),
   ""( <> )"": ('_p1f -> ('_p1f -> bool)),
   ""( = )"": ('_p20 -> ('_p20 -> bool)),
   ""( == )"": ('_p21 -> ('_p21 -> bool)),
   ""( > )"": ('_p22 -> ('_p22 -> bool)),
   ""( >= )"": ('_p23 -> ('_p23 -> bool)),
   ""( || )"": (bool -> (bool -> bool)),
   ""addi"": (('_p24 -> (bool -> int)) -> (('_p24 -> bool) -> ('_p24 -> int))),
   ""main"": int,
   ""print_int"": (int -> unit),
   ]

  $ ./typeinference_demo.exe < manytests/typed/009let_poly.ml
  [""( != )"": ('_p7 -> ('_p7 -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_p8 -> (('_p8 list) -> ('_p8 list))),
   ""( < )"": ('_p9 -> ('_p9 -> bool)),
   ""( <= )"": ('_pa -> ('_pa -> bool)),
   ""( <> )"": ('_pb -> ('_pb -> bool)),
   ""( = )"": ('_pc -> ('_pc -> bool)),
   ""( == )"": ('_pd -> ('_pd -> bool)),
   ""( > )"": ('_pe -> ('_pe -> bool)),
   ""( >= )"": ('_pf -> ('_pf -> bool)),
   ""( || )"": (bool -> (bool -> bool)),
   ""print_int"": (int -> unit),
   ""temp"": (int * bool),
   ]

  $ ./typeinference_demo.exe < manytests/typed/015tuples.ml
  [""( != )"": ('_p61 -> ('_p61 -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_p62 -> (('_p62 list) -> ('_p62 list))),
   ""( < )"": ('_p63 -> ('_p63 -> bool)),
   ""( <= )"": ('_p64 -> ('_p64 -> bool)),
   ""( <> )"": ('_p65 -> ('_p65 -> bool)),
   ""( = )"": ('_p66 -> ('_p66 -> bool)),
   ""( == )"": ('_p67 -> ('_p67 -> bool)),
   ""( > )"": ('_p68 -> ('_p68 -> bool)),
   ""( >= )"": ('_p69 -> ('_p69 -> bool)),
   ""( || )"": (bool -> (bool -> bool)),
   ""feven"": (('_p6a * (int -> int)) -> (int -> int)),
   ""fix"": ((('_p6b -> '_p6c) -> ('_p6b -> '_p6c)) -> ('_p6b -> '_p6c)),
   ""fixpoly"": ((((('_p6d -> '_p6e) * ('_p6d -> '_p6e)) -> ('_p6d -> '_p6e)) * ((('_p6d -> '_p6e) * ('_p6d -> '_p6e)) -> ('_p6d -> '_p6e))) -> (('_p6d -> '_p6e) * ('_p6d -> '_p6e))),
   ""fodd"": (((int -> int) * '_p6f) -> (int -> int)),
   ""main"": int,
   ""map"": (('_p70 -> '_p71) -> (('_p70 * '_p70) -> ('_p71 * '_p71))),
   ""meven"": (int -> int),
   ""modd"": (int -> int),
   ""print_int"": (int -> unit),
   ""tie"": ((int -> int) * (int -> int)),
   ]

  $ ./typeinference_demo.exe < manytests/typed/016lists.ml
  [""( != )"": ('_pd3 -> ('_pd3 -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_pd4 -> (('_pd4 list) -> ('_pd4 list))),
   ""( < )"": ('_pd5 -> ('_pd5 -> bool)),
   ""( <= )"": ('_pd6 -> ('_pd6 -> bool)),
   ""( <> )"": ('_pd7 -> ('_pd7 -> bool)),
   ""( = )"": ('_pd8 -> ('_pd8 -> bool)),
   ""( == )"": ('_pd9 -> ('_pd9 -> bool)),
   ""( > )"": ('_pda -> ('_pda -> bool)),
   ""( >= )"": ('_pdb -> ('_pdb -> bool)),
   ""( || )"": (bool -> (bool -> bool)),
   ""append"": (('_pdc list) -> (('_pdc list) -> ('_pdc list))),
   ""cartesian"": (('_pdd list) -> (('_pde list) -> (('_pdd * '_pde) list))),
   ""concat"": ((('_pdf list) list) -> ('_pdf list)),
   ""iter"": (('_pe0 -> unit) -> (('_pe0 list) -> unit)),
   ""length"": (('_pe1 list) -> int),
   ""length_tail"": (('_pe2 list) -> int),
   ""main"": int,
   ""map"": (('_pe3 -> '_pe4) -> (('_pe3 list) -> ('_pe4 list))),
   ""print_int"": (int -> unit),
   ]
