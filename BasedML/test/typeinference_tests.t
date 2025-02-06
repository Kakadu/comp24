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
   ""( :: )"": ('_p61 -> (('_p61 list) -> ('_p61 list))),
   ""( < )"": ('_p62 -> ('_p62 -> bool)),
   ""( <= )"": ('_p63 -> ('_p63 -> bool)),
   ""( <> )"": ('_p64 -> ('_p64 -> bool)),
   ""( = )"": ('_p65 -> ('_p65 -> bool)),
   ""( == )"": ('_p66 -> ('_p66 -> bool)),
   ""( > )"": ('_p67 -> ('_p67 -> bool)),
   ""( >= )"": ('_p68 -> ('_p68 -> bool)),
   ""feven"": (('_p69 * (int -> int)) -> (int -> int)),
   ""fix"": ((('_p6a -> '_p6b) -> ('_p6a -> '_p6b)) -> ('_p6a -> '_p6b)),
   ""fixpoly"": ((((('_p6c -> '_p6d) * ('_p6c -> '_p6d)) -> ('_p6c -> '_p6d)) * ((('_p6c -> '_p6d) * ('_p6c -> '_p6d)) -> ('_p6c -> '_p6d))) -> (('_p6c -> '_p6d) * ('_p6c -> '_p6d))),
   ""fodd"": (((int -> int) * '_p6e) -> (int -> int)),
   ""main"": int,
   ""map"": (('_p6f -> '_p70) -> (('_p6f * '_p6f) -> ('_p70 * '_p70))),
   ""meven"": (int -> int),
   ""modd"": (int -> int),
   ""print_int"": (int -> unit),
   ""tie"": ((int -> int) * (int -> int)),
   ]

  $ ./typeinference_demo.exe < manytests/typed/016lists.ml
  [""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_pda -> (('_pda list) -> ('_pda list))),
   ""( < )"": ('_pdb -> ('_pdb -> bool)),
   ""( <= )"": ('_pdc -> ('_pdc -> bool)),
   ""( <> )"": ('_pdd -> ('_pdd -> bool)),
   ""( = )"": ('_pde -> ('_pde -> bool)),
   ""( == )"": ('_pdf -> ('_pdf -> bool)),
   ""( > )"": ('_pe0 -> ('_pe0 -> bool)),
   ""( >= )"": ('_pe1 -> ('_pe1 -> bool)),
   ""append"": (('_pe2 list) -> (('_pe2 list) -> ('_pe2 list))),
   ""cartesian"": (('_pe3 list) -> (('_pe4 list) -> (('_pe3 * '_pe4) list))),
   ""concat"": ((('_pe5 list) list) -> ('_pe5 list)),
   ""iter"": (('_pe6 -> unit) -> (('_pe6 list) -> unit)),
   ""length"": (('_pe7 list) -> int),
   ""length_tail"": (('_pe8 list) -> int),
   ""main"": int,
   ""map"": (('_pe9 -> '_pea) -> (('_pe9 list) -> ('_pea list))),
   ""print_int"": (int -> unit),
   ]
