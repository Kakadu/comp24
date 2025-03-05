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
  [""( != )"": ('p2e -> ('p2e -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('p2f -> (('p2f list) -> ('p2f list))),
   ""( < )"": ('p30 -> ('p30 -> bool)),
   ""( <= )"": ('p31 -> ('p31 -> bool)),
   ""( <> )"": ('p32 -> ('p32 -> bool)),
   ""( = )"": ('p33 -> ('p33 -> bool)),
   ""( == )"": ('p34 -> ('p34 -> bool)),
   ""( > )"": ('p35 -> ('p35 -> bool)),
   ""( >= )"": ('p36 -> ('p36 -> bool)),
   ""( || )"": (bool -> (bool -> bool)),
   ""__"": int,
   ""___"": ('p37 list),
   ""fibo"": (int -> int),
   ""print_int"": (int -> unit),
   ""six"": int,
   ]

  $ ./typeinference_demo.exe < manytests/do_not_type/001.ml
  Infer error: Unbound value: fac

  $ ./typeinference_demo.exe < manytests/do_not_type/002if.ml
  Infer error: Can not unify `TInt` and `TBool`

  $ ./typeinference_demo.exe < manytests/do_not_type/003occurs.ml
  Infer error: The type variable p2 occurs inside (TFunction ((TPoly "p2"), (TPoly "p6")))

  $ ./typeinference_demo.exe < manytests/do_not_type/004let_poly.ml
  Infer error: Can not unify `TInt` and `TBool`

  $ ./typeinference_demo.exe < manytests/do_not_type/015tuples.ml
  Infer error:  Only variables are allowed as left-hand side of `let rec'

  $ ./typeinference_demo.exe < manytests/do_not_type/099.ml
  Parser error: : end_of_input

  $ ./typeinference_demo.exe < manytests/typed/001fac.ml
  [""( != )"": ('pf -> ('pf -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('p10 -> (('p10 list) -> ('p10 list))),
   ""( < )"": ('p11 -> ('p11 -> bool)),
   ""( <= )"": ('p12 -> ('p12 -> bool)),
   ""( <> )"": ('p13 -> ('p13 -> bool)),
   ""( = )"": ('p14 -> ('p14 -> bool)),
   ""( == )"": ('p15 -> ('p15 -> bool)),
   ""( > )"": ('p16 -> ('p16 -> bool)),
   ""( >= )"": ('p17 -> ('p17 -> bool)),
   ""( || )"": (bool -> (bool -> bool)),
   ""fac"": (int -> int),
   ""main"": int,
   ""print_int"": (int -> unit),
   ]

  $ ./typeinference_demo.exe < manytests/typed/002fac.ml
  [""( != )"": ('p17 -> ('p17 -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('p18 -> (('p18 list) -> ('p18 list))),
   ""( < )"": ('p19 -> ('p19 -> bool)),
   ""( <= )"": ('p1a -> ('p1a -> bool)),
   ""( <> )"": ('p1b -> ('p1b -> bool)),
   ""( = )"": ('p1c -> ('p1c -> bool)),
   ""( == )"": ('p1d -> ('p1d -> bool)),
   ""( > )"": ('p1e -> ('p1e -> bool)),
   ""( >= )"": ('p1f -> ('p1f -> bool)),
   ""( || )"": (bool -> (bool -> bool)),
   ""fac_cps"": (int -> ((int -> 'p20) -> 'p20)),
   ""main"": int,
   ""print_int"": (int -> unit),
   ]

  $ ./typeinference_demo.exe < manytests/typed/003fib.ml
  [""( != )"": ('p28 -> ('p28 -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('p29 -> (('p29 list) -> ('p29 list))),
   ""( < )"": ('p2a -> ('p2a -> bool)),
   ""( <= )"": ('p2b -> ('p2b -> bool)),
   ""( <> )"": ('p2c -> ('p2c -> bool)),
   ""( = )"": ('p2d -> ('p2d -> bool)),
   ""( == )"": ('p2e -> ('p2e -> bool)),
   ""( > )"": ('p2f -> ('p2f -> bool)),
   ""( >= )"": ('p30 -> ('p30 -> bool)),
   ""( || )"": (bool -> (bool -> bool)),
   ""fib"": (int -> int),
   ""fib_acc"": (int -> (int -> (int -> int))),
   ""main"": int,
   ""print_int"": (int -> unit),
   ]

  $ ./typeinference_demo.exe < manytests/typed/004manyargs.ml
  [""( != )"": ('p43 -> ('p43 -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('p44 -> (('p44 list) -> ('p44 list))),
   ""( < )"": ('p45 -> ('p45 -> bool)),
   ""( <= )"": ('p46 -> ('p46 -> bool)),
   ""( <> )"": ('p47 -> ('p47 -> bool)),
   ""( = )"": ('p48 -> ('p48 -> bool)),
   ""( == )"": ('p49 -> ('p49 -> bool)),
   ""( > )"": ('p4a -> ('p4a -> bool)),
   ""( >= )"": ('p4b -> ('p4b -> bool)),
   ""( || )"": (bool -> (bool -> bool)),
   ""main"": int,
   ""print_int"": (int -> unit),
   ""test10"": (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> int)))))))))),
   ""test3"": (int -> (int -> (int -> int))),
   ""wrap"": ('p4c -> 'p4c),
   ]

  $ ./typeinference_demo.exe < manytests/typed/005fix.ml
  [""( != )"": ('p19 -> ('p19 -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('p1a -> (('p1a list) -> ('p1a list))),
   ""( < )"": ('p1b -> ('p1b -> bool)),
   ""( <= )"": ('p1c -> ('p1c -> bool)),
   ""( <> )"": ('p1d -> ('p1d -> bool)),
   ""( = )"": ('p1e -> ('p1e -> bool)),
   ""( == )"": ('p1f -> ('p1f -> bool)),
   ""( > )"": ('p20 -> ('p20 -> bool)),
   ""( >= )"": ('p21 -> ('p21 -> bool)),
   ""( || )"": (bool -> (bool -> bool)),
   ""fac"": ((int -> int) -> (int -> int)),
   ""fix"": ((('p22 -> 'p23) -> ('p22 -> 'p23)) -> ('p22 -> 'p23)),
   ""main"": int,
   ""print_int"": (int -> unit),
   ]

  $ ./typeinference_demo.exe < manytests/typed/006partial.ml
  [""( != )"": ('p17 -> ('p17 -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('p18 -> (('p18 list) -> ('p18 list))),
   ""( < )"": ('p19 -> ('p19 -> bool)),
   ""( <= )"": ('p1a -> ('p1a -> bool)),
   ""( <> )"": ('p1b -> ('p1b -> bool)),
   ""( = )"": ('p1c -> ('p1c -> bool)),
   ""( == )"": ('p1d -> ('p1d -> bool)),
   ""( > )"": ('p1e -> ('p1e -> bool)),
   ""( >= )"": ('p1f -> ('p1f -> bool)),
   ""( || )"": (bool -> (bool -> bool)),
   ""foo"": (int -> int),
   ""main"": int,
   ""print_int"": (int -> unit),
   ]

  $ ./typeinference_demo.exe < manytests/typed/006partial2.ml
  [""( != )"": ('p17 -> ('p17 -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('p18 -> (('p18 list) -> ('p18 list))),
   ""( < )"": ('p19 -> ('p19 -> bool)),
   ""( <= )"": ('p1a -> ('p1a -> bool)),
   ""( <> )"": ('p1b -> ('p1b -> bool)),
   ""( = )"": ('p1c -> ('p1c -> bool)),
   ""( == )"": ('p1d -> ('p1d -> bool)),
   ""( > )"": ('p1e -> ('p1e -> bool)),
   ""( >= )"": ('p1f -> ('p1f -> bool)),
   ""( || )"": (bool -> (bool -> bool)),
   ""foo"": (int -> (int -> (int -> int))),
   ""main"": int,
   ""print_int"": (int -> unit),
   ]

  $ ./typeinference_demo.exe < manytests/typed/006partial3.ml
  [""( != )"": ('pe -> ('pe -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('pf -> (('pf list) -> ('pf list))),
   ""( < )"": ('p10 -> ('p10 -> bool)),
   ""( <= )"": ('p11 -> ('p11 -> bool)),
   ""( <> )"": ('p12 -> ('p12 -> bool)),
   ""( = )"": ('p13 -> ('p13 -> bool)),
   ""( == )"": ('p14 -> ('p14 -> bool)),
   ""( > )"": ('p15 -> ('p15 -> bool)),
   ""( >= )"": ('p16 -> ('p16 -> bool)),
   ""( || )"": (bool -> (bool -> bool)),
   ""foo"": (int -> (int -> (int -> unit))),
   ""main"": int,
   ""print_int"": (int -> unit),
   ]

  $ ./typeinference_demo.exe < manytests/typed/007order.ml
  [""( != )"": ('p25 -> ('p25 -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('p26 -> (('p26 list) -> ('p26 list))),
   ""( < )"": ('p27 -> ('p27 -> bool)),
   ""( <= )"": ('p28 -> ('p28 -> bool)),
   ""( <> )"": ('p29 -> ('p29 -> bool)),
   ""( = )"": ('p2a -> ('p2a -> bool)),
   ""( == )"": ('p2b -> ('p2b -> bool)),
   ""( > )"": ('p2c -> ('p2c -> bool)),
   ""( >= )"": ('p2d -> ('p2d -> bool)),
   ""( || )"": (bool -> (bool -> bool)),
   ""_start"": (unit -> (unit -> (int -> (unit -> (int -> (int -> (unit -> (int -> (int -> int))))))))),
   ""main"": unit,
   ""print_int"": (int -> unit),
   ]

  $ ./typeinference_demo.exe < manytests/typed/008ascription.ml
  [""( != )"": ('p1b -> ('p1b -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('p1c -> (('p1c list) -> ('p1c list))),
   ""( < )"": ('p1d -> ('p1d -> bool)),
   ""( <= )"": ('p1e -> ('p1e -> bool)),
   ""( <> )"": ('p1f -> ('p1f -> bool)),
   ""( = )"": ('p20 -> ('p20 -> bool)),
   ""( == )"": ('p21 -> ('p21 -> bool)),
   ""( > )"": ('p22 -> ('p22 -> bool)),
   ""( >= )"": ('p23 -> ('p23 -> bool)),
   ""( || )"": (bool -> (bool -> bool)),
   ""addi"": (('p24 -> (bool -> int)) -> (('p24 -> bool) -> ('p24 -> int))),
   ""main"": int,
   ""print_int"": (int -> unit),
   ]

  $ ./typeinference_demo.exe < manytests/typed/009let_poly.ml
  [""( != )"": ('p7 -> ('p7 -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('p8 -> (('p8 list) -> ('p8 list))),
   ""( < )"": ('p9 -> ('p9 -> bool)),
   ""( <= )"": ('pa -> ('pa -> bool)),
   ""( <> )"": ('pb -> ('pb -> bool)),
   ""( = )"": ('pc -> ('pc -> bool)),
   ""( == )"": ('pd -> ('pd -> bool)),
   ""( > )"": ('pe -> ('pe -> bool)),
   ""( >= )"": ('pf -> ('pf -> bool)),
   ""( || )"": (bool -> (bool -> bool)),
   ""print_int"": (int -> unit),
   ""temp"": (int * bool),
   ]

  $ ./typeinference_demo.exe < manytests/typed/015tuples.ml
  [""( != )"": ('p61 -> ('p61 -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('p62 -> (('p62 list) -> ('p62 list))),
   ""( < )"": ('p63 -> ('p63 -> bool)),
   ""( <= )"": ('p64 -> ('p64 -> bool)),
   ""( <> )"": ('p65 -> ('p65 -> bool)),
   ""( = )"": ('p66 -> ('p66 -> bool)),
   ""( == )"": ('p67 -> ('p67 -> bool)),
   ""( > )"": ('p68 -> ('p68 -> bool)),
   ""( >= )"": ('p69 -> ('p69 -> bool)),
   ""( || )"": (bool -> (bool -> bool)),
   ""feven"": (('p6a * (int -> int)) -> (int -> int)),
   ""fix"": ((('p6b -> 'p6c) -> ('p6b -> 'p6c)) -> ('p6b -> 'p6c)),
   ""fixpoly"": ((((('p6d -> 'p6e) * ('p6d -> 'p6e)) -> ('p6d -> 'p6e)) * ((('p6d -> 'p6e) * ('p6d -> 'p6e)) -> ('p6d -> 'p6e))) -> (('p6d -> 'p6e) * ('p6d -> 'p6e))),
   ""fodd"": (((int -> int) * 'p6f) -> (int -> int)),
   ""main"": int,
   ""map"": (('p70 -> 'p71) -> (('p70 * 'p70) -> ('p71 * 'p71))),
   ""meven"": (int -> int),
   ""modd"": (int -> int),
   ""print_int"": (int -> unit),
   ""tie"": ((int -> int) * (int -> int)),
   ]

  $ ./typeinference_demo.exe < manytests/typed/016lists.ml
  [""( != )"": ('pd3 -> ('pd3 -> bool)),
   ""( && )"": (bool -> (bool -> bool)),
   ""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('pd4 -> (('pd4 list) -> ('pd4 list))),
   ""( < )"": ('pd5 -> ('pd5 -> bool)),
   ""( <= )"": ('pd6 -> ('pd6 -> bool)),
   ""( <> )"": ('pd7 -> ('pd7 -> bool)),
   ""( = )"": ('pd8 -> ('pd8 -> bool)),
   ""( == )"": ('pd9 -> ('pd9 -> bool)),
   ""( > )"": ('pda -> ('pda -> bool)),
   ""( >= )"": ('pdb -> ('pdb -> bool)),
   ""( || )"": (bool -> (bool -> bool)),
   ""append"": (('pdc list) -> (('pdc list) -> ('pdc list))),
   ""cartesian"": (('pdd list) -> (('pde list) -> (('pdd * 'pde) list))),
   ""concat"": ((('pdf list) list) -> ('pdf list)),
   ""iter"": (('pe0 -> unit) -> (('pe0 list) -> unit)),
   ""length"": (('pe1 list) -> int),
   ""length_tail"": (('pe2 list) -> int),
   ""main"": int,
   ""map"": (('pe3 -> 'pe4) -> (('pe3 list) -> ('pe4 list))),
   ""print_int"": (int -> unit),
   ]
