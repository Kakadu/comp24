  $ dune exec ./anf_types_demo.exe << EOF
  > let rec fact_cps n cont =
  >   if (n = 0) then
  >    cont 1
  >   else
  >    fact_cps (n - 1) (fun acc -> cont (n * acc))
  Name: ( <> )
  Original program: ('_p13 -> ('_p13 -> bool))
  ANF: ('_p21 -> ('_p21 -> bool))
  
  Name: ( / )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( + )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( * )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( - )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( < )
  Original program: ('_p11 -> ('_p11 -> bool))
  ANF: ('_p1f -> ('_p1f -> bool))
  
  Name: ( :: )
  Original program: ('_p10 -> (('_p10 list) -> ('_p10 list)))
  ANF: ('_p1e -> (('_p1e list) -> ('_p1e list)))
  
  Name: ( <= )
  Original program: ('_p12 -> ('_p12 -> bool))
  ANF: ('_p20 -> ('_p20 -> bool))
  
  Name: ( == )
  Original program: ('_p15 -> ('_p15 -> bool))
  ANF: ('_p23 -> ('_p23 -> bool))
  
  Name: ( = )
  Original program: ('_p14 -> ('_p14 -> bool))
  ANF: ('_p22 -> ('_p22 -> bool))
  
  Name: ( >= )
  Original program: ('_p17 -> ('_p17 -> bool))
  ANF: ('_p25 -> ('_p25 -> bool))
  
  Name: ( > )
  Original program: ('_p16 -> ('_p16 -> bool))
  ANF: ('_p24 -> ('_p24 -> bool))
  
  Name: fact_cps
  Original program: (int -> ((int -> '_p18) -> '_p18))
  ANF: (int -> ((int -> '_p26) -> '_p26))
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/001fac.ml
  Name: ( <> )
  Original program: ('_p12 -> ('_p12 -> bool))
  ANF: ('_p19 -> ('_p19 -> bool))
  
  Name: ( / )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( + )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( * )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( - )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( < )
  Original program: ('_p10 -> ('_p10 -> bool))
  ANF: ('_p17 -> ('_p17 -> bool))
  
  Name: ( :: )
  Original program: ('_pf -> (('_pf list) -> ('_pf list)))
  ANF: ('_p16 -> (('_p16 list) -> ('_p16 list)))
  
  Name: ( <= )
  Original program: ('_p11 -> ('_p11 -> bool))
  ANF: ('_p18 -> ('_p18 -> bool))
  
  Name: ( >= )
  Original program: ('_p16 -> ('_p16 -> bool))
  ANF: ('_p1d -> ('_p1d -> bool))
  
  Name: ( == )
  Original program: ('_p14 -> ('_p14 -> bool))
  ANF: ('_p1b -> ('_p1b -> bool))
  
  Name: ( = )
  Original program: ('_p13 -> ('_p13 -> bool))
  ANF: ('_p1a -> ('_p1a -> bool))
  
  Name: ( > )
  Original program: ('_p15 -> ('_p15 -> bool))
  ANF: ('_p1c -> ('_p1c -> bool))
  
  Name: fac
  Original program: (int -> int)
  ANF: (int -> int)
  
  Name: main
  Original program: int
  ANF: int
  
  All types are correct

  $ dune exec ./anf_types_demo.exe << EOF
  > let nested1 = let nested2 = 5 in 
  > let nested3 = 6 in
  > let nested4 x = x + (fun i -> nested2 + nested3) 8 in nested4 55
  > EOF
  Name: ( <> )
  Original program: ('_pf -> ('_pf -> bool))
  ANF: ('_p1d -> ('_p1d -> bool))
  
  Name: ( / )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( + )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( * )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( - )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( < )
  Original program: ('_pd -> ('_pd -> bool))
  ANF: ('_p1b -> ('_p1b -> bool))
  
  Name: ( :: )
  Original program: ('_pc -> (('_pc list) -> ('_pc list)))
  ANF: ('_p1a -> (('_p1a list) -> ('_p1a list)))
  
  Name: ( <= )
  Original program: ('_pe -> ('_pe -> bool))
  ANF: ('_p1c -> ('_p1c -> bool))
  
  Name: ( == )
  Original program: ('_p11 -> ('_p11 -> bool))
  ANF: ('_p1f -> ('_p1f -> bool))
  
  Name: ( = )
  Original program: ('_p10 -> ('_p10 -> bool))
  ANF: ('_p1e -> ('_p1e -> bool))
  
  Name: ( >= )
  Original program: ('_p13 -> ('_p13 -> bool))
  ANF: ('_p21 -> ('_p21 -> bool))
  
  Name: ( > )
  Original program: ('_p12 -> ('_p12 -> bool))
  ANF: ('_p20 -> ('_p20 -> bool))
  
  Name: nested1
  Original program: int
  ANF: int
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/002fac.ml
  Name: ( <> )
  Original program: ('_p1a -> ('_p1a -> bool))
  ANF: ('_p2c -> ('_p2c -> bool))
  
  Name: ( / )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( + )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( * )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( - )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( < )
  Original program: ('_p18 -> ('_p18 -> bool))
  ANF: ('_p2a -> ('_p2a -> bool))
  
  Name: ( :: )
  Original program: ('_p17 -> (('_p17 list) -> ('_p17 list)))
  ANF: ('_p29 -> (('_p29 list) -> ('_p29 list)))
  
  Name: ( <= )
  Original program: ('_p19 -> ('_p19 -> bool))
  ANF: ('_p2b -> ('_p2b -> bool))
  
  Name: ( >= )
  Original program: ('_p1e -> ('_p1e -> bool))
  ANF: ('_p30 -> ('_p30 -> bool))
  
  Name: ( == )
  Original program: ('_p1c -> ('_p1c -> bool))
  ANF: ('_p2e -> ('_p2e -> bool))
  
  Name: ( = )
  Original program: ('_p1b -> ('_p1b -> bool))
  ANF: ('_p2d -> ('_p2d -> bool))
  
  Name: ( > )
  Original program: ('_p1d -> ('_p1d -> bool))
  ANF: ('_p2f -> ('_p2f -> bool))
  
  Name: fac_cps
  Original program: (int -> ((int -> '_p1f) -> '_p1f))
  ANF: (int -> ((int -> '_p31) -> '_p31))
  
  Name: main
  Original program: int
  ANF: int
  
  All types are correct

  $ dune exec ./anf_types_demo.exe << EOF
  > let rec fib_cps n cont =
  > if (n = 0) then
  >  cont 0
  > else if (n = 1) then
  >   cont 1
  > else
  >   fib_cps (n - 1) (fun a ->
  >   fib_cps (n - 2) (fun b ->
  >   cont (a + b)))
  > EOF
  Name: ( <> )
  Original program: ('_p1d -> ('_p1d -> bool))
  ANF: ('_p3a -> ('_p3a -> bool))
  
  Name: ( / )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( + )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( * )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( - )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( < )
  Original program: ('_p1b -> ('_p1b -> bool))
  ANF: ('_p38 -> ('_p38 -> bool))
  
  Name: ( :: )
  Original program: ('_p1a -> (('_p1a list) -> ('_p1a list)))
  ANF: ('_p37 -> (('_p37 list) -> ('_p37 list)))
  
  Name: ( <= )
  Original program: ('_p1c -> ('_p1c -> bool))
  ANF: ('_p39 -> ('_p39 -> bool))
  
  Name: ( == )
  Original program: ('_p1f -> ('_p1f -> bool))
  ANF: ('_p3c -> ('_p3c -> bool))
  
  Name: ( = )
  Original program: ('_p1e -> ('_p1e -> bool))
  ANF: ('_p3b -> ('_p3b -> bool))
  
  Name: ( >= )
  Original program: ('_p21 -> ('_p21 -> bool))
  ANF: ('_p3e -> ('_p3e -> bool))
  
  Name: ( > )
  Original program: ('_p20 -> ('_p20 -> bool))
  ANF: ('_p3d -> ('_p3d -> bool))
  
  Name: fib_cps
  Original program: (int -> ((int -> '_p22) -> '_p22))
  ANF: (int -> ((int -> '_p3f) -> '_p3f))
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/003fib.ml
  Name: ( <> )
  Original program: ('_p2b -> ('_p2b -> bool))
  ANF: ('_p3b -> ('_p3b -> bool))
  
  Name: ( / )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( + )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( * )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( - )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( < )
  Original program: ('_p29 -> ('_p29 -> bool))
  ANF: ('_p39 -> ('_p39 -> bool))
  
  Name: ( :: )
  Original program: ('_p28 -> (('_p28 list) -> ('_p28 list)))
  ANF: ('_p38 -> (('_p38 list) -> ('_p38 list)))
  
  Name: ( <= )
  Original program: ('_p2a -> ('_p2a -> bool))
  ANF: ('_p3a -> ('_p3a -> bool))
  
  Name: ( >= )
  Original program: ('_p2f -> ('_p2f -> bool))
  ANF: ('_p3f -> ('_p3f -> bool))
  
  Name: ( == )
  Original program: ('_p2d -> ('_p2d -> bool))
  ANF: ('_p3d -> ('_p3d -> bool))
  
  Name: ( = )
  Original program: ('_p2c -> ('_p2c -> bool))
  ANF: ('_p3c -> ('_p3c -> bool))
  
  Name: ( > )
  Original program: ('_p2e -> ('_p2e -> bool))
  ANF: ('_p3e -> ('_p3e -> bool))
  
  Name: fib_acc
  Original program: (int -> (int -> (int -> int)))
  ANF: (int -> (int -> (int -> int)))
  
  Name: fib
  Original program: (int -> int)
  ANF: (int -> int)
  
  Name: main
  Original program: int
  ANF: int
  
  All types are correct

  $ dune exec ./anf_types_demo.exe << EOF
  > let test = let test5   = (5 + 5, 6 + 6, 7 + 7 * 8) in test5
  > EOF
  Name: ( <> )
  Original program: ('_pd -> ('_pd -> bool))
  ANF: ('_p12 -> ('_p12 -> bool))
  
  Name: ( / )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( + )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( * )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( - )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( < )
  Original program: ('_pb -> ('_pb -> bool))
  ANF: ('_p10 -> ('_p10 -> bool))
  
  Name: ( :: )
  Original program: ('_pa -> (('_pa list) -> ('_pa list)))
  ANF: ('_pf -> (('_pf list) -> ('_pf list)))
  
  Name: ( <= )
  Original program: ('_pc -> ('_pc -> bool))
  ANF: ('_p11 -> ('_p11 -> bool))
  
  Name: ( == )
  Original program: ('_pf -> ('_pf -> bool))
  ANF: ('_p14 -> ('_p14 -> bool))
  
  Name: ( = )
  Original program: ('_pe -> ('_pe -> bool))
  ANF: ('_p13 -> ('_p13 -> bool))
  
  Name: ( >= )
  Original program: ('_p11 -> ('_p11 -> bool))
  ANF: ('_p16 -> ('_p16 -> bool))
  
  Name: ( > )
  Original program: ('_p10 -> ('_p10 -> bool))
  ANF: ('_p15 -> ('_p15 -> bool))
  
  Name: test
  Original program: (int * int * int)
  ANF: (int * int * int)
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/004manyargs.ml
  Name: ( <> )
  Original program: ('_p46 -> ('_p46 -> bool))
  ANF: ('_p57 -> ('_p57 -> bool))
  
  Name: ( / )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( + )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( * )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( - )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( < )
  Original program: ('_p44 -> ('_p44 -> bool))
  ANF: ('_p55 -> ('_p55 -> bool))
  
  Name: ( :: )
  Original program: ('_p43 -> (('_p43 list) -> ('_p43 list)))
  ANF: ('_p54 -> (('_p54 list) -> ('_p54 list)))
  
  Name: ( <= )
  Original program: ('_p45 -> ('_p45 -> bool))
  ANF: ('_p56 -> ('_p56 -> bool))
  
  Name: ( >= )
  Original program: ('_p4a -> ('_p4a -> bool))
  ANF: ('_p5b -> ('_p5b -> bool))
  
  Name: ( == )
  Original program: ('_p48 -> ('_p48 -> bool))
  ANF: ('_p59 -> ('_p59 -> bool))
  
  Name: ( = )
  Original program: ('_p47 -> ('_p47 -> bool))
  ANF: ('_p58 -> ('_p58 -> bool))
  
  Name: ( > )
  Original program: ('_p49 -> ('_p49 -> bool))
  ANF: ('_p5a -> ('_p5a -> bool))
  
  Name: main
  Original program: int
  ANF: int
  
  Name: test10
  Original program: (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> int))))))))))
  ANF: (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> int))))))))))
  
  Name: test3
  Original program: (int -> (int -> (int -> int)))
  ANF: (int -> (int -> (int -> int)))
  
  Name: wrap
  Original program: ('_p4b -> '_p4b)
  ANF: ('_p5c -> '_p5c)
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/005fix.ml
  Name: ( <> )
  Original program: ('_p1c -> ('_p1c -> bool))
  ANF: ('_p25 -> ('_p25 -> bool))
  
  Name: ( / )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( + )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( * )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( - )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( < )
  Original program: ('_p1a -> ('_p1a -> bool))
  ANF: ('_p23 -> ('_p23 -> bool))
  
  Name: ( :: )
  Original program: ('_p19 -> (('_p19 list) -> ('_p19 list)))
  ANF: ('_p22 -> (('_p22 list) -> ('_p22 list)))
  
  Name: ( <= )
  Original program: ('_p1b -> ('_p1b -> bool))
  ANF: ('_p24 -> ('_p24 -> bool))
  
  Name: ( >= )
  Original program: ('_p20 -> ('_p20 -> bool))
  ANF: ('_p29 -> ('_p29 -> bool))
  
  Name: ( == )
  Original program: ('_p1e -> ('_p1e -> bool))
  ANF: ('_p27 -> ('_p27 -> bool))
  
  Name: ( = )
  Original program: ('_p1d -> ('_p1d -> bool))
  ANF: ('_p26 -> ('_p26 -> bool))
  
  Name: ( > )
  Original program: ('_p1f -> ('_p1f -> bool))
  ANF: ('_p28 -> ('_p28 -> bool))
  
  Name: fix
  Original program: ((('_p21 -> '_p22) -> ('_p21 -> '_p22)) -> ('_p21 -> '_p22))
  ANF: ((('_p2a -> '_p2b) -> ('_p2a -> '_p2b)) -> ('_p2a -> '_p2b))
  
  Name: fac
  Original program: ((int -> int) -> (int -> int))
  ANF: ((int -> int) -> (int -> int))
  
  Name: main
  Original program: int
  ANF: int
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/006partial.ml
  Name: ( <> )
  Original program: ('_p1a -> ('_p1a -> bool))
  ANF: ('_p25 -> ('_p25 -> bool))
  
  Name: ( / )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( + )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( * )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( - )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( < )
  Original program: ('_p18 -> ('_p18 -> bool))
  ANF: ('_p23 -> ('_p23 -> bool))
  
  Name: ( :: )
  Original program: ('_p17 -> (('_p17 list) -> ('_p17 list)))
  ANF: ('_p22 -> (('_p22 list) -> ('_p22 list)))
  
  Name: ( <= )
  Original program: ('_p19 -> ('_p19 -> bool))
  ANF: ('_p24 -> ('_p24 -> bool))
  
  Name: ( >= )
  Original program: ('_p1e -> ('_p1e -> bool))
  ANF: ('_p29 -> ('_p29 -> bool))
  
  Name: ( == )
  Original program: ('_p1c -> ('_p1c -> bool))
  ANF: ('_p27 -> ('_p27 -> bool))
  
  Name: ( = )
  Original program: ('_p1b -> ('_p1b -> bool))
  ANF: ('_p26 -> ('_p26 -> bool))
  
  Name: ( > )
  Original program: ('_p1d -> ('_p1d -> bool))
  ANF: ('_p28 -> ('_p28 -> bool))
  
  Name: foo
  Original program: (int -> int)
  ANF: (int -> int)
  
  Name: main
  Original program: int
  ANF: int
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/006partial2.ml
  Name: ( <> )
  Original program: ('_p1a -> ('_p1a -> bool))
  ANF: ('_p23 -> ('_p23 -> bool))
  
  Name: ( / )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( + )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( * )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( - )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( < )
  Original program: ('_p18 -> ('_p18 -> bool))
  ANF: ('_p21 -> ('_p21 -> bool))
  
  Name: ( :: )
  Original program: ('_p17 -> (('_p17 list) -> ('_p17 list)))
  ANF: ('_p20 -> (('_p20 list) -> ('_p20 list)))
  
  Name: ( <= )
  Original program: ('_p19 -> ('_p19 -> bool))
  ANF: ('_p22 -> ('_p22 -> bool))
  
  Name: ( >= )
  Original program: ('_p1e -> ('_p1e -> bool))
  ANF: ('_p27 -> ('_p27 -> bool))
  
  Name: ( == )
  Original program: ('_p1c -> ('_p1c -> bool))
  ANF: ('_p25 -> ('_p25 -> bool))
  
  Name: ( = )
  Original program: ('_p1b -> ('_p1b -> bool))
  ANF: ('_p24 -> ('_p24 -> bool))
  
  Name: ( > )
  Original program: ('_p1d -> ('_p1d -> bool))
  ANF: ('_p26 -> ('_p26 -> bool))
  
  Name: foo
  Original program: (int -> (int -> (int -> int)))
  ANF: (int -> (int -> (int -> int)))
  
  Name: main
  Original program: int
  ANF: int
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/006partial3.ml
  Name: ( <> )
  Original program: ('_p11 -> ('_p11 -> bool))
  ANF: ('_p20 -> ('_p20 -> bool))
  
  Name: ( / )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( + )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( * )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( - )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( < )
  Original program: ('_pf -> ('_pf -> bool))
  ANF: ('_p1e -> ('_p1e -> bool))
  
  Name: ( :: )
  Original program: ('_pe -> (('_pe list) -> ('_pe list)))
  ANF: ('_p1d -> (('_p1d list) -> ('_p1d list)))
  
  Name: ( <= )
  Original program: ('_p10 -> ('_p10 -> bool))
  ANF: ('_p1f -> ('_p1f -> bool))
  
  Name: ( >= )
  Original program: ('_p15 -> ('_p15 -> bool))
  ANF: ('_p24 -> ('_p24 -> bool))
  
  Name: ( == )
  Original program: ('_p13 -> ('_p13 -> bool))
  ANF: ('_p22 -> ('_p22 -> bool))
  
  Name: ( = )
  Original program: ('_p12 -> ('_p12 -> bool))
  ANF: ('_p21 -> ('_p21 -> bool))
  
  Name: ( > )
  Original program: ('_p14 -> ('_p14 -> bool))
  ANF: ('_p23 -> ('_p23 -> bool))
  
  Name: foo
  Original program: (int -> (int -> (int -> unit)))
  ANF: (int -> (int -> (int -> unit)))
  
  Name: main
  Original program: int
  ANF: int
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/009let_poly.ml
  Name: ( <> )
  Original program: ('_pa -> ('_pa -> bool))
  ANF: ('_pd -> ('_pd -> bool))
  
  Name: ( / )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( + )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( * )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( - )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( < )
  Original program: ('_p8 -> ('_p8 -> bool))
  ANF: ('_pb -> ('_pb -> bool))
  
  Name: ( :: )
  Original program: ('_p7 -> (('_p7 list) -> ('_p7 list)))
  ANF: ('_pa -> (('_pa list) -> ('_pa list)))
  
  Name: ( <= )
  Original program: ('_p9 -> ('_p9 -> bool))
  ANF: ('_pc -> ('_pc -> bool))
  
  Name: ( == )
  Original program: ('_pc -> ('_pc -> bool))
  ANF: ('_pf -> ('_pf -> bool))
  
  Name: ( = )
  Original program: ('_pb -> ('_pb -> bool))
  ANF: ('_pe -> ('_pe -> bool))
  
  Name: ( >= )
  Original program: ('_pe -> ('_pe -> bool))
  ANF: ('_p11 -> ('_p11 -> bool))
  
  Name: ( > )
  Original program: ('_pd -> ('_pd -> bool))
  ANF: ('_p10 -> ('_p10 -> bool))
  
  Name: temp
  Original program: (int * bool)
  ANF: (int * bool)
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/016lists.ml
  Name: ( <> )
  Original program: ('_pdd -> ('_pdd -> bool))
  ANF: ('_p116 -> ('_p116 -> bool))
  
  Name: ( / )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( + )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( * )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( - )
  Original program: (int -> (int -> int))
  ANF: (int -> (int -> int))
  
  Name: ( < )
  Original program: ('_pdb -> ('_pdb -> bool))
  ANF: ('_p114 -> ('_p114 -> bool))
  
  Name: ( :: )
  Original program: ('_pda -> (('_pda list) -> ('_pda list)))
  ANF: ('_p113 -> (('_p113 list) -> ('_p113 list)))
  
  Name: ( <= )
  Original program: ('_pdc -> ('_pdc -> bool))
  ANF: ('_p115 -> ('_p115 -> bool))
  
  Name: iter
  Original program: (('_pe6 -> unit) -> (('_pe6 list) -> unit))
  ANF: (('_p11f -> unit) -> (('_p11f list) -> unit))
  
  Name: ( >= )
  Original program: ('_pe1 -> ('_pe1 -> bool))
  ANF: ('_p11a -> ('_p11a -> bool))
  
  Name: ( == )
  Original program: ('_pdf -> ('_pdf -> bool))
  ANF: ('_p118 -> ('_p118 -> bool))
  
  Name: ( = )
  Original program: ('_pde -> ('_pde -> bool))
  ANF: ('_p117 -> ('_p117 -> bool))
  
  Name: ( > )
  Original program: ('_pe0 -> ('_pe0 -> bool))
  ANF: ('_p119 -> ('_p119 -> bool))
  
  Name: cartesian
  Original program: (('_pe3 list) -> (('_pe4 list) -> (('_pe3 * '_pe4) list)))
  ANF: (('_p11c list) -> (('_p11d list) -> (('_p11c * '_p11d) list)))
  
  Name: append
  Original program: (('_pe2 list) -> (('_pe2 list) -> ('_pe2 list)))
  ANF: (('_p11b list) -> (('_p11b list) -> ('_p11b list)))
  
  Name: concat
  Original program: ((('_pe5 list) list) -> ('_pe5 list))
  ANF: ((('_p11e list) list) -> ('_p11e list))
  
  Name: length_tail
  Original program: (('_pe8 list) -> int)
  ANF: (('_p121 list) -> int)
  
  Name: length
  Original program: (('_pe7 list) -> int)
  ANF: (('_p120 list) -> int)
  
  Name: main
  Original program: int
  ANF: int
  
  Name: map
  Original program: (('_pe9 -> '_pea) -> (('_pe9 list) -> ('_pea list)))
  ANF: (('_p126 -> '_p127) -> (('_p126 list) -> ('_p127 list)))
  
  All types are correct
