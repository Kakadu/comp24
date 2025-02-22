  $ dune exec ./anf_types_demo.exe << EOF
  > let rec fact_cps n cont =
  >   if (n = 0) then
  >    cont 1
  >   else
  >    fact_cps (n - 1) (fun acc -> cont (n * acc))
  Name: fact_cps
  Original program: (int -> ((int -> '_p19) -> '_p19))
  ANF: (int -> ((int -> '_p27) -> '_p27))
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/001fac.ml
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
  Name: nested1
  Original program: int
  ANF: int
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/002fac.ml
  Name: fac_cps
  Original program: (int -> ((int -> '_p20) -> '_p20))
  ANF: (int -> ((int -> '_p32) -> '_p32))
  
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
  Name: fib_cps
  Original program: (int -> ((int -> '_p23) -> '_p23))
  ANF: (int -> ((int -> '_p40) -> '_p40))
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/003fib.ml
  Name: fib
  Original program: (int -> int)
  ANF: (int -> int)
  
  Name: fib_acc
  Original program: (int -> (int -> (int -> int)))
  ANF: (int -> (int -> (int -> int)))
  
  Name: main
  Original program: int
  ANF: int
  
  All types are correct

  $ dune exec ./anf_types_demo.exe << EOF
  > let test = let test5   = (5 + 5, 6 + 6, 7 + 7 * 8) in test5
  > EOF
  Name: test
  Original program: (int * int * int)
  ANF: (int * int * int)
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/004manyargs.ml
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
  Original program: ('_p4c -> '_p4c)
  ANF: ('_p5d -> '_p5d)
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/005fix.ml
  Name: fac
  Original program: ((int -> int) -> (int -> int))
  ANF: ((int -> int) -> (int -> int))
  
  Name: fix
  Original program: ((('_p22 -> '_p23) -> ('_p22 -> '_p23)) -> ('_p22 -> '_p23))
  ANF: ((('_p2b -> '_p2c) -> ('_p2b -> '_p2c)) -> ('_p2b -> '_p2c))
  
  Name: main
  Original program: int
  ANF: int
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/006partial.ml
  Name: foo
  Original program: (int -> int)
  ANF: (int -> int)
  
  Name: main
  Original program: int
  ANF: int
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/006partial2.ml
  Name: foo
  Original program: (int -> (int -> (int -> int)))
  ANF: (int -> (int -> (int -> int)))
  
  Name: main
  Original program: int
  ANF: int
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/006partial3.ml
  Name: foo
  Original program: (int -> (int -> (int -> unit)))
  ANF: (int -> (int -> (int -> unit)))
  
  Name: main
  Original program: int
  ANF: int
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/009let_poly.ml
  Name: temp
  Original program: (int * bool)
  ANF: (int * bool)
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/016lists.ml
  Name: append
  Original program: (('_pdc list) -> (('_pdc list) -> ('_pdc list)))
  ANF: (('_p11c list) -> (('_p11c list) -> ('_p11c list)))
  
  Name: length
  Original program: (('_pe1 list) -> int)
  ANF: (('_p121 list) -> int)
  
  Name: concat
  Original program: ((('_pdf list) list) -> ('_pdf list))
  ANF: ((('_p11f list) list) -> ('_p11f list))
  
  Name: cartesian
  Original program: (('_pdd list) -> (('_pde list) -> (('_pdd * '_pde) list)))
  ANF: (('_p11d list) -> (('_p11e list) -> (('_p11d * '_p11e) list)))
  
  Name: iter
  Original program: (('_pe0 -> unit) -> (('_pe0 list) -> unit))
  ANF: (('_p120 -> unit) -> (('_p120 list) -> unit))
  
  Name: main
  Original program: int
  ANF: int
  
  Name: length_tail
  Original program: (('_pe2 list) -> int)
  ANF: (('_p122 list) -> int)
  
  Name: map
  Original program: (('_pe3 -> '_pe4) -> (('_pe3 list) -> ('_pe4 list)))
  ANF: (('_p127 -> '_p128) -> (('_p127 list) -> ('_p128 list)))
  
  All types are correct
