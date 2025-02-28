  $ dune exec ./anf_types_demo.exe << EOF
  > let rec fact_cps n cont =
  >   if (n = 0) then
  >    cont 1
  >   else
  >    fact_cps (n - 1) (fun acc -> cont (n * acc))
  Name: fact_cps
  Original program: (int -> ((int -> 'p19) -> 'p19))
  ANF: (int -> ((int -> 'p27) -> 'p27))
  
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
  Original program: (int -> ((int -> 'p20) -> 'p20))
  ANF: (int -> ((int -> 'p32) -> 'p32))
  
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
  Original program: (int -> ((int -> 'p23) -> 'p23))
  ANF: (int -> ((int -> 'p40) -> 'p40))
  
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
  Original program: ('p4c -> 'p4c)
  ANF: ('p5d -> 'p5d)
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/005fix.ml
  Name: fac
  Original program: ((int -> int) -> (int -> int))
  ANF: ((int -> int) -> (int -> int))
  
  Name: fix
  Original program: ((('p22 -> 'p23) -> ('p22 -> 'p23)) -> ('p22 -> 'p23))
  ANF: ((('p2b -> 'p2c) -> ('p2b -> 'p2c)) -> ('p2b -> 'p2c))
  
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
  Original program: (('pdc list) -> (('pdc list) -> ('pdc list)))
  ANF: (('p11c list) -> (('p11c list) -> ('p11c list)))
  
  Name: length
  Original program: (('pe1 list) -> int)
  ANF: (('p121 list) -> int)
  
  Name: concat
  Original program: ((('pdf list) list) -> ('pdf list))
  ANF: ((('p11f list) list) -> ('p11f list))
  
  Name: cartesian
  Original program: (('pdd list) -> (('pde list) -> (('pdd * 'pde) list)))
  ANF: (('p11d list) -> (('p11e list) -> (('p11d * 'p11e) list)))
  
  Name: iter
  Original program: (('pe0 -> unit) -> (('pe0 list) -> unit))
  ANF: (('p120 -> unit) -> (('p120 list) -> unit))
  
  Name: main
  Original program: int
  ANF: int
  
  Name: length_tail
  Original program: (('pe2 list) -> int)
  ANF: (('p122 list) -> int)
  
  Name: map
  Original program: (('pe3 -> 'pe4) -> (('pe3 list) -> ('pe4 list)))
  ANF: (('p127 -> 'p128) -> (('p127 list) -> ('p128 list)))
  
  All types are correct
