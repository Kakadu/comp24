  $ dune exec ./anf_types_demo.exe << EOF
  > let rec fact_cps n cont =
  >   if (n = 0) then
  >    cont 1
  >   else
  >    fact_cps (n - 1) (fun acc -> cont (n * acc))
  Name: fact_cps
  Original program: (int -> ((int -> 'p21) -> 'p21))
  ANF: (int -> ((int -> 'p2f) -> 'p2f))
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/001fac.ml
  Name: main
  Original program: int
  ANF: int
  
  Name: fac
  Original program: (int -> int)
  ANF: (int -> int)
  
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
  Name: main
  Original program: int
  ANF: int
  
  Name: fac_cps
  Original program: (int -> ((int -> 'p28) -> 'p28))
  ANF: (int -> ((int -> 'p3a) -> 'p3a))
  
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
  Original program: (int -> ((int -> 'p2b) -> 'p2b))
  ANF: (int -> ((int -> 'p48) -> 'p48))
  
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
  Original program: ('p57 -> 'p57)
  ANF: ('p68 -> 'p68)
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/005fix.ml
  Name: fac
  Original program: ((int -> int) -> (int -> int))
  ANF: ((int -> int) -> (int -> int))
  
  Name: fix
  Original program: ((('p2a -> 'p2b) -> ('p2a -> 'p2b)) -> ('p2a -> 'p2b))
  ANF: ((('p33 -> 'p34) -> ('p33 -> 'p34)) -> ('p33 -> 'p34))
  
  Name: main
  Original program: int
  ANF: int
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/006partial.ml
  Name: main
  Original program: int
  ANF: int
  
  Name: foo
  Original program: (int -> int)
  ANF: (int -> int)
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/006partial2.ml
  Name: main
  Original program: int
  ANF: int
  
  Name: foo
  Original program: (int -> (int -> (int -> int)))
  ANF: (int -> (int -> (int -> int)))
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/006partial3.ml
  Name: main
  Original program: int
  ANF: int
  
  Name: foo
  Original program: (int -> (int -> (int -> unit)))
  ANF: (int -> (int -> (int -> unit)))
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/009let_poly.ml
  Name: temp
  Original program: (int * bool)
  ANF: (int * bool)
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/016lists.ml
  Name: concat
  Original program: ((('pe7 list) list) -> ('pe7 list))
  ANF: (('p31a list) -> ('p31b list))
  
  Name: append
  Original program: (('pe3 list) -> (('pe3 list) -> ('pe3 list)))
  ANF: (('p314 list) -> (('p315 list) -> ('p315 list)))
  
  Name: cartesian
  Original program: (('pe4 list) -> (('pe5 list) -> (('pe4 * 'pe5) list)))
  ANF: (('p316 list) -> (('p317 list) -> ('p318 list)))
  
  Name: length_tail
  Original program: (('pec list) -> int)
  ANF: (('p322 list) -> int)
  
  Name: iter
  Original program: (('pea -> unit) -> (('pea list) -> unit))
  ANF: (('p31f -> 'p320) -> (('p31e list) -> unit))
  
  Name: length
  Original program: (('peb list) -> int)
  ANF: (('p321 list) -> int)
  
  Name: map
  Original program: (('ped -> 'pee) -> (('ped list) -> ('pee list)))
  ANF: (('p328 -> 'p329) -> (('p32a list) -> ('p329 list)))
  
  Name: main
  Original program: int
  ANF: int
  
  All types are correct
