  $ dune exec ./anf_types_demo.exe << EOF
  > let rec fact_cps n cont =
  >   if (n = 0) then
  >    cont 1
  >   else
  >    fact_cps (n - 1) (fun acc -> cont (n * acc))
  Name: fact_cps
  Original program: (int -> ((int -> 'p1f) -> 'p1f))
  ANF: (int -> ((int -> 'p32) -> 'p32))
  
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
  Original program: (int -> ((int -> 'p26) -> 'p26))
  ANF: (int -> ((int -> 'p47) -> 'p47))
  
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
  Original program: (int -> ((int -> 'p29) -> 'p29))
  ANF: (int -> ((int -> 'p4f) -> 'p4f))
  
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
  Original program: ('p55 -> 'p55)
  ANF: ('p7d -> 'p7d)
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/005fix.ml
  Name: fac
  Original program: ((int -> int) -> (int -> int))
  ANF: ((int -> int) -> (int -> int))
  
  Name: fix
  Original program: ((('p28 -> 'p29) -> ('p28 -> 'p29)) -> ('p28 -> 'p29))
  ANF: ((('p3e -> 'p3f) -> ('p3e -> 'p3f)) -> ('p3e -> 'p3f))
  
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
  Name: iter
  Original program: (('pe8 -> unit) -> (('pe8 list) -> unit))
  ANF: (('p345 -> unit) -> (('p344 list) -> unit))
  
  Name: cartesian
  Original program: (('pe2 list) -> (('pe3 list) -> (('pe2 * 'pe3) list)))
  ANF: (('p33c list) -> (('p33d list) -> ('p33e list)))
  
  Name: append
  Original program: (('pe1 list) -> (('pe1 list) -> ('pe1 list)))
  ANF: (('p33a list) -> (('p33b list) -> ('p33b list)))
  
  Name: concat
  Original program: ((('pe5 list) list) -> ('pe5 list))
  ANF: (('p340 list) -> ('p341 list))
  
  Name: length_tail
  Original program: (('pea list) -> int)
  ANF: (('p347 list) -> int)
  
  Name: length
  Original program: (('pe9 list) -> int)
  ANF: (('p346 list) -> int)
  
  Name: map
  Original program: (('peb -> 'pec) -> (('peb list) -> ('pec list)))
  ANF: (('p34d -> 'p34e) -> (('p34f list) -> ('p34e list)))
  
  Name: main
  Original program: int
  ANF: int
  
  All types are correct
