  $ dune exec ./anf_types_demo.exe << EOF
  > let rec fact_cps n cont =
  >   if (n = 0) then
  >    cont 1
  >   else
  >    fact_cps (n - 1) (fun acc -> cont (n * acc))
  Name: fact_cps
  Original program: (int -> ((int -> '_p18) -> '_p18))
  ANF: (int -> ((int -> '_p26) -> '_p26))
  
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
  Name: fib_cps
  Original program: (int -> ((int -> '_p22) -> '_p22))
  ANF: (int -> ((int -> '_p3f) -> '_p3f))
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/003fib.ml
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
  Original program: ('_p4b -> '_p4b)
  ANF: ('_p5c -> '_p5c)
  
  All types are correct

  $ dune exec ./anf_types_demo.exe < ./manytests/typed/005fix.ml
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
  Original program: (('_pe6 -> unit) -> (('_pe6 list) -> unit))
  ANF: (('_p121 -> unit) -> (('_p121 list) -> unit))
  
  Name: cartesian
  Original program: (('_pe3 list) -> (('_pe4 list) -> (('_pe3 * '_pe4) list)))
  ANF: (('_p11e list) -> (('_p11f list) -> (('_p11e * '_p11f) list)))
  
  Name: append
  Original program: (('_pe2 list) -> (('_pe2 list) -> ('_pe2 list)))
  ANF: (('_p11d list) -> (('_p11d list) -> ('_p11d list)))
  
  Name: concat
  Original program: ((('_pe5 list) list) -> ('_pe5 list))
  ANF: ((('_p120 list) list) -> ('_p120 list))
  
  Name: length_tail
  Original program: (('_pe8 list) -> int)
  ANF: (('_p123 list) -> int)
  
  Name: length
  Original program: (('_pe7 list) -> int)
  ANF: (('_p122 list) -> int)
  
  Name: main
  Original program: int
  ANF: int
  
  Name: map
  Original program: (('_pe9 -> '_pea) -> (('_pe9 list) -> ('_pea list)))
  ANF: (('_p128 -> '_p129) -> (('_p128 list) -> ('_p129 list)))
  
  All types are correct
