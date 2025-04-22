  $ dune exec clos_conv_test << EOF
  > let fac n =
  >   let rec fack n k = if (n<=1) 
  >     then n (n-1)
  >     else (fun m -> k * m*n)
  >   in fack n (fun x -> x)
  > ;;
  > EOF
  let fac n=(let rec fack n k=(if ((n<=1)) then ((n (n-1))) else (((fun n k n m->((k*m)*n)) n k n))) in ((fack n ((fun n x->x) n))))
  $ dune exec clos_conv_test << EOF
  > let f a=
  >   let g c d =
  >     let h e = a*(c+d*e) in 
  >     h 4 in 
  >     g 2 3
  > ;;
  > EOF
  let f a=(let g a c d=(let h c d a e=((a*(c+(d*e)))) in (((h c d a) 4))) in (((g a) 2 3)))
  $ dune exec clos_conv_test << EOF
  > let f a b =
  >   let g c =
  >     let h = (fun x -> x*(a (c*b))) in 
  >     h a in
  >     g 3
  >  ;;
  > EOF
  let f a b=(let g a b c=(let h c a b=(((fun c a b x->(x*(a (c*b)))) c a b)) in (((h c a b) a))) in (((g a b) 3)))
  $ dune exec clos_conv_test << EOF
  > let f a =
  >   let g a b=
  >     let h b c= a*(b/c) in 
  >     h 2 3 in 
  >     g (1+0) a
  > ;;
  > EOF
  let f a=(let g a b=(let h a a b c=((a*(b/c))) in (((h a a) 2 3))) in ((g (1+0) a)))
  $ dune exec clos_conv_test << EOF
  > let f a =
  >   let g b = a / b in 
  >   let h c = (a * c) in 
  >   ((h 1) + (g 2))
  > ;;
  > EOF
  let f a=(let g a b=((a/b)) in (let h a c=((a*c)) in ((((h a) 1)+((g a) 2)))))
  $ dune exec clos_conv_test << EOF
  > let f a =
  >   let g = (fun x -> x) in 
  >   let h = (fun x -> a * x) in 
  >   ((g a) + (h a))
  > ;;
  > EOF
  let f a=(let g a=(((fun a x->x) a)) in (let h a=(((fun a x->(a*x)) a)) in ((((g a) a)+((h a) a)))))
  $ dune exec clos_conv_test << EOF
  > let fac n = 
  >   let rec fack n f = if (n <= 1) then (f 1) else (fack (n - 1) (fun x -> x * (f n))) in
  >   (fack n (fun x -> x))
  > ;;
  > EOF
  let fac n=(let rec fack n f=(if ((n<=1)) then ((f 1)) else ((fack (n-1) ((fun n f n x->(x*(f n))) n f n)))) in ((fack n ((fun n x->x) n))))
  $ dune exec clos_conv_test << EOF
  > let fac n = 
  >   let rec fack n = if (n < 1) then n else n * (fack (n - 1)) in
  >   (fack n)
  > ;;
  > EOF
  let fac n=(let rec fack n=(if ((n<1)) then (n) else ((n*(fack (n-1))))) in ((fack n)))
  $ dune exec clos_conv_test << EOF
  > let f a =
  >   let g c d =
  >     let h e d = a * (c + d * e) in
  >     (h 4)
  >   in
  >   (g 2 3)
  > EOF
  let f a=(let g a c d=(let h c a e d=((a*(c+(d*e)))) in (((h c a) 4))) in (((g a) 2 3)))
  $ dune exec clos_conv_test << EOF
  > let rec fac n = if n<=1 then 1 else n * fac (n-1)
  > 
  > let main =
  >   let () = print_int (fac 4) in
  >   0
  > EOF
  let rec fac n=(if ((n<=1)) then (1) else ((n*(fac (n-1)))))
  let main=(let ()=((print_int (fac 4))) in (0))
  $ dune exec clos_conv_test < manytests/do_not_type/003occurs.ml
  let fix f=(((fun f x->(f ((fun x f->(x x f)) x f))) f))
  ((fun x->(f ((fun x f->(x x f)) x))) )
  $ dune exec clos_conv_test < manytests/typed/001fac.ml
  let rec fac n=(if ((n<=1)) then (1) else ((n*(fac (n-1)))))
  let main=(let ()=((print_int (fac 4))) in (0))
  $ dune exec clos_conv_test < manytests/typed/002fac.ml
  let rec fac_cps n k=(if ((n=1)) then ((k 1)) else ((fac_cps (n-1) ((fun n k p->(k (p*n))) n k))))
  let main=(let ()=((print_int (fac_cps 4 ((fun print_int->print_int) )))) in (0))
  $ dune exec clos_conv_test < manytests/typed/003fib.ml
  let rec fib_acc a b n=(if ((n=1)) then (b) else (let n1 a b n=((n-1)) in (let ab a b n=((a+b)) in ((fib_acc b (ab a b n) (n1 a b n))))))
  let rec fib n=(if ((n<2)) then (n) else ((fib ((n-1)+(fib (n-2))))))
  let main=(let ()=((print_int (fib_acc 0 1 4))) in (let ()=((print_int (fib 4))) in (0)))
  $ dune exec clos_conv_test < manytests/typed/004manyargs.ml
  let wrap f=(if ((1=1)) then (f) else (f))
  let test3 a b c=(let a_0 a b c=((print_int a)) in (let b_0 a b c=((print_int b)) in (let c_0 a b c=((print_int c)) in (0))))
  let test10 a b c d e f g h i j=((((((((((a+b)+c)+d)+e)+f)+g)+h)+i)+j))
  let main=(let rez=((wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000)) in (let ()=((print_int rez)) in (let temp2=((wrap test3 1 10 100)) in (0))))
  $ dune exec clos_conv_test < manytests/typed/005fix.ml
  let rec fix f x=((f (fix f) x))
  let fac self n=(if ((n<=1)) then (1) else ((n*(self (n-1)))))
  let main=(let ()=((print_int (fix fac 6))) in (0))
  $ dune exec clos_conv_test < manytests/typed/006partial.ml
  let foo b=(if (b) then (((fun b foo->(foo+2)) b)) else (((fun b foo->(foo*10)) b)))
  let foo_0 x=((foo true (foo false (foo true (foo false x)))))
  let main=(let ()=((print_int (foo_0 11))) in (0))
  $ dune exec clos_conv_test < manytests/typed/006partial2.ml
  let foo a b c=(let ()=((print_int a)) in (let ()=((print_int b)) in (let ()=((print_int c)) in ((a+(b*c))))))
  let main=(let foo_0=((foo 1)) in (let foo_0_2=((foo_0 2)) in (let foo_0_2_4=((foo_0_2 3)) in (let ()=((print_int foo_0_2_4)) in (0)))))
  $ dune exec clos_conv_test < manytests/typed/006partial3.ml
  let foo a=(let ()=((print_int a)) in (((fun a b->let ()=((print_int b)) in (((fun b a c->(print_int c)) b a))) a)))
  let main=(let ()=((foo 4 8 9)) in (0))
  $ dune exec clos_conv_test << EOF
  > let f a =
  >   let g c d =
  >     let h e = a * (c + d * e) in
  >     (h 4)
  >   in
  >   (g 2 3)
  > EOF
  let f a=(let g a c d=(let h c d a e=((a*(c+(d*e)))) in (((h c d a) 4))) in (((g a) 2 3)))
