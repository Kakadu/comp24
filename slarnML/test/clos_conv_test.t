  $ dune exec clos_conv_test << EOF
  > let fac n = 
  >   let rec fack n f = if (n <= 1) then (f 1) else (fack (n - 1) (fun x -> x * (f n))) in
  >   (fack n (fun x -> x))
  > ;;
  > EOF
  (let fac n=(let rec fack n f=if ((n<=1)) then ((f 1)) else ((fack (n-1) (fun x->(x*(f n))))) in (fack n (fun x->x))))
  $ dune exec clos_conv_test << EOF
  > let fac n = 
  >   let rec fack n = if (n < 1) then n else n * (fack (n - 1)) in
  >   (fack n)
  > ;;
  > EOF
  (let fac n=(let rec fack n=if ((n<1)) then (n) else ((n*(fack (n-1)))) in (fack n)))
  $ dune exec clos_conv_test << EOF
  > let f a =
  >   let g c d =
  >     let h e d = a * (c + d * e) in
  >     (h 4)
  >   in
  >   (g 2 3)
  > EOF
  (let f a=(let g a c d=(let h c a e d=(a*(c+(d*e))) in (h c a 4)) in (g a 2 3)))
  $ dune exec clos_conv_test << EOF
  > let rec fac n = if n<=1 then 1 else n * fac (n-1)
  > 
  > let main =
  >   let () = print_int (fac 4) in
  >   0
  > EOF
  (let rec fac n=if ((n<=1)) then (1) else ((n*(fac (n-1)))))
  (let main=(let ()=(print_int (fac 4)) in 0))
  $ dune exec clos_conv_test < manytests/typed/001fac.ml
  (let rec fac n=if ((n<=1)) then (1) else ((n*(fac (n-1)))))
  (let main=(let ()=(print_int (fac 4)) in 0))
  $ dune exec clos_conv_test < manytests/typed/002fac.ml
  (let rec fac_cps n k=if ((n=1)) then ((k 1)) else ((fac_cps (n-1) (fun p->(k (p*n))))))
  (let main=(let ()=(print_int (fac_cps 4 (fun print_int->print_int))) in 0))
  $ dune exec clos_conv_test < manytests/typed/003fib.ml
  (let rec fib_acc a b n=if ((n=1)) then (b) else ((let n1 a b n=(n-1) in (let ab a b n=(a+b) in (fib_acc b ab n1)))))
  (let rec fib n=if ((n<2)) then (n) else ((fib ((n-1)+(fib (n-2))))))
  (let main=(let ()=(print_int (fib_acc 0 1 4)) in (let ()=(print_int (fib 4)) in 0)))
  $ dune exec clos_conv_test < manytests/typed/004manyargs.ml
  (let wrap f=if ((1=1)) then (f) else (f))
  (let test3 a b c=(let a_0 a b c=(print_int a) in (let b_0 a b c=(print_int b) in (let c_0 a b c=(print_int c) in 0))))
  (let test10 a b c d e f g h i j=(((((((((a+b)+c)+d)+e)+f)+g)+h)+i)+j))
  (let main=(let rez=(wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000) in (let ()=(print_int rez) in (let temp2=(wrap test3 1 10 100) in 0))))
  $ dune exec clos_conv_test < manytests/typed/005fix.ml
  (let rec fix f x=(f (fix f) x))
  (let fac self n=if ((n<=1)) then (1) else ((n*(self (n-1)))))
  (let main=(let ()=(print_int (fix fac 6)) in 0))
  $ dune exec clos_conv_test < manytests/typed/006partial.ml
  (let foo b=if (b) then ((fun b foo->(foo+2))) else ((fun b foo->(foo*10))))
  (let foo_0 x=(foo true (foo false (foo true (foo false x)))))
  (let main=(let ()=(print_int (foo 11)) in 0))
  $ dune exec clos_conv_test < manytests/typed/006partial2.ml
  (let foo a b c=(let () a b c=(print_int a) in (let () a b c=(print_int b) in (let () a b c=(print_int c) in (a+(b*c))))))
  (let main=(let foo_0=(foo 1) in (let foo_1=(foo_0 2) in (let foo_2=(foo_1 3) in (let ()=(print_int foo_2) in 0)))))
  $ dune exec clos_conv_test < manytests/typed/006partial3.ml
  (let foo a=(let () a=(print_int a) in (fun a b->(let () b a=(print_int b) in (fun b a c->(print_int c))))))
  (let main=(let ()=(foo 4 8 9) in 0))
  $ dune exec clos_conv_test < manytests/typed/007order.ml
  : end_of_input
  $ dune exec clos_conv_test < manytests/typed/008ascription.ml
  : end_of_input
  $ dune exec clos_conv_test < manytests/typed/015tuples.ml
  : end_of_input
  $ dune exec clos_conv_test < manytests/typed/016lists.ml
  : end_of_input
