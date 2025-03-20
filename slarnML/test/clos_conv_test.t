  $ dune exec clos_conv_test << EOF
  > let fac n = 
  >   let rec fack n f = if (n <= 1) then (f 1) else (fack (n - 1) (fun x -> x * (f n))) in
  >   (fack n (fun x -> x))
  > ;;
  > EOF
  (let fac n=(let rec fack n f=if ((n<=1)) then (((f ) 1)) else (((fack ) (n-1) ((fun n f x->(x*((f ) n))) n f))) in ((fack ) n ((fun x->x) ))))
  $ dune exec clos_conv_test << EOF
  > let fac n = 
  >   let rec fack n = if (n < 1) then n else n * (fack (n - 1)) in
  >   (fack n)
  > ;;
  > EOF
  (let fac n=(let rec fack n=if ((n<1)) then (n) else ((n*((fack ) (n-1)))) in ((fack ) n)))
  $ dune exec clos_conv_test << EOF
  > let f a =
  >   let g c d =
  >     let h e = a * (c + d * e) in
  >     (h 4)
  >   in
  >   (g 2 3)
  > EOF
  (let f a=(let g a c d=(let h a c d e=(a*(c+(d*e))) in ((h a c d) 4)) in ((g a) 2 3)))
  $ dune exec clos_conv_test << EOF
  > let rec fac n = if n<=1 then 1 else n * fac (n-1)
  > 
  > let main =
  >   let () = print_int (fac 4) in
  >   0
  > EOF
  (let rec fac n=if ((n<=1)) then (1) else ((n*((fac ) (n-1)))))
  (let main=(let ()=((print_int ) ((fac ) 4)) in 0))
  $ dune exec clos_conv_test < manytests/typed/001fac.ml
  (let rec fac n=if ((n<=1)) then (1) else ((n*((fac ) (n-1)))))
  (let main=(let ()=((print_int ) ((fac ) 4)) in 0))
  $ dune exec clos_conv_test < manytests/typed/002fac.ml
  (let rec fac_cps n k=if ((n=1)) then (((k ) 1)) else (((fac_cps ) (n-1) ((fun n k p->((k ) (p*n))) n k))))
  (let main=(let ()=((print_int ) ((fac_cps ) 4 ((fun print_int->print_int) ))) in 0))
  $ dune exec clos_conv_test < manytests/typed/003fib.ml
  : end_of_input
  $ dune exec clos_conv_test < manytests/typed/004manyargs.ml
  (let wrap f=if ((1=1)) then (f) else (f))
  (let test3 a b c=(let a a=((print_int ) a) in (let b b=((print_int ) b) in (let c c=((print_int ) c) in 0))))
  (let test10 a b c d e f g h i j=(((((((((a+b)+c)+d)+e)+f)+g)+h)+i)+j))
  (let main=(let temp0=((wrap ) test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000) in (let temp1 temp0=((print_int ) temp0) in (let temp2=((wrap ) test3 1 10 100) in 0))))
  $ dune exec clos_conv_test < manytests/typed/005fix.ml
  (let rec fix f x=((f ) ((fix ) f) x))
  (let fac self n=if ((n<=1)) then (1) else ((n*((self ) (n-1)))))
  (let main=(let ()=((print_int ) ((fix ) fac 6)) in 0))
  $ dune exec clos_conv_test < manytests/typed/006partial.ml
  (let foo b=if (b) then (((fun foo->(foo+2)) )) else (((fun foo->(foo*10)) )))
  (let foo x=((foo ) true ((foo ) false ((foo ) true ((foo ) false x)))))
  (let main=(let ()=((print_int ) ((foo ) 11)) in 0))
  $ dune exec clos_conv_test < manytests/typed/006partial2.ml
  (let foo a b c=(let ()=((print_int ) a) in (let ()=((print_int ) b) in (let ()=((print_int ) c) in (a+(b*c))))))
  (let main=(let foo=((foo ) 1) in (let foo foo=((foo foo) 2) in (let foo foo=((foo foo) 3) in (let ()=((print_int ) foo) in 0)))))
  $ dune exec clos_conv_test < manytests/typed/006partial3.ml
  (let foo a=(let ()=((print_int ) a) in ((fun b->(let ()=((print_int ) b) in ((fun c->((print_int ) c)) ))) )))
  (let main=(let ()=((foo ) 4 8 9) in 0))
  $ dune exec clos_conv_test < manytests/typed/007order.ml
  : end_of_input
  $ dune exec clos_conv_test < manytests/typed/008ascription.ml
  : end_of_input
  $ dune exec clos_conv_test < manytests/typed/015tuples.ml
  : end_of_input
  $ dune exec clos_conv_test < manytests/typed/016lists.ml
  : end_of_input
