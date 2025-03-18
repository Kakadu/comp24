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
