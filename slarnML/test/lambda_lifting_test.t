  $ dune exec lambda_lifting_test << EOF
  > let fac n = 
  >   let rec fack n f = if (n <= 1) then (f 1) else (fack (n - 1) (fun x -> x * (f n))) in
  >   (fack n (fun x -> x))
  > ;;
  > EOF
  (fun anon$1#fack#fac(n f x)->((x*(f n))))
  (fun fack#fac(n f)->(if ((n<=1)) then ((f 1)) else ((fack#fac (n-1) (anon$1#fack#fac n f)))))
  (fun anon$2#fac(x)->(x))
  (fun fac(n)->((fack#fac n (anon$2#fac ))))
  $ dune exec lambda_lifting_test << EOF
  > let fac n = 
  >   let rec fack n = if (n < 1) then n else n * (fack (n - 1)) in
  >   (fack n)
  > ;;
  > EOF
  (fun fack#fac(n)->(if ((n<1)) then (n) else ((n*(fack#fac (n-1))))))
  (fun fac(n)->((fack#fac n)))
  $ dune exec lambda_lifting_test << EOF
  > let f a =
  >   let g c d =
  >     let h e = a * (c + d * e) in
  >     (h 4)
  >   in
  >   (g 2 3)
  > EOF
  (fun h#g#f(a c d e)->((a*(c+(d*e)))))
  (fun g#f(a c d)->((h#g#f a c d 4)))
  (fun f(a)->((g#f a 2 3)))
