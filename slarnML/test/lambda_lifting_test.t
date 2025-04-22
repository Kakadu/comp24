  $ dune exec lambda_lifting_test << EOF
  > let fac n =
  >   let rec fack n k = if (n<=1) 
  >     then n (n-1)
  >     else (fun m -> k * m*n)
  >   in fack n (fun x -> x)
  > ;;
  > EOF
  let anon_1 n k n m =(((k*m)*n))
  let fack n k =(if ((n<=1)) then (n (n-1)) else (anon_1 n k n))
  let anon_2 n x =(x)
  let fac n =(fack n anon_2 n)
  $ dune exec lambda_lifting_test << EOF
  > let f a=
  >   let g c d =
  >     let h e = a*(c+d*e) in 
  >     h 4 in 
  >     g 2 3
  > ;;
  > EOF
  let h c d a e =((a*(c+(d*e))))
  let g a c d =(h c d a 4)
  let f a =(g a 2 3)
  $ dune exec lambda_lifting_test << EOF
  > let f a b =
  >   let g c =
  >     let h = (fun x -> x*(a (c*b))) in 
  >     h a in
  >     g 3
  >  ;;
  > EOF
  let anon_1 c a b x =((x*a (c*b)))
  let h c a b =(anon_1 c a b)
  let g a b c =(h c a b a)
  let f a b =(g a b 3)
  $ dune exec lambda_lifting_test << EOF
  > let f a =
  >   let g a b=
  >     let h b c= a*(b/c) in 
  >     h 2 3 in 
  >     g (1+0) a
  > ;;
  > EOF
  let h a a b c =((a*(b/c)))
  let g a b =(h a a 2 3)
  let f a =(g (1+0) a)
  $ dune exec lambda_lifting_test << EOF
  > let f a =
  >   let g b = a / b in 
  >   let h c = (a * c) in 
  >   ((h 1) + (g 2))
  > ;;
  > EOF
  let g a b =((a/b))
  let h a c =((a*c))
  let f a =((h a 1+g a 2))
  $ dune exec lambda_lifting_test << EOF
  > let f a =
  >   let g = (fun x -> x) in 
  >   let h = (fun x -> a * x) in 
  >   ((g a) + (h a))
  > ;;
  > EOF
  let anon_1 a x =(x)
  let g a =(anon_1 a)
  let anon_2 a x =((a*x))
  let h a =(anon_2 a)
  let f a =((g a a+h a a))
  $ dune exec lambda_lifting_test << EOF
  > let fac n = 
  >   let rec fack n f = if (n <= 1) then (f 1) else (fack (n - 1) (fun x -> x * (f n))) in
  >   (fack n (fun x -> x))
  > ;;
  > EOF
  let anon_1 n f n x =((x*f n))
  let fack n f =(if ((n<=1)) then (f 1) else (fack (n-1) anon_1 n f n))
  let anon_2 n x =(x)
  let fac n =(fack n anon_2 n)
  $ dune exec lambda_lifting_test << EOF
  > let fac n = 
  >   let rec fack n = if (n < 1) then n else n * (fack (n - 1)) in
  >   (fack n)
  > ;;
  > EOF
  let fack n =(if ((n<1)) then (n) else ((n*fack (n-1))))
  let fac n =(fack n)
  $ dune exec lambda_lifting_test << EOF
  > let f a =
  >   let g c d =
  >     let h e = a * (c + d * e) in
  >     (h 4)
  >   in
  >   (g 2 3)
  > EOF
  let h c d a e =((a*(c+(d*e))))
  let g a c d =(h c d a 4)
  let f a =(g a 2 3)
  $ dune exec lambda_lifting_test < manytests/do_not_type/001.ml
  let recfac n =(if ((n<=1)) then (1) else ((n*fac (n-1))))
  $ dune exec lambda_lifting_test < manytests/do_not_type/002if.ml
  let main  =(if (true) then (1) else (false))
  $ dune exec lambda_lifting_test < manytests/do_not_type/003occurs.ml
  let anon_2 x f =(x x f)
  let anon_1 f x =(f anon_2 x f)
  let fix f =(anon_1 f)
  let anon_4 x f =(x x f)
  let anon_3 x =(f anon_4 x)
  $ dune exec lambda_lifting_test < manytests/typed/001fac.ml
  let fac n =(if ((n<=1)) then (1) else ((n*fac (n-1))))
  let main  =(let () = (print_int fac 4 in 0))
  $ dune exec lambda_lifting_test < manytests/typed/002fac.ml
  let anon_1 n k p =(k (p*n))
  let fac_cps n k =(if ((n=1)) then (k 1) else (fac_cps (n-1) anon_1 n k))
  let anon_2 print_int =(print_int)
  let main  =(let () = (print_int fac_cps 4 anon_2  in 0))
  $ dune exec lambda_lifting_test < manytests/typed/003fib.ml
  let n1 a b n =((n-1))
  let ab a b n =((a+b))
  let fib_acc a b n =(if ((n=1)) then (b) else (fib_acc b ab a b n n1 a b n))
  let fib n =(if ((n<2)) then (n) else (fib ((n-1)+fib (n-2))))
  let main  =(let () = (print_int fib_acc 0 1 4 in let () = (print_int fib 4 in 0)))
  $ dune exec lambda_lifting_test < manytests/typed/004manyargs.ml
  let wrap f =(if ((1=1)) then (f) else (f))
  let a_0 a b c =(print_int a)
  let b_0 a b c =(print_int b)
  let c_0 a b c =(print_int c)
  let test3 a b c =(0)
  let test10 a b c d e f g h i j =((((((((((a+b)+c)+d)+e)+f)+g)+h)+i)+j))
  let main  =(let rez = (wrap test10  1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000 in let () = (print_int rez in let temp2 = (wrap test3  1 10 100 in 0))))
  $ dune exec lambda_lifting_test < manytests/typed/005fix.ml
  let fix f x =(f fix f x)
  let fac self n =(if ((n<=1)) then (1) else ((n*self (n-1))))
  let main  =(let () = (print_int fix fac  6 in 0))
  $ dune exec lambda_lifting_test < manytests/typed/006partial.ml
  let anon_1 b foo =((foo+2))
  let anon_2 b foo =((foo*10))
  let foo b =(if (b) then (anon_1 b) else (anon_2 b))
  let foo_0 x =(foo true foo false foo true foo false x)
  let main  =(let () = (print_int foo_0 11 in 0))
  $ dune exec lambda_lifting_test < manytests/typed/006partial2.ml
  let foo a b c =(let () = (print_int a in let () = (print_int b in let () = (print_int c in (a+(b*c))))))
  let main  =(let foo_0 = (foo 1 in let foo_0_2 = (foo_0 2 in let foo_0_2_4 = (foo_0_2 3 in let () = (print_int foo_0_2_4 in 0)))))
  $ dune exec lambda_lifting_test < manytests/typed/006partial3.ml
  let anon_2 b a c =(print_int c)
  let anon_1 a b =(let () = (print_int b in anon_2 b a))
  let foo a =(let () = (print_int a in anon_1 a))
  let main  =(let () = (foo 4 8 9 in 0))
  $ dune exec lambda_lifting_test << EOF
  > let f a =
  >   let g c d =
  >     let h e = a * (c + d * e) in
  >     (h 4)
  >   in
  >   (g 2 3)
  > EOF
  let h c d a e =((a*(c+(d*e))))
  let g a c d =(h c d a 4)
  let f a =(g a 2 3)
