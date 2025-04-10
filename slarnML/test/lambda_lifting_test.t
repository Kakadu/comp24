  $ dune exec lambda_lifting_test << EOF
  > let fac n = 
  >   let rec fack n f = if (n <= 1) then (f 1) else (fack (n - 1) (fun x -> x * (f n))) in
  >   (fack n (fun x -> x))
  > ;;
  > EOF
  (fun anon$1(x)->((x*(f n))))
  (fun fack(n f)->(if ((n<=1)) then ((f 1)) else ((fack (n-1) anon$1))))
  (fun anon$2(x)->(x))
  (fun fac(n)->((fack n anon$2)))
  $ dune exec lambda_lifting_test << EOF
  > let fac n = 
  >   let rec fack n = if (n < 1) then n else n * (fack (n - 1)) in
  >   (fack n)
  > ;;
  > EOF
  (fun fack(n)->(if ((n<1)) then (n) else ((n*(fack (n-1))))))
  (fun fac(n)->((fack n)))
  $ dune exec lambda_lifting_test << EOF
  > let f a =
  >   let g c d =
  >     let h e = a * (c + d * e) in
  >     (h 4)
  >   in
  >   (g 2 3)
  > EOF
  (fun h(c d a e)->(((a )*((c )+((d )*(e ))))))
  (fun g(a c d)->((h c d a 4)))
  (fun f(a)->((g a 2 3)))
  $ dune exec lambda_lifting_test < manytests/do_not_type/001.ml
  Id fac not found in env
  (fun recfac(n)->(if (((n )<=1)) then (1) else (((n )*(fac (n ) ((n )-1))))))
  $ dune exec lambda_lifting_test < manytests/do_not_type/002if.ml
  (fun main()->(if (true) then (1) else (false)))
  $ dune exec lambda_lifting_test < manytests/do_not_type/003occurs.ml
  Id f not found in env
  (fun anon$2(f)->((x f x f)))
  (fun anon$1(f x)->((f anon$2)))
  (fun fix(f)->(anon$1))
  (fun anon$4(f)->((x x f)))
  (fun anon$3(x)->((f x anon$4)))
  $ dune exec lambda_lifting_test < manytests/typed/001fac.ml
  (fun fac(n)->(if ((n<=1)) then (1) else ((n*(fac (n-1))))))
  (fun main()->(let () = ((print_int (fac 4)) in 0)))
  $ dune exec lambda_lifting_test < manytests/typed/002fac.ml
  (fun anon$1(p)->((k (p*n))))
  (fun fac_cps(n k)->(if ((n=1)) then ((k 1)) else ((fac_cps (n-1) anon$1))))
  (fun anon$2(print_int)->(print_int))
  (fun main()->(let () = ((print_int (fac_cps 4 anon$2)) in 0)))
  $ dune exec lambda_lifting_test < manytests/typed/003fib.ml
  (fun n1(a b n)->((n-1)))
  (fun ab(a b n)->((a+b)))
  (fun fib_acc(a b n)->(if ((n=1)) then (b) else ((fib_acc b ab n1))))
  (fun fib(n)->(if ((n<2)) then (n) else ((fib ((n-1)+(fib (n-2)))))))
  (fun main()->(let () = ((print_int (fib_acc 0 1 4)) in let () = ((print_int (fib 4)) in 0))))
  $ dune exec lambda_lifting_test < manytests/typed/004manyargs.ml
  (fun wrap(f)->(if ((1=1)) then ((f )) else ((f ))))
  (fun a_0(a b c)->((print_int (a ))))
  (fun b_0(a b c)->((print_int b)))
  (fun c_0(a b c)->((print_int c)))
  (fun test3(a b c)->(0))
  (fun test10(a b c d e f g h i j)->(((((((((((a )+(b ))+(c ))+(d ))+(e ))+(f ))+(g ))+(h ))+(i ))+(j ))))
  (fun rez()->((wrap (test10 ) 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000)))
  (fun temp2()->((wrap (test3 ) 1 10 100)))
  (fun main()->(let () = ((print_int rez) in 0)))
  $ dune exec lambda_lifting_test < manytests/typed/005fix.ml
  (fun fix(f x)->((f (fix f) x)))
  (fun fac(self n)->(if (((n )<=1)) then (1) else (((n )*(self ((n )-1))))))
  (fun main()->(let () = ((print_int (fix (fac ) 6)) in 0)))
  $ dune exec lambda_lifting_test < manytests/typed/006partial.ml
  (fun anon$1(b foo)->((foo+2)))
  (fun anon$2(b foo)->((foo*10)))
  (fun foo(b)->(if ((b )) then (anon$1) else (anon$2)))
  (fun foo_0(x)->((foo true (foo false (foo true (foo false (x )))))))
  (fun main()->(let () = ((print_int (foo_0 11)) in 0)))
  $ dune exec lambda_lifting_test < manytests/typed/006partial2.ml
  (fun foo(a b c)->(let () = ((print_int (a )) in let () = ((print_int (b )) in let () = ((print_int (c )) in ((a )+((b )*(c ))))))))
  (fun foo_0()->((foo 1)))
  (fun foo_0_1()->((foo_0 2)))
  (fun foo_0_1_2()->((foo_0_1 3)))
  (fun main()->(let () = ((print_int foo_0_1_2) in 0)))
  $ dune exec lambda_lifting_test < manytests/typed/006partial3.ml
  (fun anon$2(b a c)->((print_int c)))
  (fun anon$1(a b)->(let () = ((print_int b) in anon$2)))
  (fun foo(a)->(let () = ((print_int (a )) in anon$1)))
  (fun main()->(let () = ((foo 4 8 9) in 0)))
  $ dune exec lambda_lifting_test < manytests/typed/007order.ml
  : end_of_input
  $ dune exec lambda_lifting_test < manytests/typed/008ascription.ml
  : end_of_input
  $ dune exec lambda_lifting_test < manytests/typed/015tuples.ml
  : end_of_input
  $ dune exec lambda_lifting_test < manytests/typed/016lists.ml
  : end_of_input
