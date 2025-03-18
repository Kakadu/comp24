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
  $ dune exec lambda_lifting_test < manytests/typed/001fac.ml
  (fun fac(n)->(if ((n<=1)) then (1) else ((n*(fac (n-1))))))
  (fun ()#main()->((print_int (fac 4))))
  (fun main()->(0))
  $ dune exec lambda_lifting_test < manytests/typed/002fac.ml
  (fun anon$1#fac_cps(n k p)->((k (p*n))))
  (fun fac_cps(n k)->(if ((n=1)) then ((k 1)) else ((fac_cps (n-1) (anon$1#fac_cps n k)))))
  (fun anon$1#()#main(print_int)->(print_int))
  (fun ()#main()->((print_int (fac_cps 4 (anon$1#()#main )))))
  (fun main()->(0))
  $ dune exec lambda_lifting_test < manytests/typed/003fib.ml
  : end_of_input
  $ dune exec lambda_lifting_test < manytests/typed/004manyargs.ml
  (fun wrap(f)->(if ((1=1)) then (f) else (f)))
  (fun a#test3(a)->((print_int a#test3)))
  (fun b#test3(b)->((print_int b#test3)))
  (fun c#test3(c)->((print_int c#test3)))
  (fun test3(a b c)->(0))
  (fun test10(a b c d e f g h i j)->((((((((((a+b)+c)+d)+e)+f)+g)+h)+i)+j)))
  (fun temp0#main()->((wrap (test10 ) 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000)))
  (fun temp1#main(temp0)->((print_int temp0)))
  (fun temp2#main()->((wrap (test3 ) 1 10 100)))
  (fun main()->(0))
  $ dune exec lambda_lifting_test < manytests/typed/005fix.ml
  (fun fix(f x)->((f (fix f) x)))
  (fun fac(self n)->(if ((n<=1)) then (1) else ((n*(self (n-1))))))
  (fun ()#main()->((print_int (fix (fac ) 6))))
  (fun main()->(0))
  $ dune exec lambda_lifting_test < manytests/typed/006partial.ml
  (fun anon$1#foo(foo)->((foo+2)))
  (fun anon$2#foo(foo)->((foo*10)))
  (fun foo(b)->(if (b) then ((anon$1#foo )) else ((anon$2#foo ))))
  (fun foo(x)->((foo true (foo false (foo true (foo false x))))))
  (fun ()#main()->((print_int (foo 11))))
  (fun main()->(0))
  $ dune exec lambda_lifting_test < manytests/typed/006partial2.ml
  (fun ()#foo(a)->((print_int a)))
  (fun ()#foo(b)->((print_int b)))
  (fun ()#foo(c)->((print_int c)))
  (fun foo(a b c)->((a+(b*c))))
  (fun foo#main()->((foo#main 1)))
  (fun foo#main(foo)->((foo#main foo#main 2)))
  (fun foo#main(foo)->((foo#main foo#main 3)))
  (fun ()#main(foo)->((print_int foo)))
  (fun main()->(0))
  $ dune exec lambda_lifting_test < manytests/typed/006partial3.ml
  (fun ()#foo(a)->((print_int a)))
  (fun ()#anon$1#foo(b)->((print_int b)))
  (fun anon$2#anon$1#foo(c)->((print_int c)))
  (fun anon$1#foo(b)->((anon$2#anon$1#foo )))
  (fun foo(a)->((anon$1#foo )))
  (fun ()#main()->((foo 4 8 9)))
  (fun main()->(0))
  $ dune exec lambda_lifting_test < manytests/typed/007order.ml
  : end_of_input
  $ dune exec lambda_lifting_test < manytests/typed/008ascription.ml
  : end_of_input
  $ dune exec lambda_lifting_test < manytests/typed/015tuples.ml
  : end_of_input
  $ dune exec lambda_lifting_test < manytests/typed/016lists.ml
  : end_of_input
