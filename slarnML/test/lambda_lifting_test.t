  $ dune exec lambda_lifting_test << EOF
  > let fac n =
  >   let rec fack n k = if (n<=1) 
  >     then n (n-1)
  >     else (fun m -> k * m*n)
  >   in fack n (fun x -> x)
  > ;;
  > EOF
  (fun anon$1(n k n m)->(((k*m)*n)))
  (fun fack(n k)->(if ((n<=1)) then ({n (n-1)}) else ({{{anon$1 n} k} n})))
  (fun anon$2(n x)->(x))
  (fun fac(n)->(let fack = (if ((n<=1)) then ({n (n-1)}) else ({{{anon$1 n} k} n}) in {{fack n} {anon$2 n}})))
  $ dune exec lambda_lifting_test << EOF
  > let f a=
  >   let g c d =
  >     let h e = a*(c+d*e) in 
  >     h 4 in 
  >     g 2 3
  > ;;
  > EOF
  (fun h(c d a e)->((a*(c+(d*e)))))
  (fun g(a c d)->(let h = ((a*(c+(d*e))) in {{{{h c} d} a} 4})))
  (fun f(a)->(let g = (let h = ((a*(c+(d*e))) in {{{{h c} d} a} 4}) in {{{g a} 2} 3})))
  $ dune exec lambda_lifting_test << EOF
  > let f a b =
  >   let g c =
  >     let h = (fun x -> x*(a (c*b))) in 
  >     h a in
  >     g 3
  >  ;;
  > EOF
  (fun anon$1(c a b x)->((x*{a (c*b)})))
  (fun h(c a b)->({{{anon$1 c} a} b}))
  (fun g(a b c)->(let h = ({{{anon$1 c} a} b} in {{{{h c} a} b} a})))
  (fun f(a b)->(let g = (let h = ({{{anon$1 c} a} b} in {{{{h c} a} b} a}) in {{{g a} b} 3})))
  $ dune exec lambda_lifting_test << EOF
  > let f a =
  >   let g a b=
  >     let h b c= a*(b/c) in 
  >     h 2 3 in 
  >     g (1+0) a
  > ;;
  > EOF
  (fun h(a a b c)->((a*(b/c))))
  (fun g(a b)->(let h = ((a*(b/c)) in {{{{h a} a} 2} 3})))
  (fun f(a)->(let g = (let h = ((a*(b/c)) in {{{{h a} a} 2} 3}) in {{g (1+0)} a})))
  $ dune exec lambda_lifting_test << EOF
  > let f a =
  >   let g b = a / b in 
  >   let h c = (a * c) in 
  >   ((h 1) + (g 2))
  > ;;
  > EOF
  (fun g(a b)->((a/b)))
  (fun h(a c)->((a*c)))
  (fun f(a)->(let g = ((a/b) in let h = ((a*c) in ({{h a} 1}+{{g a} 2})))))
  $ dune exec lambda_lifting_test << EOF
  > let f a =
  >   let g = (fun x -> x) in 
  >   let h = (fun x -> a * x) in 
  >   ((g a) + (h a))
  > ;;
  > EOF
  (fun anon$1(a x)->(x))
  (fun g(a)->({anon$1 a}))
  (fun anon$2(a x)->((a*x)))
  (fun h(a)->({anon$2 a}))
  (fun f(a)->(let g = ({anon$1 a} in let h = ({anon$2 a} in ({{g a} a}+{{h a} a})))))
  $ dune exec lambda_lifting_test << EOF
  > let fac n = 
  >   let rec fack n f = if (n <= 1) then (f 1) else (fack (n - 1) (fun x -> x * (f n))) in
  >   (fack n (fun x -> x))
  > ;;
  > EOF
  (fun anon$1(n f n x)->((x*{f n})))
  (fun fack(n f)->(if ((n<=1)) then ({f 1}) else ({{fack (n-1)} {{{anon$1 n} f} n}})))
  (fun anon$2(n x)->(x))
  (fun fac(n)->(let fack = (if ((n<=1)) then ({f 1}) else ({{fack (n-1)} {{{anon$1 n} f} n}}) in {{fack n} {anon$2 n}})))
  $ dune exec lambda_lifting_test << EOF
  > let fac n = 
  >   let rec fack n = if (n < 1) then n else n * (fack (n - 1)) in
  >   (fack n)
  > ;;
  > EOF
  (fun fack(n)->(if ((n<1)) then (n) else ((n*{fack (n-1)}))))
  (fun fac(n)->(let fack = (if ((n<1)) then (n) else ((n*{fack (n-1)})) in {fack n})))
  $ dune exec lambda_lifting_test << EOF
  > let f a =
  >   let g c d =
  >     let h e = a * (c + d * e) in
  >     (h 4)
  >   in
  >   (g 2 3)
  > EOF
  (fun h(c d a e)->((a*(c+(d*e)))))
  (fun g(a c d)->(let h = ((a*(c+(d*e))) in {{{{h c} d} a} 4})))
  (fun f(a)->(let g = (let h = ((a*(c+(d*e))) in {{{{h c} d} a} 4}) in {{{g a} 2} 3})))
  $ dune exec lambda_lifting_test < manytests/do_not_type/001.ml
  Id fac not found in env
  (fun recfac(n)->(if ((n<=1)) then (1) else ((n*{fac (n-1)}))))
  $ dune exec lambda_lifting_test < manytests/do_not_type/002if.ml
  (fun main()->(if (true) then (1) else (false)))
  $ dune exec lambda_lifting_test < manytests/do_not_type/003occurs.ml
  Id f not found in env
  (fun anon$2(x f)->({{x x} f}))
  (fun anon$1(f x)->({f {{anon$2 x} f}}))
  (fun fix(f)->({anon$1 f}))
  (fun anon$4(x f)->({{x x} f}))
  (fun anon$3(x)->({f {anon$4 x}}))
  $ dune exec lambda_lifting_test < manytests/typed/001fac.ml
  (fun fac(n)->(if ((n<=1)) then (1) else ((n*{fac (n-1)}))))
  (fun main()->(let () = ({print_int {fac 4}} in 0)))
  $ dune exec lambda_lifting_test < manytests/typed/002fac.ml
  (fun anon$1(n k p)->({k (p*n)}))
  (fun fac_cps(n k)->(if ((n=1)) then ({k 1}) else ({{fac_cps (n-1)} {{anon$1 n} k}})))
  (fun anon$2(print_int)->(print_int))
  (fun main()->(let () = ({print_int {{fac_cps 4} {anon$2 }}} in 0)))
  $ dune exec lambda_lifting_test < manytests/typed/003fib.ml
  (fun n1(a b n)->((n-1)))
  (fun ab(a b n)->((a+b)))
  (fun fib_acc(a b n)->(if ((n=1)) then (b) else (let n1 = ((n-1) in let ab = ((a+b) in {{{fib_acc b} {{{ab a} b} n}} {{{n1 a} b} n}})))))
  (fun fib(n)->(if ((n<2)) then (n) else (({fib (n-1)}+{fib (n-2)}))))
  (fun main()->(let () = ({print_int {{{fib_acc 0} 1} 4}} in let () = ({print_int {fib 4}} in 0))))
  $ dune exec lambda_lifting_test < manytests/typed/004manyargs.ml
  (fun wrap(f)->(if ((1=1)) then (f) else (f)))
  (fun a_0(a b c)->({print_int a}))
  (fun b_0(a b c)->({print_int b}))
  (fun c_0(a b c)->({print_int c}))
  (fun test3(a b c)->(let a_0 = ({print_int a} in let b_0 = ({print_int b} in let c_0 = ({print_int c} in 0)))))
  (fun test10(a b c d e f g h i j)->((((((((((a+b)+c)+d)+e)+f)+g)+h)+i)+j)))
  (fun main()->(let rez = ({{{{{{{{{{{wrap {test10 }} 1} 10} 100} 1000} 10000} 100000} 1000000} 10000000} 100000000} 1000000000} in let () = ({print_int rez} in let temp3 = ({{{{wrap {test3 }} 1} 10} 100} in let () = ({print_int temp3} in 0))))))
  $ dune exec lambda_lifting_test < manytests/typed/005fix.ml
  (fun fix(f x)->({{f {fix f}} x}))
  (fun fac(self n)->(if ((n<=1)) then (1) else ((n*{self (n-1)}))))
  (fun main()->(let () = ({print_int {{fix {fac }} 6}} in 0)))
  $ dune exec lambda_lifting_test < manytests/typed/006partial.ml
  (fun anon$1(b foo)->((foo+2)))
  (fun anon$2(b foo)->((foo*10)))
  (fun foo(b)->(if (b) then ({anon$1 b}) else ({anon$2 b})))
  (fun foo_0(x)->({{foo true} {{foo false} {{foo true} {{foo false} x}}}}))
  (fun main()->(let () = ({print_int {foo_0 11}} in 0)))
  $ dune exec lambda_lifting_test < manytests/typed/006partial2.ml
  (fun foo(a b c)->(let () = ({print_int a} in let () = ({print_int b} in let () = ({print_int c} in (a+(b*c)))))))
  (fun main()->(let foo_0 = ({foo 1} in let foo_0_2 = ({foo_0 2} in let foo_0_2_4 = ({foo_0_2 3} in let () = ({print_int foo_0_2_4} in 0))))))
  $ dune exec lambda_lifting_test < manytests/typed/006partial3.ml
  (fun anon$2(b a c)->({print_int c}))
  (fun anon$1(a b)->(let () = ({print_int b} in {{anon$2 b} a})))
  (fun foo(a)->(let () = ({print_int a} in {anon$1 a})))
  (fun main()->(let () = ({{{foo 4} 8} 9} in 0)))
  $ dune exec lambda_lifting_test < manytests/typed/007order.ml
  : end_of_input
  $ dune exec lambda_lifting_test < manytests/typed/008ascription.ml
  : end_of_input
  $ dune exec lambda_lifting_test < manytests/typed/015tuples.ml
  : end_of_input
  $ dune exec lambda_lifting_test < manytests/typed/016lists.ml
  : end_of_input
