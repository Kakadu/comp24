  $ dune exec parser_test << EOF
  > let a = 3
  > EOF
  let a=(3)
  $ dune exec parser_test << EOF
  > let () = 0
  > EOF
  let ()=(0)
  $ dune exec parser_test << EOF
  > (fun a -> b)
  > EOF
  (fun a->b)
  $ dune exec parser_test << EOF
  > let rec a = b in (c)
  > EOF
  let rec a=(b) in (c)
  $ dune exec parser_test << EOF
  > if a then b else c
  > EOF
  if (a) then (b) else (c)
  $ dune exec parser_test << EOF
  > let a = 
  >   let b = 1 in
  >   let c = b in
  >   c
  > EOF
  let a=(let b=(1) in (let c=(b) in (c)))
  $ dune exec parser_test << EOF
  > true && (a + (f false (g 3 y)) = 3  || 2)
  > EOF
  (true&&(((a+(f false (g 3 y)))=3)||2))
  $ dune exec parser_test << EOF
  > (a b 2 1+3 * b d (-2) (r f)) + 3
  > EOF
  Error: : end_of_input
  $ dune exec parser_test << EOF
  > a b c
  > EOF
  (a b c)
  $ dune exec parser_test << EOF
  > (a + (f 2 x (g 3*z y)) * 3)
  > EOF
  (a+((f 2 x ((g 3)*(z y)))*3))
  $ dune exec parser_test << EOF
  > (a + f 2 x (g 3*z y) * 3)
  > EOF
  (a+(f 2 x (((g 3)*(z y))*3)))
  $ dune exec parser_test << EOF
  > a + 2 <= b * 3
  > EOF
  ((a+2)<=(b*3))
  $ dune exec parser_test << EOF
  > a < 2 && b = 3
  > EOF
  ((a<2)&&(b=3))
  $ dune exec parser_test << EOF
  > ((a b 2 1 + 3 * b d (-2) (r f)) + 3)
  > EOF
  (((a b 2 1)+(3*(b d (-2) (r f))))+3)
  $ dune exec parser_test << EOF
  > let fac n = 
  >   let rec fack n f = 
  >     if n <= 1
  >     then f 1
  >     else fack (n - 1) (fun x -> x * f n)
  >   in
  >   fack n (fun x -> x)
  > ;;
  > EOF
  let fac n=(let rec fack n f=(if ((n<=1)) then ((f 1)) else ((fack (n-1) (fun x->(x*(f n)))))) in ((fack n (fun x->x))))
  $ dune exec parser_test << EOF
  > let fac n = 
  >   let rec fack n = if n < 1 then n else n * fack (n - 1) in
  >   fack n
  > ;;
  > EOF
  let fac n=(let rec fack n=(if ((n<1)) then (n) else ((n*(fack (n-1))))) in ((fack n)))
  $ dune exec parser_test << EOF
  > let x = fack n
  > ;;
  > EOF
  let x=((fack n))
  $ dune exec parser_test << EOF
  > f 1 + f 2
  > EOF
  ((f 1)+(f 2))
  $ dune exec parser_test << EOF
  > let rec fib n =
  >   if n<2
  >   then n
  >   else (fib (n - 1) + fib (n - 2))
  > EOF
  let rec fib n=(if ((n<2)) then (n) else ((fib ((n-1)+(fib (n-2))))))
  $ dune exec parser_test < manytests/do_not_type/001.ml
  let recfac n=(if ((n<=1)) then (1) else ((n*(fac (n-1)))))
  $ dune exec parser_test < manytests/do_not_type/002if.ml
  let main=(if (true) then (1) else (false))
  $ dune exec parser_test < manytests/do_not_type/003occurs.ml
  let fix f=((fun x->(f (fun f->(x x f)))))	
  (fun x->(f (fun f->(x x f))))
  $ dune exec parser_test < manytests/typed/001fac.ml
  let rec fac n=(if ((n<=1)) then (1) else ((n*(fac (n-1)))))	
  let main=(let ()=((print_int (fac 4))) in (0))
  $ dune exec parser_test < manytests/typed/002fac.ml
  let rec fac_cps n k=(if ((n=1)) then ((k 1)) else ((fac_cps (n-1) (fun p->(k (p*n))))))	
  let main=(let ()=((print_int (fac_cps 4 (fun print_int->print_int)))) in (0))
  $ dune exec parser_test < manytests/typed/003fib.ml
  let rec fib_acc a b n=(if ((n=1)) then (b) else (let n1=((n-1)) in (let ab=((a+b)) in ((fib_acc b ab n1)))))	
  let rec fib n=(if ((n<2)) then (n) else ((fib ((n-1)+(fib (n-2))))))	
  let main=(let ()=((print_int (fib_acc 0 1 4))) in (let ()=((print_int (fib 4))) in (0)))
  $ dune exec parser_test < manytests/typed/004manyargs.ml
  let wrap f=(if ((1=1)) then (f) else (f))	
  let test3 a b c=(let a=((print_int a)) in (let b=((print_int b)) in (let c=((print_int c)) in (0))))	
  let test10 a b c d e f g h i j=((((((((((a+b)+c)+d)+e)+f)+g)+h)+i)+j))	
  let main=(let rez=((wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000)) in (let ()=((print_int rez)) in (let temp2=((wrap test3 1 10 100)) in (0))))
  $ dune exec parser_test < manytests/typed/005fix.ml
  let rec fix f x=((f (fix f) x))	
  let fac self n=(if ((n<=1)) then (1) else ((n*(self (n-1)))))	
  let main=(let ()=((print_int (fix fac 6))) in (0))
  $ dune exec parser_test < manytests/typed/006partial.ml
  let foo b=(if (b) then ((fun foo->(foo+2))) else ((fun foo->(foo*10))))	
  let foo x=((foo true (foo false (foo true (foo false x)))))	
  let main=(let ()=((print_int (foo 11))) in (0))
  $ dune exec parser_test < manytests/typed/006partial2.ml
  let foo a b c=(let ()=((print_int a)) in (let ()=((print_int b)) in (let ()=((print_int c)) in ((a+(b*c))))))	
  let main=(let foo=((foo 1)) in (let foo=((foo 2)) in (let foo=((foo 3)) in (let ()=((print_int foo)) in (0)))))
  $ dune exec parser_test < manytests/typed/006partial3.ml
  let foo a=(let ()=((print_int a)) in ((fun b->let ()=((print_int b)) in ((fun c->(print_int c))))))	
  let main=(let ()=((foo 4 8 9)) in (0))
  $ dune exec parser_test << EOF
  > let f a =
  >   let g c d =
  >     let h e = a * (c + d * e) in
  >     (h 4)
  >   in
  >   (g 2 3)
  > EOF
  let f a=(let g c d=(let h e=((a*(c+(d*e)))) in ((h 4))) in ((g 2 3)))
