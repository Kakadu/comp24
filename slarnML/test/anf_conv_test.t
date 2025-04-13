  $ dune exec anf_conv_test << EOF
  > let fac n = 
  >   let rec fack n f = if (n <= 1) then (f 1) else (fack (n - 1) (fun x -> x * (f n))) in
  >   (fack n (fun x -> x))
  > ;;
  > EOF
  (fun anon$1(n f n x)->
  	(let anf_app#1=(f n)
  	in
  	(let anf_op#2=(x*anf_app#1)
  	in
  	anf_op#2))
  )
  (fun fack(n f)->
  	(let anf_op#3=(n<=1)
  	in
  	(let anf_if#4=if (anf_op#3)
  		then (
  			(let anf_app#5=(f 1)
  			in
  			anf_app#5)
  		) else (
  			(let anf_op#6=(n-1)
  			in
  			(let anf_app#7=(anon$1 n f n)
  			in
  			(let anf_app#8=(fack anf_op#6 anf_app#7)
  			in
  			anf_app#8))))
  	in
  	anf_if#4))
  )
  (fun anon$2(n x)->
  	x
  )
  (fun fac(n)->
  	(let anf_app#9=(anon$2 n)
  	in
  	(let anf_app#10=(fack n anf_app#9)
  	in
  	anf_app#10))
  )
  $ dune exec anf_conv_test << EOF
  > let fac n = 
  >   let rec fack n = if (n < 1) then n else n * (fack (n - 1)) in
  >   (fack n)
  > ;;
  > EOF
  (fun fack(n)->
  	(let anf_op#1=(n<1)
  	in
  	(let anf_if#2=if (anf_op#1)
  		then (
  			n
  		) else (
  			(let anf_op#3=(n-1)
  			in
  			(let anf_app#4=(fack anf_op#3)
  			in
  			(let anf_op#5=(n*anf_app#4)
  			in
  			anf_op#5))))
  	in
  	anf_if#2))
  )
  (fun fac(n)->
  	(let anf_app#6=(fack n)
  	in
  	anf_app#6)
  )
  $ dune exec anf_conv_test << EOF
  > let f a =
  >   let g c d =
  >     let h e = a * (c + d * e) in
  >     (h 4)
  >   in
  >   (g 2 3)
  > EOF
  (fun h(c d a e)->
  	(let anf_op#1=(d*e)
  	in
  	(let anf_op#2=(c+anf_op#1)
  	in
  	(let anf_op#3=(a*anf_op#2)
  	in
  	anf_op#3)))
  )
  (fun g(a c d)->
  	(let anf_app#4=(h c d a c d a 4)
  	in
  	anf_app#4)
  )
  (fun f(a)->
  	(let anf_app#5=(g a a 2 3)
  	in
  	anf_app#5)
  )
  $ dune exec anf_conv_test < manytests/do_not_type/001.ml
  Id fac not found in env
  (fun recfac(n)->
  	(let anf_op#1=(n<=1)
  	in
  	(let anf_if#2=if (anf_op#1)
  		then (
  			1
  		) else (
  			(let anf_op#3=(n-1)
  			in
  			(let anf_app#4=(fac n anf_op#3)
  			in
  			(let anf_op#5=(n*anf_app#4)
  			in
  			anf_op#5))))
  	in
  	anf_if#2))
  )
  $ dune exec anf_conv_test < manytests/do_not_type/002if.ml
  (fun main()->
  	(let anf_if#1=if (true)
  		then (
  			1
  		) else (
  			false)
  	in
  	anf_if#1)
  )
  $ dune exec anf_conv_test < manytests/do_not_type/003occurs.ml
  Id f not found in env
  (fun anon$2(x f)->
  	(let anf_app#1=(x x f)
  	in
  	anf_app#1)
  )
  (fun anon$1(f x)->
  	(let anf_app#2=(anon$2 x f)
  	in
  	(let anf_app#3=(f anf_app#2)
  	in
  	anf_app#3))
  )
  (fun fix(f)->
  	(let anf_app#4=(anon$1 f)
  	in
  	anf_app#4)
  )
  (fun anon$4(x f)->
  	(let anf_app#5=(x x f)
  	in
  	anf_app#5)
  )
  (fun anon$3(x)->
  	(let anf_app#6=(anon$4 x)
  	in
  	(let anf_app#7=(f x anf_app#6)
  	in
  	anf_app#7))
  )
  $ dune exec anf_conv_test < manytests/typed/001fac.ml
  (fun fac(n)->
  	(let anf_op#1=(n<=1)
  	in
  	(let anf_if#2=if (anf_op#1)
  		then (
  			1
  		) else (
  			(let anf_op#3=(n-1)
  			in
  			(let anf_app#4=(fac anf_op#3)
  			in
  			(let anf_op#5=(n*anf_app#4)
  			in
  			anf_op#5))))
  	in
  	anf_if#2))
  )
  (fun main()->
  	(let anf_app#6=(fac 4)
  	in
  	(let anf_app#7=(print_int anf_app#6)
  	in
  	(let anf_()#8=anf_app#7
  	in
  	0)))
  )
  $ dune exec anf_conv_test < manytests/typed/002fac.ml
  (fun anon$1(n k p)->
  	(let anf_op#1=(p*n)
  	in
  	(let anf_app#2=(k anf_op#1)
  	in
  	anf_app#2))
  )
  (fun fac_cps(n k)->
  	(let anf_op#3=(n=1)
  	in
  	(let anf_if#4=if (anf_op#3)
  		then (
  			(let anf_app#5=(k 1)
  			in
  			anf_app#5)
  		) else (
  			(let anf_op#6=(n-1)
  			in
  			(let anf_app#7=(anon$1 n k)
  			in
  			(let anf_app#8=(fac_cps anf_op#6 anf_app#7)
  			in
  			anf_app#8))))
  	in
  	anf_if#4))
  )
  (fun anon$2(print_int)->
  	print_int
  )
  (fun main()->
  	(let anf_app#9=(anon$2 )
  	in
  	(let anf_app#10=(fac_cps 4 anf_app#9)
  	in
  	(let anf_app#11=(print_int anf_app#10)
  	in
  	(let anf_()#12=anf_app#11
  	in
  	0))))
  )
  $ dune exec anf_conv_test < manytests/typed/003fib.ml
  (fun n1(a b n)->
  	(let anf_op#1=(n-1)
  	in
  	anf_op#1)
  )
  (fun ab(a b n)->
  	(let anf_op#2=(a+b)
  	in
  	anf_op#2)
  )
  (fun fib_acc(a b n)->
  	(let anf_op#3=(n=1)
  	in
  	(let anf_if#4=if (anf_op#3)
  		then (
  			b
  		) else (
  			(let anf_app#5=(ab a b n)
  			in
  			(let anf_app#6=(n1 a b n)
  			in
  			(let anf_app#7=(fib_acc b anf_app#5 anf_app#6)
  			in
  			anf_app#7))))
  	in
  	anf_if#4))
  )
  (fun fib(n)->
  	(let anf_op#8=(n<2)
  	in
  	(let anf_if#9=if (anf_op#8)
  		then (
  			n
  		) else (
  			(let anf_op#10=(n-1)
  			in
  			(let anf_app#11=(fib anf_op#10)
  			in
  			(let anf_op#12=(n-2)
  			in
  			(let anf_app#13=(fib anf_op#12)
  			in
  			(let anf_op#14=(anf_app#11+anf_app#13)
  			in
  			anf_op#14))))))
  	in
  	anf_if#9))
  )
  (fun main()->
  	(let anf_app#15=(fib_acc 0 1 4)
  	in
  	(let anf_app#16=(print_int anf_app#15)
  	in
  	(let anf_()#17=anf_app#16
  	in
  	(let anf_app#18=(fib 4)
  	in
  	(let anf_app#19=(print_int anf_app#18)
  	in
  	(let anf_()#20=anf_app#19
  	in
  	0))))))
  )
  $ dune exec anf_conv_test < manytests/typed/004manyargs.ml
  (fun wrap(f)->
  	(let anf_op#1=(1=1)
  	in
  	(let anf_if#2=if (anf_op#1)
  		then (
  			f
  		) else (
  			f)
  	in
  	anf_if#2))
  )
  (fun test3(a b c)->
  	(let anf_app#3=(print_int a)
  	in
  	(let anf_()#4=anf_app#3
  	in
  	(let anf_app#5=(print_int b)
  	in
  	(let anf_()#6=anf_app#5
  	in
  	(let anf_app#7=(print_int c)
  	in
  	(let anf_()#8=anf_app#7
  	in
  	0))))))
  )
  (fun test10(a b c d e f g h i j)->
  	(let anf_op#9=(a+b)
  	in
  	(let anf_op#10=(anf_op#9+c)
  	in
  	(let anf_op#11=(anf_op#10+d)
  	in
  	(let anf_op#12=(anf_op#11+e)
  	in
  	(let anf_op#13=(anf_op#12+f)
  	in
  	(let anf_op#14=(anf_op#13+g)
  	in
  	(let anf_op#15=(anf_op#14+h)
  	in
  	(let anf_op#16=(anf_op#15+i)
  	in
  	(let anf_op#17=(anf_op#16+j)
  	in
  	anf_op#17)))))))))
  )
  (fun rez()->
  	(let anf_app#18=(test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000)
  	in
  	anf_app#18)
  )
  (fun main()->
  	(let anf_app#19=(rez )
  	in
  	(let anf_app#20=(print_int anf_app#19)
  	in
  	(let anf_()#21=anf_app#20
  	in
  	(let anf_app#22=(test3 1 10 100)
  	in
  	(let anf_()#23=anf_app#22
  	in
  	0)))))
  )
  $ dune exec anf_conv_test < manytests/typed/005fix.ml
  (fun fix(f x)->
  	(let anf_app#1=(fix f)
  	in
  	(let anf_app#2=(f anf_app#1 x)
  	in
  	anf_app#2))
  )
  (fun fac(self n)->
  	(let anf_op#3=(n<=1)
  	in
  	(let anf_if#4=if (anf_op#3)
  		then (
  			1
  		) else (
  			(let anf_op#5=(n-1)
  			in
  			(let anf_app#6=(self anf_op#5)
  			in
  			(let anf_op#7=(n*anf_app#6)
  			in
  			anf_op#7))))
  	in
  	anf_if#4))
  )
  (fun main()->
  	(let anf_app#8=(fac )
  	in
  	(let anf_app#9=(fix anf_app#8 6)
  	in
  	(let anf_app#10=(print_int anf_app#9)
  	in
  	(let anf_()#11=anf_app#10
  	in
  	0))))
  )
  $ dune exec anf_conv_test < manytests/typed/006partial.ml
  (fun anon$1(b foo)->
  	(let anf_app#1=(foo )
  	in
  	(let anf_op#2=(anf_app#1+2)
  	in
  	anf_op#2))
  )
  (fun anon$2(b foo)->
  	(let anf_app#3=(foo )
  	in
  	(let anf_op#4=(anf_app#3*10)
  	in
  	anf_op#4))
  )
  (fun foo(b)->
  	(let anf_if#5=if (b)
  		then (
  			(let anf_app#6=(anon$1 b)
  			in
  			anf_app#6)
  		) else (
  			(let anf_app#7=(anon$2 b)
  			in
  			anf_app#7))
  	in
  	anf_if#5)
  )
  (fun foo_0(x)->
  	(let anf_app#8=(foo false x)
  	in
  	(let anf_app#9=(foo true anf_app#8)
  	in
  	(let anf_app#10=(foo false anf_app#9)
  	in
  	(let anf_app#11=(foo true anf_app#10)
  	in
  	anf_app#11))))
  )
  (fun main()->
  	(let anf_app#12=(foo_0 11)
  	in
  	(let anf_app#13=(print_int anf_app#12)
  	in
  	(let anf_()#14=anf_app#13
  	in
  	0)))
  )
  $ dune exec anf_conv_test < manytests/typed/006partial2.ml
  (fun foo(a b c)->
  	(let anf_app#1=(print_int a)
  	in
  	(let anf_()#2=anf_app#1
  	in
  	(let anf_app#3=(print_int b)
  	in
  	(let anf_()#4=anf_app#3
  	in
  	(let anf_app#5=(print_int c)
  	in
  	(let anf_()#6=anf_app#5
  	in
  	(let anf_op#7=(b*c)
  	in
  	(let anf_op#8=(a+anf_op#7)
  	in
  	anf_op#8))))))))
  )
  (fun foo_0()->
  	(let anf_app#9=(foo 1)
  	in
  	anf_app#9)
  )
  (fun foo_0_2()->
  	(let anf_app#10=(foo_0 2)
  	in
  	anf_app#10)
  )
  (fun foo_0_2_4()->
  	(let anf_app#11=(foo_0_2 3)
  	in
  	anf_app#11)
  )
  (fun main()->
  	(let anf_app#12=(foo_0_2_4 )
  	in
  	(let anf_app#13=(print_int anf_app#12)
  	in
  	(let anf_()#14=anf_app#13
  	in
  	0)))
  )
  $ dune exec anf_conv_test < manytests/typed/006partial3.ml
  (fun anon$2(b a c)->
  	(let anf_app#1=(print_int c)
  	in
  	anf_app#1)
  )
  (fun anon$1(a b)->
  	(let anf_app#2=(print_int b)
  	in
  	(let anf_()#3=anf_app#2
  	in
  	(let anf_app#4=(anon$2 b a)
  	in
  	anf_app#4)))
  )
  (fun foo(a)->
  	(let anf_app#5=(print_int a)
  	in
  	(let anf_()#6=anf_app#5
  	in
  	(let anf_app#7=(anon$1 a)
  	in
  	anf_app#7)))
  )
  (fun main()->
  	(let anf_app#8=(foo 4 8 9)
  	in
  	(let anf_()#9=anf_app#8
  	in
  	0))
  )
  $ dune exec anf_conv_test < manytests/typed/007order.ml
  : end_of_input
  $ dune exec anf_conv_test < manytests/typed/008ascription.ml
  : end_of_input
  $ dune exec anf_conv_test < manytests/typed/015tuples.ml
  : end_of_input
  $ dune exec anf_conv_test < manytests/typed/016lists.ml
  : end_of_input
