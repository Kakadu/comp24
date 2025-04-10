  $ dune exec anf_conv_test << EOF
  > let fac n = 
  >   let rec fack n f = if (n <= 1) then (f 1) else (fack (n - 1) (fun x -> x * (f n))) in
  >   (fack n (fun x -> x))
  > ;;
  > EOF
  (fun anon$1(x)->
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
  			(let anf_app#7=(fack anf_op#6 anon$1)
  			in
  			anf_app#7)))
  	in
  	anf_if#4))
  )
  (fun anon$2(x)->
  	x
  )
  (fun fac(n)->
  	(let anf_app#8=(fack n anon$2)
  	in
  	anf_app#8)
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
  	(let anf_app#1=(a )
  	in
  	(let anf_app#2=(c )
  	in
  	(let anf_app#3=(d )
  	in
  	(let anf_app#4=(e )
  	in
  	(let anf_op#5=(anf_app#3*anf_app#4)
  	in
  	(let anf_op#6=(anf_app#2+anf_op#5)
  	in
  	(let anf_op#7=(anf_app#1*anf_op#6)
  	in
  	anf_op#7)))))))
  )
  (fun g(a c d)->
  	(let anf_app#8=(h c d a 4)
  	in
  	anf_app#8)
  )
  (fun f(a)->
  	(let anf_app#9=(g a 2 3)
  	in
  	anf_app#9)
  )
  $ dune exec anf_conv_test < manytests/do_not_type/001.ml
  Id fac not found in env
  (fun recfac(n)->
  	(let anf_app#1=(n )
  	in
  	(let anf_op#2=(anf_app#1<=1)
  	in
  	(let anf_if#3=if (anf_op#2)
  		then (
  			1
  		) else (
  			(let anf_app#4=(n )
  			in
  			(let anf_app#5=(n )
  			in
  			(let anf_app#6=(n )
  			in
  			(let anf_op#7=(anf_app#6-1)
  			in
  			(let anf_app#8=(fac anf_app#5 anf_op#7)
  			in
  			(let anf_op#9=(anf_app#4*anf_app#8)
  			in
  			anf_op#9)))))))
  	in
  	anf_if#3)))
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
  (fun anon$2(f)->
  	(let anf_app#1=(x f x f)
  	in
  	anf_app#1)
  )
  (fun anon$1(f x)->
  	(let anf_app#2=(f anon$2)
  	in
  	anf_app#2)
  )
  (fun fix(f)->
  	anon$1
  )
  (fun anon$4(f)->
  	(let anf_app#3=(x x f)
  	in
  	anf_app#3)
  )
  (fun anon$3(x)->
  	(let anf_app#4=(f x anon$4)
  	in
  	anf_app#4)
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
  (fun anon$1(p)->
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
  			(let anf_app#7=(fac_cps anf_op#6 anon$1)
  			in
  			anf_app#7)))
  	in
  	anf_if#4))
  )
  (fun anon$2(print_int)->
  	print_int
  )
  (fun main()->
  	(let anf_app#8=(fac_cps 4 anon$2)
  	in
  	(let anf_app#9=(print_int anf_app#8)
  	in
  	(let anf_()#10=anf_app#9
  	in
  	0)))
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
  			(let anf_app#5=(fib_acc b ab n1)
  			in
  			anf_app#5))
  	in
  	anf_if#4))
  )
  (fun fib(n)->
  	(let anf_op#6=(n<2)
  	in
  	(let anf_if#7=if (anf_op#6)
  		then (
  			n
  		) else (
  			(let anf_op#8=(n-1)
  			in
  			(let anf_op#9=(n-2)
  			in
  			(let anf_app#10=(fib anf_op#9)
  			in
  			(let anf_op#11=(anf_op#8+anf_app#10)
  			in
  			(let anf_app#12=(fib anf_op#11)
  			in
  			anf_app#12))))))
  	in
  	anf_if#7))
  )
  (fun main()->
  	(let anf_app#13=(fib_acc 0 1 4)
  	in
  	(let anf_app#14=(print_int anf_app#13)
  	in
  	(let anf_()#15=anf_app#14
  	in
  	(let anf_app#16=(fib 4)
  	in
  	(let anf_app#17=(print_int anf_app#16)
  	in
  	(let anf_()#18=anf_app#17
  	in
  	0))))))
  )
  $ dune exec anf_conv_test < manytests/typed/004manyargs.ml
  (fun wrap(f)->
  	(let anf_op#1=(1=1)
  	in
  	(let anf_if#2=if (anf_op#1)
  		then (
  			(let anf_app#3=(f )
  			in
  			anf_app#3)
  		) else (
  			(let anf_app#4=(f )
  			in
  			anf_app#4))
  	in
  	anf_if#2))
  )
  (fun a_0(a b c)->
  	(let anf_app#5=(a )
  	in
  	(let anf_app#6=(print_int anf_app#5)
  	in
  	anf_app#6))
  )
  (fun b_0(a b c)->
  	(let anf_app#7=(print_int b)
  	in
  	anf_app#7)
  )
  (fun c_0(a b c)->
  	(let anf_app#8=(print_int c)
  	in
  	anf_app#8)
  )
  (fun test3(a b c)->
  	0
  )
  (fun test10(a b c d e f g h i j)->
  	(let anf_app#9=(a )
  	in
  	(let anf_app#10=(b )
  	in
  	(let anf_op#11=(anf_app#9+anf_app#10)
  	in
  	(let anf_app#12=(c )
  	in
  	(let anf_op#13=(anf_op#11+anf_app#12)
  	in
  	(let anf_app#14=(d )
  	in
  	(let anf_op#15=(anf_op#13+anf_app#14)
  	in
  	(let anf_app#16=(e )
  	in
  	(let anf_op#17=(anf_op#15+anf_app#16)
  	in
  	(let anf_app#18=(f )
  	in
  	(let anf_op#19=(anf_op#17+anf_app#18)
  	in
  	(let anf_app#20=(g )
  	in
  	(let anf_op#21=(anf_op#19+anf_app#20)
  	in
  	(let anf_app#22=(h )
  	in
  	(let anf_op#23=(anf_op#21+anf_app#22)
  	in
  	(let anf_app#24=(i )
  	in
  	(let anf_op#25=(anf_op#23+anf_app#24)
  	in
  	(let anf_app#26=(j )
  	in
  	(let anf_op#27=(anf_op#25+anf_app#26)
  	in
  	anf_op#27)))))))))))))))))))
  )
  (fun rez()->
  	(let anf_app#28=(test10 )
  	in
  	(let anf_app#29=(wrap anf_app#28 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000)
  	in
  	anf_app#29))
  )
  (fun temp2()->
  	(let anf_app#30=(test3 )
  	in
  	(let anf_app#31=(wrap anf_app#30 1 10 100)
  	in
  	anf_app#31))
  )
  (fun main()->
  	(let anf_app#32=(print_int rez)
  	in
  	(let anf_()#33=anf_app#32
  	in
  	0))
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
  	(let anf_app#3=(n )
  	in
  	(let anf_op#4=(anf_app#3<=1)
  	in
  	(let anf_if#5=if (anf_op#4)
  		then (
  			1
  		) else (
  			(let anf_app#6=(n )
  			in
  			(let anf_app#7=(n )
  			in
  			(let anf_op#8=(anf_app#7-1)
  			in
  			(let anf_app#9=(self anf_op#8)
  			in
  			(let anf_op#10=(anf_app#6*anf_app#9)
  			in
  			anf_op#10))))))
  	in
  	anf_if#5)))
  )
  (fun main()->
  	(let anf_app#11=(fac )
  	in
  	(let anf_app#12=(fix anf_app#11 6)
  	in
  	(let anf_app#13=(print_int anf_app#12)
  	in
  	(let anf_()#14=anf_app#13
  	in
  	0))))
  )
  $ dune exec anf_conv_test < manytests/typed/006partial.ml
  (fun anon$1(b foo)->
  	(let anf_op#1=(foo+2)
  	in
  	anf_op#1)
  )
  (fun anon$2(b foo)->
  	(let anf_op#2=(foo*10)
  	in
  	anf_op#2)
  )
  (fun foo(b)->
  	(let anf_app#3=(b )
  	in
  	(let anf_if#4=if (anf_app#3)
  		then (
  			anon$1
  		) else (
  			anon$2)
  	in
  	anf_if#4))
  )
  (fun foo_0(x)->
  	(let anf_app#5=(x )
  	in
  	(let anf_app#6=(foo false anf_app#5)
  	in
  	(let anf_app#7=(foo true anf_app#6)
  	in
  	(let anf_app#8=(foo false anf_app#7)
  	in
  	(let anf_app#9=(foo true anf_app#8)
  	in
  	anf_app#9)))))
  )
  (fun main()->
  	(let anf_app#10=(foo_0 11)
  	in
  	(let anf_app#11=(print_int anf_app#10)
  	in
  	(let anf_()#12=anf_app#11
  	in
  	0)))
  )
  $ dune exec anf_conv_test < manytests/typed/006partial2.ml
  (fun foo(a b c)->
  	(let anf_app#1=(a )
  	in
  	(let anf_app#2=(print_int anf_app#1)
  	in
  	(let anf_()#3=anf_app#2
  	in
  	(let anf_app#4=(b )
  	in
  	(let anf_app#5=(print_int anf_app#4)
  	in
  	(let anf_()#6=anf_app#5
  	in
  	(let anf_app#7=(c )
  	in
  	(let anf_app#8=(print_int anf_app#7)
  	in
  	(let anf_()#9=anf_app#8
  	in
  	(let anf_app#10=(a )
  	in
  	(let anf_app#11=(b )
  	in
  	(let anf_app#12=(c )
  	in
  	(let anf_op#13=(anf_app#11*anf_app#12)
  	in
  	(let anf_op#14=(anf_app#10+anf_op#13)
  	in
  	anf_op#14))))))))))))))
  )
  (fun foo_0()->
  	(let anf_app#15=(foo 1)
  	in
  	anf_app#15)
  )
  (fun foo_0_1()->
  	(let anf_app#16=(foo_0 2)
  	in
  	anf_app#16)
  )
  (fun foo_0_1_2()->
  	(let anf_app#17=(foo_0_1 3)
  	in
  	anf_app#17)
  )
  (fun main()->
  	(let anf_app#18=(print_int foo_0_1_2)
  	in
  	(let anf_()#19=anf_app#18
  	in
  	0))
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
  	anon$2))
  )
  (fun foo(a)->
  	(let anf_app#4=(a )
  	in
  	(let anf_app#5=(print_int anf_app#4)
  	in
  	(let anf_()#6=anf_app#5
  	in
  	anon$1)))
  )
  (fun main()->
  	(let anf_app#7=(foo 4 8 9)
  	in
  	(let anf_()#8=anf_app#7
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
