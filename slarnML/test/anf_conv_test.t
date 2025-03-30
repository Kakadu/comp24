  $ dune exec anf_conv_test << EOF
  > let fac n = 
  >   let rec fack n f = if (n <= 1) then (f 1) else (fack (n - 1) (fun x -> x * (f n))) in
  >   (fack n (fun x -> x))
  > ;;
  > EOF
  (fun anon$1(n f x)->
  	(let anf_app#1=(n )
  	in
  	(let anf_app#2=(f anf_app#1)
  	in
  	(let anf_op#3=(x*anf_app#2)
  	in
  	anf_op#3)))
  )
  (fun fack(n f)->
  	(let anf_op#4=(n<=1)
  	in
  	(let anf_if#5=if (anf_op#4)
  		then (
  			(let anf_app#6=(f 1)
  			in
  			anf_app#6)
  		) else (
  			(let anf_op#7=(n-1)
  			in
  			(let anf_app#8=(n )
  			in
  			(let anf_app#9=(f )
  			in
  			(let anf_app#10=(anon$1 anf_app#8 anf_app#9)
  			in
  			(let anf_app#11=(fack anf_op#7 anf_app#10)
  			in
  			anf_app#11))))))
  	in
  	anf_if#5))
  )
  (fun anon$2(x)->
  	x
  )
  (fun fac(n)->
  	(let anf_app#12=(n )
  	in
  	(let anf_app#13=(anon$2 )
  	in
  	(let anf_app#14=(fack anf_app#12 anf_app#13)
  	in
  	anf_app#14)))
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
  	(let anf_app#6=(n )
  	in
  	(let anf_app#7=(fack anf_app#6)
  	in
  	anf_app#7))
  )
  $ dune exec anf_conv_test << EOF
  > let f a =
  >   let g c d =
  >     let h e = a * (c + d * e) in
  >     (h 4)
  >   in
  >   (g 2 3)
  > EOF
  (fun h(a c d e)->
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
  	(let anf_app#8=(a )
  	in
  	(let anf_app#9=(c )
  	in
  	(let anf_app#10=(d )
  	in
  	(let anf_app#11=(h anf_app#8 anf_app#9 anf_app#10 4)
  	in
  	anf_app#11))))
  )
  (fun f(a)->
  	(let anf_app#12=(a )
  	in
  	(let anf_app#13=(g anf_app#12 2 3)
  	in
  	anf_app#13))
  )
  $ dune exec anf_conv_test < manytests/do_not_type/001.ml
  fac not exist
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
  f not exist
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
  	(let anf_()#8=(anf_app#7 )
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
  			(let anf_app#7=(n )
  			in
  			(let anf_app#8=(k )
  			in
  			(let anf_app#9=(anon$1 anf_app#7 anf_app#8)
  			in
  			(let anf_app#10=(fac_cps anf_op#6 anf_app#9)
  			in
  			anf_app#10))))))
  	in
  	anf_if#4))
  )
  (fun anon$2(print_int)->
  	print_int
  )
  (fun main()->
  	(let anf_app#11=(anon$2 )
  	in
  	(let anf_app#12=(fac_cps 4 anf_app#11)
  	in
  	(let anf_app#13=(print_int anf_app#12)
  	in
  	(let anf_()#14=(anf_app#13 )
  	in
  	0))))
  )
  $ dune exec anf_conv_test < manytests/typed/003fib.ml
  (fun n1(n)->
  	(let anf_op#1=(n-1)
  	in
  	anf_op#1)
  )
  (fun ab(a b)->
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
  			(let anf_app#5=(b )
  			in
  			(let anf_app#6=(ab )
  			in
  			(let anf_app#7=(n1 )
  			in
  			(let anf_app#8=(fib_acc anf_app#5 anf_app#6 anf_app#7)
  			in
  			anf_app#8)))))
  	in
  	anf_if#4))
  )
  (fun fib(n)->
  	(let anf_op#9=(n<2)
  	in
  	(let anf_if#10=if (anf_op#9)
  		then (
  			n
  		) else (
  			(let anf_op#11=(n-1)
  			in
  			(let anf_op#12=(n-2)
  			in
  			(let anf_app#13=(fib anf_op#12)
  			in
  			(let anf_op#14=(anf_op#11+anf_app#13)
  			in
  			(let anf_app#15=(fib anf_op#14)
  			in
  			anf_app#15))))))
  	in
  	anf_if#10))
  )
  (fun main()->
  	(let anf_app#16=(fib_acc 0 1 4)
  	in
  	(let anf_app#17=(print_int anf_app#16)
  	in
  	(let anf_()#18=(anf_app#17 )
  	in
  	(let anf_app#19=(fib 4)
  	in
  	(let anf_app#20=(print_int anf_app#19)
  	in
  	(let anf_()#21=(anf_app#20 )
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
  (fun a(a)->
  	(let anf_app#5=(a )
  	in
  	(let anf_app#6=(print_int anf_app#5)
  	in
  	anf_app#6))
  )
  (fun b(b)->
  	(let anf_app#7=(b )
  	in
  	(let anf_app#8=(print_int anf_app#7)
  	in
  	anf_app#8))
  )
  (fun c(c)->
  	(let anf_app#9=(c )
  	in
  	(let anf_app#10=(print_int anf_app#9)
  	in
  	anf_app#10))
  )
  (fun test3(a b c)->
  	0
  )
  (fun test10(a b c d e f g h i j)->
  	(let anf_app#11=(a )
  	in
  	(let anf_app#12=(b )
  	in
  	(let anf_op#13=(anf_app#11+anf_app#12)
  	in
  	(let anf_app#14=(c )
  	in
  	(let anf_op#15=(anf_op#13+anf_app#14)
  	in
  	(let anf_app#16=(d )
  	in
  	(let anf_op#17=(anf_op#15+anf_app#16)
  	in
  	(let anf_app#18=(e )
  	in
  	(let anf_op#19=(anf_op#17+anf_app#18)
  	in
  	(let anf_app#20=(f )
  	in
  	(let anf_op#21=(anf_op#19+anf_app#20)
  	in
  	(let anf_app#22=(g )
  	in
  	(let anf_op#23=(anf_op#21+anf_app#22)
  	in
  	(let anf_app#24=(h )
  	in
  	(let anf_op#25=(anf_op#23+anf_app#24)
  	in
  	(let anf_app#26=(i )
  	in
  	(let anf_op#27=(anf_op#25+anf_app#26)
  	in
  	(let anf_app#28=(j )
  	in
  	(let anf_op#29=(anf_op#27+anf_app#28)
  	in
  	anf_op#29)))))))))))))))))))
  )
  (fun rez()->
  	(let anf_app#30=(test10 )
  	in
  	(let anf_app#31=(wrap anf_app#30 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000)
  	in
  	anf_app#31))
  )
  (fun temp2()->
  	(let anf_app#32=(test3 )
  	in
  	(let anf_app#33=(wrap anf_app#32 1 10 100)
  	in
  	anf_app#33))
  )
  (fun main()->
  	(let anf_app#34=(rez )
  	in
  	(let anf_app#35=(print_int anf_app#34)
  	in
  	(let anf_()#36=(anf_app#35 )
  	in
  	0)))
  )
  $ dune exec anf_conv_test < manytests/typed/005fix.ml
  (fun fix(f x)->
  	(let anf_app#1=(f )
  	in
  	(let anf_app#2=(fix anf_app#1)
  	in
  	(let anf_app#3=(x )
  	in
  	(let anf_app#4=(f anf_app#2 anf_app#3)
  	in
  	anf_app#4))))
  )
  (fun fac(self n)->
  	(let anf_app#5=(n )
  	in
  	(let anf_op#6=(anf_app#5<=1)
  	in
  	(let anf_if#7=if (anf_op#6)
  		then (
  			1
  		) else (
  			(let anf_app#8=(n )
  			in
  			(let anf_app#9=(n )
  			in
  			(let anf_op#10=(anf_app#9-1)
  			in
  			(let anf_app#11=(self anf_op#10)
  			in
  			(let anf_op#12=(anf_app#8*anf_app#11)
  			in
  			anf_op#12))))))
  	in
  	anf_if#7)))
  )
  (fun main()->
  	(let anf_app#13=(fac )
  	in
  	(let anf_app#14=(fix anf_app#13 6)
  	in
  	(let anf_app#15=(print_int anf_app#14)
  	in
  	(let anf_()#16=(anf_app#15 )
  	in
  	0))))
  )
  $ dune exec anf_conv_test < manytests/typed/006partial.ml
  (fun anon$1(foo)->
  	(let anf_op#1=(foo+2)
  	in
  	anf_op#1)
  )
  (fun anon$2(foo)->
  	(let anf_op#2=(foo*10)
  	in
  	anf_op#2)
  )
  (fun foo(b)->
  	(let anf_app#3=(b )
  	in
  	(let anf_if#4=if (anf_app#3)
  		then (
  			(let anf_app#5=(anon$1 )
  			in
  			anf_app#5)
  		) else (
  			(let anf_app#6=(anon$2 )
  			in
  			anf_app#6))
  	in
  	anf_if#4))
  )
  (fun foo(x)->
  	(let anf_app#7=(x )
  	in
  	(let anf_app#8=(foo false anf_app#7)
  	in
  	(let anf_app#9=(foo true anf_app#8)
  	in
  	(let anf_app#10=(foo false anf_app#9)
  	in
  	(let anf_app#11=(foo true anf_app#10)
  	in
  	anf_app#11)))))
  )
  (fun main()->
  	(let anf_app#12=(foo 11)
  	in
  	(let anf_app#13=(print_int anf_app#12)
  	in
  	(let anf_()#14=(anf_app#13 )
  	in
  	0)))
  )
  $ dune exec anf_conv_test < manytests/typed/006partial2.ml
  (fun foo(a b c)->
  	(let anf_app#1=(a )
  	in
  	(let anf_app#2=(print_int anf_app#1)
  	in
  	(let anf_()#3=(anf_app#2 )
  	in
  	(let anf_app#4=(b )
  	in
  	(let anf_app#5=(print_int anf_app#4)
  	in
  	(let anf_()#6=(anf_app#5 )
  	in
  	(let anf_app#7=(c )
  	in
  	(let anf_app#8=(print_int anf_app#7)
  	in
  	(let anf_()#9=(anf_app#8 )
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
  (fun foo()->
  	(let anf_app#15=(foo 1)
  	in
  	anf_app#15)
  )
  (fun foo(foo)->
  	(let anf_app#16=(foo )
  	in
  	(let anf_app#17=(foo anf_app#16 2)
  	in
  	anf_app#17))
  )
  (fun foo(foo)->
  	(let anf_app#18=(foo )
  	in
  	(let anf_app#19=(foo anf_app#18 3)
  	in
  	anf_app#19))
  )
  (fun main()->
  	(let anf_app#20=(foo )
  	in
  	(let anf_app#21=(print_int anf_app#20)
  	in
  	(let anf_()#22=(anf_app#21 )
  	in
  	0)))
  )
  $ dune exec anf_conv_test < manytests/typed/006partial3.ml
  (fun anon$2(c)->
  	(let anf_app#1=(c )
  	in
  	(let anf_app#2=(print_int anf_app#1)
  	in
  	anf_app#2))
  )
  (fun anon$1(b)->
  	(let anf_app#3=(b )
  	in
  	(let anf_app#4=(print_int anf_app#3)
  	in
  	(let anf_()#5=(anf_app#4 )
  	in
  	(let anf_app#6=(anon$2 )
  	in
  	anf_app#6))))
  )
  (fun foo(a)->
  	(let anf_app#7=(a )
  	in
  	(let anf_app#8=(print_int anf_app#7)
  	in
  	(let anf_()#9=(anf_app#8 )
  	in
  	(let anf_app#10=(anon$1 )
  	in
  	anf_app#10))))
  )
  (fun main()->
  	(let anf_app#11=(foo 4 8 9)
  	in
  	(let anf_()#12=(anf_app#11 )
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
