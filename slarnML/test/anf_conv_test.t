  $ dune exec anf_conv_test << EOF
  > let fac n = 
  >   let rec fack n f = if (n <= 1) then (f 1) else (fack (n - 1) (fun x -> x * (f n))) in
  >   (fack n (fun x -> x))
  > ;;
  > EOF
  (fun anon$1#fack#fac(n f x)->
  	(let anf_app#1=(f n)
  	in
  	(let anf_op#2=(x*anf_app#1)
  	in
  	anf_op#2))
  )
  (fun fack#fac(n f)->
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
  			(let anf_app#7=(anon$1#fack#fac n f)
  			in
  			(let anf_app#8=(fack#fac anf_op#6 anf_app#7)
  			in
  			anf_app#8))))
  	in
  	anf_if#4))
  )
  (fun anon$2#fac(x)->
  	x
  )
  (fun fac(n)->
  	(let anf_app#9=(anon$2#fac )
  	in
  	(let anf_app#10=(fack#fac n anf_app#9)
  	in
  	anf_app#10))
  )
  $ dune exec anf_conv_test << EOF
  > let fac n = 
  >   let rec fack n = if (n < 1) then n else n * (fack (n - 1)) in
  >   (fack n)
  > ;;
  > EOF
  (fun fack#fac(n)->
  	(let anf_op#1=(n<1)
  	in
  	(let anf_if#2=if (anf_op#1)
  		then (
  			n
  		) else (
  			(let anf_op#3=(n-1)
  			in
  			(let anf_app#4=(fack#fac anf_op#3)
  			in
  			(let anf_op#5=(n*anf_app#4)
  			in
  			anf_op#5))))
  	in
  	anf_if#2))
  )
  (fun fac(n)->
  	(let anf_app#6=(fack#fac n)
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
  (fun h#g#f(a c d e)->
  	(let anf_op#1=(d*e)
  	in
  	(let anf_op#2=(c+anf_op#1)
  	in
  	(let anf_op#3=(a*anf_op#2)
  	in
  	anf_op#3)))
  )
  (fun g#f(a c d)->
  	(let anf_app#4=(h#g#f a c d 4)
  	in
  	anf_app#4)
  )
  (fun f(a)->
  	(let anf_app#5=(g#f a 2 3)
  	in
  	anf_app#5)
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
  (fun ()#main()->
  	(let anf_app#6=(fac 4)
  	in
  	(let anf_app#7=(print_int anf_app#6)
  	in
  	anf_app#7))
  )
  (fun main()->
  	0
  )
  $ dune exec anf_conv_test < manytests/typed/002fac.ml
  (fun anon$1#fac_cps(n k p)->
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
  			(let anf_app#7=(anon$1#fac_cps n k)
  			in
  			(let anf_app#8=(fac_cps anf_op#6 anf_app#7)
  			in
  			anf_app#8))))
  	in
  	anf_if#4))
  )
  (fun anon$1#()#main(print_int)->
  	print_int
  )
  (fun ()#main()->
  	(let anf_app#9=(anon$1#()#main )
  	in
  	(let anf_app#10=(fac_cps 4 anf_app#9)
  	in
  	(let anf_app#11=(print_int anf_app#10)
  	in
  	anf_app#11)))
  )
  (fun main()->
  	0
  )
  $ dune exec anf_conv_test < manytests/typed/003fib.ml
  : end_of_input
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
  (fun a#test3(a)->
  	(let anf_app#3=(print_int a#test3)
  	in
  	anf_app#3)
  )
  (fun b#test3(b)->
  	(let anf_app#4=(print_int b#test3)
  	in
  	anf_app#4)
  )
  (fun c#test3(c)->
  	(let anf_app#5=(print_int c#test3)
  	in
  	anf_app#5)
  )
  (fun test3(a b c)->
  	0
  )
  (fun test10(a b c d e f g h i j)->
  	(let anf_op#6=(a+b)
  	in
  	(let anf_op#7=(anf_op#6+c)
  	in
  	(let anf_op#8=(anf_op#7+d)
  	in
  	(let anf_op#9=(anf_op#8+e)
  	in
  	(let anf_op#10=(anf_op#9+f)
  	in
  	(let anf_op#11=(anf_op#10+g)
  	in
  	(let anf_op#12=(anf_op#11+h)
  	in
  	(let anf_op#13=(anf_op#12+i)
  	in
  	(let anf_op#14=(anf_op#13+j)
  	in
  	anf_op#14)))))))))
  )
  (fun temp0#main()->
  	(let anf_app#15=(test10 )
  	in
  	(let anf_app#16=(wrap anf_app#15 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000)
  	in
  	anf_app#16))
  )
  (fun temp1#main(temp0)->
  	(let anf_app#17=(print_int temp0)
  	in
  	anf_app#17)
  )
  (fun temp2#main()->
  	(let anf_app#18=(test3 )
  	in
  	(let anf_app#19=(wrap anf_app#18 1 10 100)
  	in
  	anf_app#19))
  )
  (fun main()->
  	0
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
  (fun ()#main()->
  	(let anf_app#8=(fac )
  	in
  	(let anf_app#9=(fix anf_app#8 6)
  	in
  	(let anf_app#10=(print_int anf_app#9)
  	in
  	anf_app#10)))
  )
  (fun main()->
  	0
  )
  $ dune exec anf_conv_test < manytests/typed/006partial.ml
  (fun anon$1#foo(foo)->
  	(let anf_op#1=(foo+2)
  	in
  	anf_op#1)
  )
  (fun anon$2#foo(foo)->
  	(let anf_op#2=(foo*10)
  	in
  	anf_op#2)
  )
  (fun foo(b)->
  	(let anf_if#3=if (b)
  		then (
  			(let anf_app#4=(anon$1#foo )
  			in
  			anf_app#4)
  		) else (
  			(let anf_app#5=(anon$2#foo )
  			in
  			anf_app#5))
  	in
  	anf_if#3)
  )
  (fun foo(x)->
  	(let anf_app#6=(foo false x)
  	in
  	(let anf_app#7=(foo true anf_app#6)
  	in
  	(let anf_app#8=(foo false anf_app#7)
  	in
  	(let anf_app#9=(foo true anf_app#8)
  	in
  	anf_app#9))))
  )
  (fun ()#main()->
  	(let anf_app#10=(foo 11)
  	in
  	(let anf_app#11=(print_int anf_app#10)
  	in
  	anf_app#11))
  )
  (fun main()->
  	0
  )
  $ dune exec anf_conv_test < manytests/typed/006partial2.ml
  (fun ()#foo(a)->
  	(let anf_app#1=(print_int a)
  	in
  	anf_app#1)
  )
  (fun ()#foo(b)->
  	(let anf_app#2=(print_int b)
  	in
  	anf_app#2)
  )
  (fun ()#foo(c)->
  	(let anf_app#3=(print_int c)
  	in
  	anf_app#3)
  )
  (fun foo(a b c)->
  	(let anf_op#4=(b*c)
  	in
  	(let anf_op#5=(a+anf_op#4)
  	in
  	anf_op#5))
  )
  (fun foo#main()->
  	(let anf_app#6=(foo#main 1)
  	in
  	anf_app#6)
  )
  (fun foo#main(foo)->
  	(let anf_app#7=(foo#main foo#main 2)
  	in
  	anf_app#7)
  )
  (fun foo#main(foo)->
  	(let anf_app#8=(foo#main foo#main 3)
  	in
  	anf_app#8)
  )
  (fun ()#main(foo)->
  	(let anf_app#9=(print_int foo)
  	in
  	anf_app#9)
  )
  (fun main()->
  	0
  )
  $ dune exec anf_conv_test < manytests/typed/006partial3.ml
  (fun ()#foo(a)->
  	(let anf_app#1=(print_int a)
  	in
  	anf_app#1)
  )
  (fun ()#anon$1#foo(b)->
  	(let anf_app#2=(print_int b)
  	in
  	anf_app#2)
  )
  (fun anon$2#anon$1#foo(c)->
  	(let anf_app#3=(print_int c)
  	in
  	anf_app#3)
  )
  (fun anon$1#foo(b)->
  	(let anf_app#4=(anon$2#anon$1#foo )
  	in
  	anf_app#4)
  )
  (fun foo(a)->
  	(let anf_app#5=(anon$1#foo )
  	in
  	anf_app#5)
  )
  (fun ()#main()->
  	(let anf_app#6=(foo 4 8 9)
  	in
  	anf_app#6)
  )
  (fun main()->
  	0
  )
  $ dune exec anf_conv_test < manytests/typed/007order.ml
  : end_of_input
  $ dune exec anf_conv_test < manytests/typed/008ascription.ml
  : end_of_input
  $ dune exec anf_conv_test < manytests/typed/015tuples.ml
  : end_of_input
  $ dune exec anf_conv_test < manytests/typed/016lists.ml
  : end_of_input
