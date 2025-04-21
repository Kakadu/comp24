  $ dune exec anf_conv_test << EOF
  > let fac n =
  >   let rec fack n k = if (n<=1) 
  >     then n (n-1)
  >     else (fun m -> k * m*n)
  >   in fack n (fun x -> x)
  > ;;
  > EOF
  (fun anon$1(n k n m)->
  	(let anf_op#1=(k*m)
  	in
  	(let anf_op#2=(anf_op#1*n)
  	in
  	anf_op#2))
  )
  (fun fack(n k)->
  	(let anf_op#3=(n<=1)
  	in
  	(let anf_if#4=if (anf_op#3)
  		then (
  			(let anf_op#5=(n-1)
  			in
  			(let anf_app#6=(n anf_op#5)
  			in
  			anf_app#6))
  		) else (
  			(let anf_app#7=(anon$1 n)
  			in
  			(let anf_app#8=(anf_app#7 k)
  			in
  			(let anf_app#9=(anf_app#8 n)
  			in
  			anf_app#9))))
  	in
  	anf_if#4))
  )
  (fun anon$2(n x)->
  	x
  )
  (fun fac(n)->
  	(let anf_op#10=(n<=1)
  	in
  	(let anf_if#11=if (anf_op#10)
  		then (
  			(let anf_op#12=(n-1)
  			in
  			(let anf_app#13=(n anf_op#12)
  			in
  			anf_app#13))
  		) else (
  			(let anf_app#14=(anon$1 n)
  			in
  			(let anf_app#15=(anf_app#14 k)
  			in
  			(let anf_app#16=(anf_app#15 n)
  			in
  			anf_app#16))))
  	in
  	(let fack=anf_if#11
  	in
  	(let anf_app#17=(fack n)
  	in
  	(let anf_app#18=(anon$2 n)
  	in
  	(let anf_app#19=(anf_app#17 anf_app#18)
  	in
  	anf_app#19))))))
  )
  $ dune exec anf_conv_test << EOF
  > let f a=
  >   let g c d =
  >     let h e = a*(c+d*e) in 
  >     h 4 in 
  >     g 2 3
  > ;;
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
  	(let anf_op#4=(d*e)
  	in
  	(let anf_op#5=(c+anf_op#4)
  	in
  	(let anf_op#6=(a*anf_op#5)
  	in
  	(let h=anf_op#6
  	in
  	(let anf_app#7=(h c)
  	in
  	(let anf_app#8=(anf_app#7 d)
  	in
  	(let anf_app#9=(anf_app#8 a)
  	in
  	(let anf_app#10=(anf_app#9 4)
  	in
  	anf_app#10))))))))
  )
  (fun f(a)->
  	(let anf_op#11=(d*e)
  	in
  	(let anf_op#12=(c+anf_op#11)
  	in
  	(let anf_op#13=(a*anf_op#12)
  	in
  	(let h=anf_op#13
  	in
  	(let anf_app#14=(h c)
  	in
  	(let anf_app#15=(anf_app#14 d)
  	in
  	(let anf_app#16=(anf_app#15 a)
  	in
  	(let anf_app#17=(anf_app#16 4)
  	in
  	(let g=anf_app#17
  	in
  	(let anf_app#18=(g a)
  	in
  	(let anf_app#19=(anf_app#18 2)
  	in
  	(let anf_app#20=(anf_app#19 3)
  	in
  	anf_app#20))))))))))))
  )
  $ dune exec anf_conv_test << EOF
  > let f a b =
  >   let g c =
  >     let h = (fun x -> x*(a (c*b))) in 
  >     h a in
  >     g 3
  >  ;;
  > EOF
  (fun anon$1(c a b x)->
  	(let anf_op#1=(c*b)
  	in
  	(let anf_app#2=(a anf_op#1)
  	in
  	(let anf_op#3=(x*anf_app#2)
  	in
  	anf_op#3)))
  )
  (fun h(c a b)->
  	(let anf_app#4=(anon$1 c)
  	in
  	(let anf_app#5=(anf_app#4 a)
  	in
  	(let anf_app#6=(anf_app#5 b)
  	in
  	anf_app#6)))
  )
  (fun g(a b c)->
  	(let anf_app#7=(anon$1 c)
  	in
  	(let anf_app#8=(anf_app#7 a)
  	in
  	(let anf_app#9=(anf_app#8 b)
  	in
  	(let h=anf_app#9
  	in
  	(let anf_app#10=(h c)
  	in
  	(let anf_app#11=(anf_app#10 a)
  	in
  	(let anf_app#12=(anf_app#11 b)
  	in
  	(let anf_app#13=(anf_app#12 a)
  	in
  	anf_app#13))))))))
  )
  (fun f(a b)->
  	(let anf_app#14=(anon$1 c)
  	in
  	(let anf_app#15=(anf_app#14 a)
  	in
  	(let anf_app#16=(anf_app#15 b)
  	in
  	(let h=anf_app#16
  	in
  	(let anf_app#17=(h c)
  	in
  	(let anf_app#18=(anf_app#17 a)
  	in
  	(let anf_app#19=(anf_app#18 b)
  	in
  	(let anf_app#20=(anf_app#19 a)
  	in
  	(let g=anf_app#20
  	in
  	(let anf_app#21=(g a)
  	in
  	(let anf_app#22=(anf_app#21 b)
  	in
  	(let anf_app#23=(anf_app#22 3)
  	in
  	anf_app#23))))))))))))
  )
  $ dune exec anf_conv_test << EOF
  > let f a =
  >   let g a b=
  >     let h b c= a*(b/c) in 
  >     h 2 3 in 
  >     g (1+0) a
  > ;;
  > EOF
  (fun h(a a b c)->
  	(let anf_op#1=(b/c)
  	in
  	(let anf_op#2=(a*anf_op#1)
  	in
  	anf_op#2))
  )
  (fun g(a b)->
  	(let anf_op#3=(b/c)
  	in
  	(let anf_op#4=(a*anf_op#3)
  	in
  	(let h=anf_op#4
  	in
  	(let anf_app#5=(h a)
  	in
  	(let anf_app#6=(anf_app#5 a)
  	in
  	(let anf_app#7=(anf_app#6 2)
  	in
  	(let anf_app#8=(anf_app#7 3)
  	in
  	anf_app#8)))))))
  )
  (fun f(a)->
  	(let anf_op#9=(b/c)
  	in
  	(let anf_op#10=(a*anf_op#9)
  	in
  	(let h=anf_op#10
  	in
  	(let anf_app#11=(h a)
  	in
  	(let anf_app#12=(anf_app#11 a)
  	in
  	(let anf_app#13=(anf_app#12 2)
  	in
  	(let anf_app#14=(anf_app#13 3)
  	in
  	(let g=anf_app#14
  	in
  	(let anf_op#15=(1+0)
  	in
  	(let anf_app#16=(g anf_op#15)
  	in
  	(let anf_app#17=(anf_app#16 a)
  	in
  	anf_app#17)))))))))))
  )
  $ dune exec anf_conv_test << EOF
  > let f a =
  >   let g b = a / b in 
  >   let h c = (a * c) in 
  >   ((h 1) + (g 2))
  > ;;
  > EOF
  (fun g(a b)->
  	(let anf_op#1=(a/b)
  	in
  	anf_op#1)
  )
  (fun h(a c)->
  	(let anf_op#2=(a*c)
  	in
  	anf_op#2)
  )
  (fun f(a)->
  	(let anf_op#3=(a/b)
  	in
  	(let g=anf_op#3
  	in
  	(let anf_op#4=(a*c)
  	in
  	(let h=anf_op#4
  	in
  	(let anf_app#5=(h a)
  	in
  	(let anf_app#6=(anf_app#5 1)
  	in
  	(let anf_app#7=(g a)
  	in
  	(let anf_app#8=(anf_app#7 2)
  	in
  	(let anf_op#9=(anf_app#6+anf_app#8)
  	in
  	anf_op#9)))))))))
  )
  $ dune exec anf_conv_test << EOF
  > let f a =
  >   let g = (fun x -> x) in 
  >   let h = (fun x -> a * x) in 
  >   ((g a) + (h a))
  > ;;
  > EOF
  (fun anon$1(a x)->
  	x
  )
  (fun g(a)->
  	(let anf_app#1=(anon$1 a)
  	in
  	anf_app#1)
  )
  (fun anon$2(a x)->
  	(let anf_op#2=(a*x)
  	in
  	anf_op#2)
  )
  (fun h(a)->
  	(let anf_app#3=(anon$2 a)
  	in
  	anf_app#3)
  )
  (fun f(a)->
  	(let anf_app#4=(anon$1 a)
  	in
  	(let g=anf_app#4
  	in
  	(let anf_app#5=(anon$2 a)
  	in
  	(let h=anf_app#5
  	in
  	(let anf_app#6=(g a)
  	in
  	(let anf_app#7=(anf_app#6 a)
  	in
  	(let anf_app#8=(h a)
  	in
  	(let anf_app#9=(anf_app#8 a)
  	in
  	(let anf_op#10=(anf_app#7+anf_app#9)
  	in
  	anf_op#10)))))))))
  )
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
  			(let anf_app#7=(fack anf_op#6)
  			in
  			(let anf_app#8=(anon$1 n)
  			in
  			(let anf_app#9=(anf_app#8 f)
  			in
  			(let anf_app#10=(anf_app#9 n)
  			in
  			(let anf_app#11=(anf_app#7 anf_app#10)
  			in
  			anf_app#11)))))))
  	in
  	anf_if#4))
  )
  (fun anon$2(n x)->
  	x
  )
  (fun fac(n)->
  	(let anf_op#12=(n<=1)
  	in
  	(let anf_if#13=if (anf_op#12)
  		then (
  			(let anf_app#14=(f 1)
  			in
  			anf_app#14)
  		) else (
  			(let anf_op#15=(n-1)
  			in
  			(let anf_app#16=(fack anf_op#15)
  			in
  			(let anf_app#17=(anon$1 n)
  			in
  			(let anf_app#18=(anf_app#17 f)
  			in
  			(let anf_app#19=(anf_app#18 n)
  			in
  			(let anf_app#20=(anf_app#16 anf_app#19)
  			in
  			anf_app#20)))))))
  	in
  	(let fack=anf_if#13
  	in
  	(let anf_app#21=(fack n)
  	in
  	(let anf_app#22=(anon$2 n)
  	in
  	(let anf_app#23=(anf_app#21 anf_app#22)
  	in
  	anf_app#23))))))
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
  	(let anf_op#6=(n<1)
  	in
  	(let anf_if#7=if (anf_op#6)
  		then (
  			n
  		) else (
  			(let anf_op#8=(n-1)
  			in
  			(let anf_app#9=(fack anf_op#8)
  			in
  			(let anf_op#10=(n*anf_app#9)
  			in
  			anf_op#10))))
  	in
  	(let fack=anf_if#7
  	in
  	(let anf_app#11=(fack n)
  	in
  	anf_app#11))))
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
  	(let anf_op#4=(d*e)
  	in
  	(let anf_op#5=(c+anf_op#4)
  	in
  	(let anf_op#6=(a*anf_op#5)
  	in
  	(let h=anf_op#6
  	in
  	(let anf_app#7=(h c)
  	in
  	(let anf_app#8=(anf_app#7 d)
  	in
  	(let anf_app#9=(anf_app#8 a)
  	in
  	(let anf_app#10=(anf_app#9 4)
  	in
  	anf_app#10))))))))
  )
  (fun f(a)->
  	(let anf_op#11=(d*e)
  	in
  	(let anf_op#12=(c+anf_op#11)
  	in
  	(let anf_op#13=(a*anf_op#12)
  	in
  	(let h=anf_op#13
  	in
  	(let anf_app#14=(h c)
  	in
  	(let anf_app#15=(anf_app#14 d)
  	in
  	(let anf_app#16=(anf_app#15 a)
  	in
  	(let anf_app#17=(anf_app#16 4)
  	in
  	(let g=anf_app#17
  	in
  	(let anf_app#18=(g a)
  	in
  	(let anf_app#19=(anf_app#18 2)
  	in
  	(let anf_app#20=(anf_app#19 3)
  	in
  	anf_app#20))))))))))))
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
  			(let anf_app#4=(fac anf_op#3)
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
  	(let anf_app#1=(x x)
  	in
  	(let anf_app#2=(anf_app#1 f)
  	in
  	anf_app#2))
  )
  (fun anon$1(f x)->
  	(let anf_app#3=(anon$2 x)
  	in
  	(let anf_app#4=(anf_app#3 f)
  	in
  	(let anf_app#5=(f anf_app#4)
  	in
  	anf_app#5)))
  )
  (fun fix(f)->
  	(let anf_app#6=(anon$1 f)
  	in
  	anf_app#6)
  )
  (fun anon$4(x f)->
  	(let anf_app#7=(x x)
  	in
  	(let anf_app#8=(anf_app#7 f)
  	in
  	anf_app#8))
  )
  (fun anon$3(x)->
  	(let anf_app#9=(anon$4 x)
  	in
  	(let anf_app#10=(f anf_app#9)
  	in
  	anf_app#10))
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
  	(let ()=anf_app#7
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
  			(let anf_app#7=(fac_cps anf_op#6)
  			in
  			(let anf_app#8=(anon$1 n)
  			in
  			(let anf_app#9=(anf_app#8 k)
  			in
  			(let anf_app#10=(anf_app#7 anf_app#9)
  			in
  			anf_app#10))))))
  	in
  	anf_if#4))
  )
  (fun anon$2(print_int)->
  	print_int
  )
  (fun main()->
  	(let anf_app#11=(fac_cps 4)
  	in
  	(let anf_app#12=(anon$2 )
  	in
  	(let anf_app#13=(anf_app#11 anf_app#12)
  	in
  	(let anf_app#14=(print_int anf_app#13)
  	in
  	(let ()=anf_app#14
  	in
  	0)))))
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
  			(let anf_op#5=(n-1)
  			in
  			(let n1=anf_op#5
  			in
  			(let anf_op#6=(a+b)
  			in
  			(let ab=anf_op#6
  			in
  			(let anf_app#7=(fib_acc b)
  			in
  			(let anf_app#8=(ab a)
  			in
  			(let anf_app#9=(anf_app#8 b)
  			in
  			(let anf_app#10=(anf_app#9 n)
  			in
  			(let anf_app#11=(anf_app#7 anf_app#10)
  			in
  			(let anf_app#12=(n1 a)
  			in
  			(let anf_app#13=(anf_app#12 b)
  			in
  			(let anf_app#14=(anf_app#13 n)
  			in
  			(let anf_app#15=(anf_app#11 anf_app#14)
  			in
  			anf_app#15))))))))))))))
  	in
  	anf_if#4))
  )
  (fun fib(n)->
  	(let anf_op#16=(n<2)
  	in
  	(let anf_if#17=if (anf_op#16)
  		then (
  			n
  		) else (
  			(let anf_op#18=(n-1)
  			in
  			(let anf_op#19=(n-2)
  			in
  			(let anf_app#20=(fib anf_op#19)
  			in
  			(let anf_op#21=(anf_op#18+anf_app#20)
  			in
  			(let anf_app#22=(fib anf_op#21)
  			in
  			anf_app#22))))))
  	in
  	anf_if#17))
  )
  (fun main()->
  	(let anf_app#23=(fib_acc 0)
  	in
  	(let anf_app#24=(anf_app#23 1)
  	in
  	(let anf_app#25=(anf_app#24 4)
  	in
  	(let anf_app#26=(print_int anf_app#25)
  	in
  	(let ()=anf_app#26
  	in
  	(let anf_app#27=(fib 4)
  	in
  	(let anf_app#28=(print_int anf_app#27)
  	in
  	(let ()=anf_app#28
  	in
  	0))))))))
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
  (fun a_0(a b c)->
  	(let anf_app#3=(print_int a)
  	in
  	anf_app#3)
  )
  (fun b_0(a b c)->
  	(let anf_app#4=(print_int b)
  	in
  	anf_app#4)
  )
  (fun c_0(a b c)->
  	(let anf_app#5=(print_int c)
  	in
  	anf_app#5)
  )
  (fun test3(a b c)->
  	(let anf_app#6=(print_int a)
  	in
  	(let a_0=anf_app#6
  	in
  	(let anf_app#7=(print_int b)
  	in
  	(let b_0=anf_app#7
  	in
  	(let anf_app#8=(print_int c)
  	in
  	(let c_0=anf_app#8
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
  (fun main()->
  	(let anf_app#18=(test10 )
  	in
  	(let anf_app#19=(wrap anf_app#18)
  	in
  	(let anf_app#20=(anf_app#19 1)
  	in
  	(let anf_app#21=(anf_app#20 10)
  	in
  	(let anf_app#22=(anf_app#21 100)
  	in
  	(let anf_app#23=(anf_app#22 1000)
  	in
  	(let anf_app#24=(anf_app#23 10000)
  	in
  	(let anf_app#25=(anf_app#24 100000)
  	in
  	(let anf_app#26=(anf_app#25 1000000)
  	in
  	(let anf_app#27=(anf_app#26 10000000)
  	in
  	(let anf_app#28=(anf_app#27 100000000)
  	in
  	(let anf_app#29=(anf_app#28 1000000000)
  	in
  	(let rez=anf_app#29
  	in
  	(let anf_app#30=(print_int rez)
  	in
  	(let ()=anf_app#30
  	in
  	(let anf_app#31=(test3 )
  	in
  	(let anf_app#32=(wrap anf_app#31)
  	in
  	(let anf_app#33=(anf_app#32 1)
  	in
  	(let anf_app#34=(anf_app#33 10)
  	in
  	(let anf_app#35=(anf_app#34 100)
  	in
  	(let temp2=anf_app#35
  	in
  	0)))))))))))))))))))))
  )
  $ dune exec anf_conv_test < manytests/typed/005fix.ml
  (fun fix(f x)->
  	(let anf_app#1=(fix f)
  	in
  	(let anf_app#2=(f anf_app#1)
  	in
  	(let anf_app#3=(anf_app#2 x)
  	in
  	anf_app#3)))
  )
  (fun fac(self n)->
  	(let anf_op#4=(n<=1)
  	in
  	(let anf_if#5=if (anf_op#4)
  		then (
  			1
  		) else (
  			(let anf_op#6=(n-1)
  			in
  			(let anf_app#7=(self anf_op#6)
  			in
  			(let anf_op#8=(n*anf_app#7)
  			in
  			anf_op#8))))
  	in
  	anf_if#5))
  )
  (fun main()->
  	(let anf_app#9=(fac )
  	in
  	(let anf_app#10=(fix anf_app#9)
  	in
  	(let anf_app#11=(anf_app#10 6)
  	in
  	(let anf_app#12=(print_int anf_app#11)
  	in
  	(let ()=anf_app#12
  	in
  	0)))))
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
  	(let anf_if#3=if (b)
  		then (
  			(let anf_app#4=(anon$1 b)
  			in
  			anf_app#4)
  		) else (
  			(let anf_app#5=(anon$2 b)
  			in
  			anf_app#5))
  	in
  	anf_if#3)
  )
  (fun foo_0(x)->
  	(let anf_app#6=(foo true)
  	in
  	(let anf_app#7=(foo false)
  	in
  	(let anf_app#8=(foo true)
  	in
  	(let anf_app#9=(foo false)
  	in
  	(let anf_app#10=(anf_app#9 x)
  	in
  	(let anf_app#11=(anf_app#8 anf_app#10)
  	in
  	(let anf_app#12=(anf_app#7 anf_app#11)
  	in
  	(let anf_app#13=(anf_app#6 anf_app#12)
  	in
  	anf_app#13))))))))
  )
  (fun main()->
  	(let anf_app#14=(foo_0 11)
  	in
  	(let anf_app#15=(print_int anf_app#14)
  	in
  	(let ()=anf_app#15
  	in
  	0)))
  )
  $ dune exec anf_conv_test < manytests/typed/006partial2.ml
  (fun foo(a b c)->
  	(let anf_app#1=(print_int a)
  	in
  	(let ()=anf_app#1
  	in
  	(let anf_app#2=(print_int b)
  	in
  	(let ()=anf_app#2
  	in
  	(let anf_app#3=(print_int c)
  	in
  	(let ()=anf_app#3
  	in
  	(let anf_op#4=(b*c)
  	in
  	(let anf_op#5=(a+anf_op#4)
  	in
  	anf_op#5))))))))
  )
  (fun main()->
  	(let anf_app#6=(foo 1)
  	in
  	(let foo_0=anf_app#6
  	in
  	(let anf_app#7=(foo_0 2)
  	in
  	(let foo_0_2=anf_app#7
  	in
  	(let anf_app#8=(foo_0_2 3)
  	in
  	(let foo_0_2_4=anf_app#8
  	in
  	(let anf_app#9=(print_int foo_0_2_4)
  	in
  	(let ()=anf_app#9
  	in
  	0))))))))
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
  	(let ()=anf_app#2
  	in
  	(let anf_app#3=(anon$2 b)
  	in
  	(let anf_app#4=(anf_app#3 a)
  	in
  	anf_app#4))))
  )
  (fun foo(a)->
  	(let anf_app#5=(print_int a)
  	in
  	(let ()=anf_app#5
  	in
  	(let anf_app#6=(anon$1 a)
  	in
  	anf_app#6)))
  )
  (fun main()->
  	(let anf_app#7=(foo 4)
  	in
  	(let anf_app#8=(anf_app#7 8)
  	in
  	(let anf_app#9=(anf_app#8 9)
  	in
  	(let ()=anf_app#9
  	in
  	0))))
  )
  $ dune exec anf_conv_test < manytests/typed/007order.ml
  : end_of_input
  $ dune exec anf_conv_test < manytests/typed/008ascription.ml
  : end_of_input
  $ dune exec anf_conv_test < manytests/typed/015tuples.ml
  : end_of_input
  $ dune exec anf_conv_test < manytests/typed/016lists.ml
  : end_of_input
