  $ dune exec anf_conv_test << EOF
  > let fac n =
  >   let rec fack n k = if (n<=1) 
  >     then n (n-1)
  >     else (fun m -> k * m*n)
  >   in fack n (fun x -> x)
  > ;;
  > EOF
  let anon_1 n k n m=
  	let anf_op_1=(k*m)
  	in
  	let anf_op_2=(anf_op_1*n)
  	in
  	anf_op_2
  
  let fack n k=
  	let anf_op_3=(n<=1)
  	in
  	let anf_if_4=if (anf_op_3)
  		then (
  			let anf_op_5=(n-1)
  			in
  			let anf_app_6=(n anf_op_5)
  			in
  			anf_app_6
  		) else (
  			let anf_app_7=(anon_1 n k n)
  			in
  			anf_app_7)
  	in
  	anf_if_4
  
  let anon_2 n x=
  	x
  
  let fac n=
  	let anf_app_8=(anon_2 n)
  	in
  	let anf_app_9=(fack n anf_app_8)
  	in
  	anf_app_9
  $ dune exec anf_conv_test << EOF
  > let f a=
  >   let g c d =
  >     let h e = a*(c+d*e) in 
  >     h 4 in 
  >     g 2 3
  > ;;
  > EOF
  let h c d a e=
  	let anf_op_1=(d*e)
  	in
  	let anf_op_2=(c+anf_op_1)
  	in
  	let anf_op_3=(a*anf_op_2)
  	in
  	anf_op_3
  
  let g a c d=
  	let anf_app_4=(h c d a 4)
  	in
  	anf_app_4
  
  let f a=
  	let anf_app_5=(g a 2 3)
  	in
  	anf_app_5
  $ dune exec anf_conv_test << EOF
  > let f a b =
  >   let g c =
  >     let h = (fun x -> x*(a (c*b))) in 
  >     h a in
  >     g 3
  >  ;;
  > EOF
  let anon_1 c a b x=
  	let anf_op_1=(c*b)
  	in
  	let anf_app_2=(a anf_op_1)
  	in
  	let anf_op_3=(x*anf_app_2)
  	in
  	anf_op_3
  
  let h c a b=
  	let anf_app_4=(anon_1 c a b)
  	in
  	anf_app_4
  
  let g a b c=
  	let anf_app_5=(h c a b)
  	in
  	let anf_app_6=(anf_app_5 a)
  	in
  	anf_app_6
  
  let f a b=
  	let anf_app_7=(g a b 3)
  	in
  	anf_app_7
  $ dune exec anf_conv_test << EOF
  > let f a =
  >   let g a b=
  >     let h b c= a*(b/c) in 
  >     h 2 3 in 
  >     g (1+0) a
  > ;;
  > EOF
  let h a a b c=
  	let anf_op_1=(b/c)
  	in
  	let anf_op_2=(a*anf_op_1)
  	in
  	anf_op_2
  
  let g a b=
  	let anf_app_3=(h a a 2 3)
  	in
  	anf_app_3
  
  let f a=
  	let anf_op_4=(1+0)
  	in
  	let anf_app_5=(g anf_op_4 a)
  	in
  	anf_app_5
  $ dune exec anf_conv_test << EOF
  > let f a =
  >   let g b = a / b in 
  >   let h c = (a * c) in 
  >   ((h 1) + (g 2))
  > ;;
  > EOF
  let g a b=
  	let anf_op_1=(a/b)
  	in
  	anf_op_1
  
  let h a c=
  	let anf_op_2=(a*c)
  	in
  	anf_op_2
  
  let f a=
  	let anf_app_3=(h a 1)
  	in
  	let anf_app_4=(g a 2)
  	in
  	let anf_op_5=(anf_app_3+anf_app_4)
  	in
  	anf_op_5
  $ dune exec anf_conv_test << EOF
  > let f a =
  >   let g = (fun x -> x) in 
  >   let h = (fun x -> a * x) in 
  >   ((g a) + (h a))
  > ;;
  > EOF
  let anon_1 a x=
  	x
  
  let g a=
  	let anf_app_1=(anon_1 a)
  	in
  	anf_app_1
  
  let anon_2 a x=
  	let anf_op_2=(a*x)
  	in
  	anf_op_2
  
  let h a=
  	let anf_app_3=(anon_2 a)
  	in
  	anf_app_3
  
  let f a=
  	let anf_app_4=(g a)
  	in
  	let anf_app_5=(anf_app_4 a)
  	in
  	let anf_app_6=(h a)
  	in
  	let anf_app_7=(anf_app_6 a)
  	in
  	let anf_op_8=(anf_app_5+anf_app_7)
  	in
  	anf_op_8
  $ dune exec anf_conv_test << EOF
  > let fac n = 
  >   let rec fack n f = if (n <= 1) then (f 1) else (fack (n - 1) (fun x -> x * (f n))) in
  >   (fack n (fun x -> x))
  > ;;
  > EOF
  let anon_1 n f n x=
  	let anf_app_1=(f n)
  	in
  	let anf_op_2=(x*anf_app_1)
  	in
  	anf_op_2
  
  let fack n f=
  	let anf_op_3=(n<=1)
  	in
  	let anf_if_4=if (anf_op_3)
  		then (
  			let anf_app_5=(f 1)
  			in
  			anf_app_5
  		) else (
  			let anf_op_6=(n-1)
  			in
  			let anf_app_7=(anon_1 n f n)
  			in
  			let anf_app_8=(fack anf_op_6 anf_app_7)
  			in
  			anf_app_8)
  	in
  	anf_if_4
  
  let anon_2 n x=
  	x
  
  let fac n=
  	let anf_app_9=(anon_2 n)
  	in
  	let anf_app_10=(fack n anf_app_9)
  	in
  	anf_app_10
  $ dune exec anf_conv_test << EOF
  > let fac n = 
  >   let rec fack n = if (n < 1) then n else n * (fack (n - 1)) in
  >   (fack n)
  > ;;
  > EOF
  let fack n=
  	let anf_op_1=(n<1)
  	in
  	let anf_if_2=if (anf_op_1)
  		then (
  			n
  		) else (
  			let anf_op_3=(n-1)
  			in
  			let anf_app_4=(fack anf_op_3)
  			in
  			let anf_op_5=(n*anf_app_4)
  			in
  			anf_op_5)
  	in
  	anf_if_2
  
  let fac n=
  	let anf_app_6=(fack n)
  	in
  	anf_app_6
  $ dune exec anf_conv_test << EOF
  > let f a =
  >   let g c d =
  >     let h e = a * (c + d * e) in
  >     (h 4)
  >   in
  >   (g 2 3)
  > EOF
  let h c d a e=
  	let anf_op_1=(d*e)
  	in
  	let anf_op_2=(c+anf_op_1)
  	in
  	let anf_op_3=(a*anf_op_2)
  	in
  	anf_op_3
  
  let g a c d=
  	let anf_app_4=(h c d a 4)
  	in
  	anf_app_4
  
  let f a=
  	let anf_app_5=(g a 2 3)
  	in
  	anf_app_5
  $ dune exec anf_conv_test < manytests/do_not_type/001.ml
  let recfac n=
  	let anf_op_1=(n<=1)
  	in
  	let anf_if_2=if (anf_op_1)
  		then (
  			1
  		) else (
  			let anf_op_3=(n-1)
  			in
  			let anf_app_4=(fac anf_op_3)
  			in
  			let anf_op_5=(n*anf_app_4)
  			in
  			anf_op_5)
  	in
  	anf_if_2
  $ dune exec anf_conv_test < manytests/do_not_type/002if.ml
  let main =
  	let anf_if_1=if (true)
  		then (
  			1
  		) else (
  			false)
  	in
  	anf_if_1
  $ dune exec anf_conv_test < manytests/do_not_type/003occurs.ml
  let anon_2 x f=
  	let anf_app_1=(x x)
  	in
  	let anf_app_2=(anf_app_1 f)
  	in
  	anf_app_2
  
  let anon_1 f x=
  	let anf_app_3=(anon_2 x f)
  	in
  	let anf_app_4=(f anf_app_3)
  	in
  	anf_app_4
  
  let fix f=
  	let anf_app_5=(anon_1 f)
  	in
  	anf_app_5
  
  let anon_4 x f=
  	let anf_app_6=(x x)
  	in
  	let anf_app_7=(anf_app_6 f)
  	in
  	anf_app_7
  
  let anon_3 x=
  	let anf_app_8=(anon_4 x)
  	in
  	let anf_app_9=(f anf_app_8)
  	in
  	anf_app_9
  $ dune exec anf_conv_test < manytests/typed/001fac.ml
  let fac n=
  	let anf_op_1=(n<=1)
  	in
  	let anf_if_2=if (anf_op_1)
  		then (
  			1
  		) else (
  			let anf_op_3=(n-1)
  			in
  			let anf_app_4=(fac anf_op_3)
  			in
  			let anf_op_5=(n*anf_app_4)
  			in
  			anf_op_5)
  	in
  	anf_if_2
  
  let main =
  	let anf_app_6=(fac 4)
  	in
  	let anf_app_7=(print_int anf_app_6)
  	in
  	let ()=anf_app_7
  	in
  	0
  $ dune exec anf_conv_test < manytests/typed/002fac.ml
  let anon_1 n k p=
  	let anf_op_1=(p*n)
  	in
  	let anf_app_2=(k anf_op_1)
  	in
  	anf_app_2
  
  let fac_cps n k=
  	let anf_op_3=(n=1)
  	in
  	let anf_if_4=if (anf_op_3)
  		then (
  			let anf_app_5=(k 1)
  			in
  			anf_app_5
  		) else (
  			let anf_op_6=(n-1)
  			in
  			let anf_app_7=(anon_1 n k)
  			in
  			let anf_app_8=(fac_cps anf_op_6 anf_app_7)
  			in
  			anf_app_8)
  	in
  	anf_if_4
  
  let anon_2 print_int=
  	print_int
  
  let main =
  	let anf_app_9=(anon_2 )
  	in
  	let anf_app_10=(fac_cps 4 anf_app_9)
  	in
  	let anf_app_11=(print_int anf_app_10)
  	in
  	let ()=anf_app_11
  	in
  	0
  $ dune exec anf_conv_test < manytests/typed/003fib.ml
  let n1 a b n=
  	let anf_op_1=(n-1)
  	in
  	anf_op_1
  
  let ab a b n=
  	let anf_op_2=(a+b)
  	in
  	anf_op_2
  
  let fib_acc a b n=
  	let anf_op_3=(n=1)
  	in
  	let anf_if_4=if (anf_op_3)
  		then (
  			b
  		) else (
  			let anf_app_5=(ab a b n)
  			in
  			let anf_app_6=(n1 a b n)
  			in
  			let anf_app_7=(fib_acc b anf_app_5 anf_app_6)
  			in
  			anf_app_7)
  	in
  	anf_if_4
  
  let fib n=
  	let anf_op_8=(n<2)
  	in
  	let anf_if_9=if (anf_op_8)
  		then (
  			n
  		) else (
  			let anf_op_10=(n-1)
  			in
  			let anf_op_11=(n-2)
  			in
  			let anf_app_12=(fib anf_op_11)
  			in
  			let anf_op_13=(anf_op_10+anf_app_12)
  			in
  			let anf_app_14=(fib anf_op_13)
  			in
  			anf_app_14)
  	in
  	anf_if_9
  
  let main =
  	let anf_app_15=(fib_acc 0 1 4)
  	in
  	let anf_app_16=(print_int anf_app_15)
  	in
  	let ()=anf_app_16
  	in
  	let anf_app_17=(fib 4)
  	in
  	let anf_app_18=(print_int anf_app_17)
  	in
  	let ()=anf_app_18
  	in
  	0
  $ dune exec anf_conv_test < manytests/typed/004manyargs.ml
  let wrap f=
  	let anf_op_1=(1=1)
  	in
  	let anf_if_2=if (anf_op_1)
  		then (
  			f
  		) else (
  			f)
  	in
  	anf_if_2
  
  let a_0 a b c=
  	let anf_app_3=(print_int a)
  	in
  	anf_app_3
  
  let b_0 a b c=
  	let anf_app_4=(print_int b)
  	in
  	anf_app_4
  
  let c_0 a b c=
  	let anf_app_5=(print_int c)
  	in
  	anf_app_5
  
  let test3 a b c=
  	0
  
  let test10 a b c d e f g h i j=
  	let anf_op_6=(a+b)
  	in
  	let anf_op_7=(anf_op_6+c)
  	in
  	let anf_op_8=(anf_op_7+d)
  	in
  	let anf_op_9=(anf_op_8+e)
  	in
  	let anf_op_10=(anf_op_9+f)
  	in
  	let anf_op_11=(anf_op_10+g)
  	in
  	let anf_op_12=(anf_op_11+h)
  	in
  	let anf_op_13=(anf_op_12+i)
  	in
  	let anf_op_14=(anf_op_13+j)
  	in
  	anf_op_14
  
  let main =
  	let anf_app_15=(test10 )
  	in
  	let anf_app_16=(wrap anf_app_15)
  	in
  	let anf_app_17=(anf_app_16 1)
  	in
  	let anf_app_18=(anf_app_17 10)
  	in
  	let anf_app_19=(anf_app_18 100)
  	in
  	let anf_app_20=(anf_app_19 1000)
  	in
  	let anf_app_21=(anf_app_20 10000)
  	in
  	let anf_app_22=(anf_app_21 100000)
  	in
  	let anf_app_23=(anf_app_22 1000000)
  	in
  	let anf_app_24=(anf_app_23 10000000)
  	in
  	let anf_app_25=(anf_app_24 100000000)
  	in
  	let anf_app_26=(anf_app_25 1000000000)
  	in
  	let rez=anf_app_26
  	in
  	let anf_app_27=(print_int rez)
  	in
  	let ()=anf_app_27
  	in
  	let anf_app_28=(test3 )
  	in
  	let anf_app_29=(wrap anf_app_28)
  	in
  	let anf_app_30=(anf_app_29 1)
  	in
  	let anf_app_31=(anf_app_30 10)
  	in
  	let anf_app_32=(anf_app_31 100)
  	in
  	let temp2=anf_app_32
  	in
  	0
  $ dune exec anf_conv_test < manytests/typed/005fix.ml
  let fix f x=
  	let anf_app_1=(fix f)
  	in
  	let anf_app_2=(f anf_app_1)
  	in
  	let anf_app_3=(anf_app_2 x)
  	in
  	anf_app_3
  
  let fac self n=
  	let anf_op_4=(n<=1)
  	in
  	let anf_if_5=if (anf_op_4)
  		then (
  			1
  		) else (
  			let anf_op_6=(n-1)
  			in
  			let anf_app_7=(self anf_op_6)
  			in
  			let anf_op_8=(n*anf_app_7)
  			in
  			anf_op_8)
  	in
  	anf_if_5
  
  let main =
  	let anf_app_9=(fac )
  	in
  	let anf_app_10=(fix anf_app_9 6)
  	in
  	let anf_app_11=(print_int anf_app_10)
  	in
  	let ()=anf_app_11
  	in
  	0
  $ dune exec anf_conv_test < manytests/typed/006partial.ml
  let anon_1 b foo=
  	let anf_op_1=(foo+2)
  	in
  	anf_op_1
  
  let anon_2 b foo=
  	let anf_op_2=(foo*10)
  	in
  	anf_op_2
  
  let foo b=
  	let anf_if_3=if (b)
  		then (
  			let anf_app_4=(anon_1 b)
  			in
  			anf_app_4
  		) else (
  			let anf_app_5=(anon_2 b)
  			in
  			anf_app_5)
  	in
  	anf_if_3
  
  let foo_0 x=
  	let anf_app_6=(foo true)
  	in
  	let anf_app_7=(foo false)
  	in
  	let anf_app_8=(foo true)
  	in
  	let anf_app_9=(foo false)
  	in
  	let anf_app_10=(anf_app_9 x)
  	in
  	let anf_app_11=(anf_app_8 anf_app_10)
  	in
  	let anf_app_12=(anf_app_7 anf_app_11)
  	in
  	let anf_app_13=(anf_app_6 anf_app_12)
  	in
  	anf_app_13
  
  let main =
  	let anf_app_14=(foo_0 11)
  	in
  	let anf_app_15=(print_int anf_app_14)
  	in
  	let ()=anf_app_15
  	in
  	0
  $ dune exec anf_conv_test < manytests/typed/006partial2.ml
  let foo a b c=
  	let anf_app_1=(print_int a)
  	in
  	let ()=anf_app_1
  	in
  	let anf_app_2=(print_int b)
  	in
  	let ()=anf_app_2
  	in
  	let anf_app_3=(print_int c)
  	in
  	let ()=anf_app_3
  	in
  	let anf_op_4=(b*c)
  	in
  	let anf_op_5=(a+anf_op_4)
  	in
  	anf_op_5
  
  let main =
  	let anf_app_6=(foo 1)
  	in
  	let foo_0=anf_app_6
  	in
  	let anf_app_7=(foo_0 2)
  	in
  	let foo_0_2=anf_app_7
  	in
  	let anf_app_8=(foo_0_2 3)
  	in
  	let foo_0_2_4=anf_app_8
  	in
  	let anf_app_9=(print_int foo_0_2_4)
  	in
  	let ()=anf_app_9
  	in
  	0
  $ dune exec anf_conv_test < manytests/typed/006partial3.ml
  let anon_2 b a c=
  	let anf_app_1=(print_int c)
  	in
  	anf_app_1
  
  let anon_1 a b=
  	let anf_app_2=(print_int b)
  	in
  	let ()=anf_app_2
  	in
  	let anf_app_3=(anon_2 b a)
  	in
  	anf_app_3
  
  let foo a=
  	let anf_app_4=(print_int a)
  	in
  	let ()=anf_app_4
  	in
  	let anf_app_5=(anon_1 a)
  	in
  	anf_app_5
  
  let main =
  	let anf_app_6=(foo 4)
  	in
  	let anf_app_7=(anf_app_6 8)
  	in
  	let anf_app_8=(anf_app_7 9)
  	in
  	let ()=anf_app_8
  	in
  	0
  $ dune exec anf_conv_test << EOF
  > let f a =
  >   let g c d =
  >     let h e = a * (c + d * e) in
  >     (h 4)
  >   in
  >   (g 2 3)
  > EOF
  let h c d a e=
  	let anf_op_1=(d*e)
  	in
  	let anf_op_2=(c+anf_op_1)
  	in
  	let anf_op_3=(a*anf_op_2)
  	in
  	anf_op_3
  
  let g a c d=
  	let anf_app_4=(h c d a 4)
  	in
  	anf_app_4
  
  let f a=
  	let anf_app_5=(g a 2 3)
  	in
  	anf_app_5
