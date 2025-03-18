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
