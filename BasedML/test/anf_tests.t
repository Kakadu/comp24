  $ dune exec ./anf_demo.exe << EOF
  > let nested1 = let nested2 = 5 in 
  > let nested3 = 6 in
  > let nested4 x = x + (fun i -> nested2 + nested3) 8 in nested4 55
  > EOF
  let  ll_1 ( + ) nested2 nested3 i  = let anf_app_0 = ( + ) nested2 in
   let anf_app_1 = anf_app_0 nested3 in
   anf_app_1
  let  ll_0 ( + ) nested2 nested3 x  = let anf_app_0 = ( + ) x in
   let anf_app_1 = ll_1 ( + ) in
   let anf_app_2 = anf_app_1 nested2 in
   let anf_app_3 = anf_app_2 nested3 in
   let anf_app_4 = anf_app_3 8 in
   let anf_app_5 = anf_app_0 anf_app_4 in
   anf_app_5
  let  nested1  = let nested2 = 5 in
   let nested3 = 6 in
   let anf_app_0 = ll_0 ( + ) in
   let anf_app_1 = anf_app_0 nested2 in
   let anf_app_2 = anf_app_1 nested3 in
   let anf_app_3 = anf_app_2 55 in
   anf_app_3

  $ dune exec ./anf_demo.exe << EOF
  > let rec fact_cps n cont =
  > if (n = 0) then
  >  cont 1
  > else
  >  fact_cps (n - 1) (fun acc -> cont (n * acc))
  > EOF
  let  ll_0 ( * ) cont n acc  = let anf_app_0 = ( * ) n in
   let anf_app_1 = anf_app_0 acc in
   let anf_app_2 = cont anf_app_1 in
   anf_app_2
  let rec fact_cps n cont  = let anf_app_0 = ( = ) n in
   let anf_app_1 = anf_app_0 0 in
   let anf_ifthenelse_10 = if anf_app_1 then let anf_app_2 = cont 1 in
   anf_app_2 else let anf_app_3 = ( - ) n in
   let anf_app_4 = anf_app_3 1 in
   let anf_app_5 = fact_cps anf_app_4 in
   let anf_app_6 = ll_0 ( * ) in
   let anf_app_7 = anf_app_6 cont in
   let anf_app_8 = anf_app_7 n in
   let anf_app_9 = anf_app_5 anf_app_8 in
   anf_app_9 in
   anf_ifthenelse_10

  $ dune exec ./anf_demo.exe << EOF
  > let rec fib_cps n cont =
  > if (n = 0) then
  >  cont 0
  > else if (n = 1) then
  >   cont 1
  > else
  >   fib_cps (n - 1) (fun a ->
  >   fib_cps (n - 2) (fun b ->
  >   cont (a + b)))
  > EOF
  let  ll_1 ( + ) a cont b  = let anf_app_0 = ( + ) a in
   let anf_app_1 = anf_app_0 b in
   let anf_app_2 = cont anf_app_1 in
   anf_app_2
  let  ll_0 ( + ) ( - ) cont fib_cps n a  = let anf_app_0 = ( - ) n in
   let anf_app_1 = anf_app_0 2 in
   let anf_app_2 = fib_cps anf_app_1 in
   let anf_app_3 = ll_1 ( + ) in
   let anf_app_4 = anf_app_3 a in
   let anf_app_5 = anf_app_4 cont in
   let anf_app_6 = anf_app_2 anf_app_5 in
   anf_app_6
  let rec fib_cps n cont  = let anf_app_0 = ( = ) n in
   let anf_app_1 = anf_app_0 0 in
   let anf_ifthenelse_16 = if anf_app_1 then let anf_app_2 = cont 0 in
   anf_app_2 else let anf_app_3 = ( = ) n in
   let anf_app_4 = anf_app_3 1 in
   let anf_ifthenelse_15 = if anf_app_4 then let anf_app_5 = cont 1 in
   anf_app_5 else let anf_app_6 = ( - ) n in
   let anf_app_7 = anf_app_6 1 in
   let anf_app_8 = fib_cps anf_app_7 in
   let anf_app_9 = ll_0 ( + ) in
   let anf_app_10 = anf_app_9 ( - ) in
   let anf_app_11 = anf_app_10 cont in
   let anf_app_12 = anf_app_11 fib_cps in
   let anf_app_13 = anf_app_12 n in
   let anf_app_14 = anf_app_8 anf_app_13 in
   anf_app_14 in
   anf_ifthenelse_15 in
   anf_ifthenelse_16

  $ dune exec ./anf_demo.exe << EOF
  > let test = let test5   = (5 + 5, 6 + 6, 7 + 7 * 8) in test5
  > EOF
  let  test  = let anf_app_0 = ( + ) 5 in
   let anf_app_1 = anf_app_0 5 in
   let anf_app_2 = ( + ) 6 in
   let anf_app_3 = anf_app_2 6 in
   let anf_app_4 = ( + ) 7 in
   let anf_app_5 = ( * ) 7 in
   let anf_app_6 = anf_app_5 8 in
   let anf_app_7 = anf_app_4 anf_app_6 in
   let anf_tuple_8 = (anf_app_1, anf_app_3, anf_app_7) in
   let test5 = anf_tuple_8 in
   test5

