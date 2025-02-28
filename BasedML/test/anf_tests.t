  $ dune exec ./anf_demo.exe << EOF
  > let test = let nested_pat x = match x with | 5 -> a 5 | 6 -> 6 in nested_pat 7 
  > EOF
  let  ll_0 unbound_a_0 x_0  = let anf_matching_1 = match x_0 with
  | 5 -> let anf_app_0 = unbound_a_0 5 in
   anf_app_0
  | 6 -> 6 in
   anf_matching_1;;
  let  test_0  = let anf_app_0 = ll_0 unbound_a_0 7 in
   anf_app_0;;

  $ dune exec ./anf_demo.exe << EOF
  > let test = let nested_pat x = if x 5 then 5 else 6 in nested_pat 7 
  > EOF
  let  ll_0 x_0  = let anf_app_0 = x_0 5 in
   let anf_ifthenelse_1 = if anf_app_0 then 5 else 6 in
   anf_ifthenelse_1;;
  let  test_0  = let anf_app_0 = ll_0 7 in
   anf_app_0;;

  $ dune exec ./anf_demo.exe << EOF
  > let rec fact_cps n cont =
  > if (n = 0) then
  >  cont 1
  > else
  >  fact_cps (n - 1) (fun acc -> cont (n * acc))
  > EOF
  let  ll_0 cont_0 n_0 acc_0  = let anf_app_0 = ( * ) n_0 acc_0 in
   let anf_app_1 = cont_0 anf_app_0 in
   anf_app_1;;
  let rec fact_cps_0 n_0 cont_0  = let anf_app_0 = ( = ) n_0 0 in
   let anf_ifthenelse_5 = if anf_app_0 then let anf_app_1 = cont_0 1 in
   anf_app_1 else let anf_app_2 = ll_0 cont_0 n_0 in
   let anf_app_3 = ( - ) n_0 1 in
   let anf_app_4 = fact_cps_0 anf_app_3 anf_app_2 in
   anf_app_4 in
   anf_ifthenelse_5;;

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
  let  ll_1 a_0 cont_0 b_0  = let anf_app_0 = ( + ) a_0 b_0 in
   let anf_app_1 = cont_0 anf_app_0 in
   anf_app_1;;
  let  ll_0 cont_0 fib_cps_0 n_0 a_0  = let anf_app_0 = ll_1 a_0 cont_0 in
   let anf_app_1 = ( - ) n_0 2 in
   let anf_app_2 = fib_cps_0 anf_app_1 anf_app_0 in
   anf_app_2;;
  let rec fib_cps_0 n_0 cont_0  = let anf_app_0 = ( = ) n_0 0 in
   let anf_ifthenelse_8 = if anf_app_0 then let anf_app_1 = cont_0 0 in
   anf_app_1 else let anf_app_2 = ( = ) n_0 1 in
   let anf_ifthenelse_7 = if anf_app_2 then let anf_app_3 = cont_0 1 in
   anf_app_3 else let anf_app_4 = ll_0 cont_0 fib_cps_0 n_0 in
   let anf_app_5 = ( - ) n_0 1 in
   let anf_app_6 = fib_cps_0 anf_app_5 anf_app_4 in
   anf_app_6 in
   anf_ifthenelse_7 in
   anf_ifthenelse_8;;

  $ dune exec ./anf_demo.exe << EOF
  > let test = let test5   = (5 + 5, 6 + 6, 7 + 7 * 8) in test5
  > EOF
  let  test_0  = let anf_app_0 = ( + ) 5 5 in
   let anf_app_1 = ( + ) 6 6 in
   let anf_app_2 = ( * ) 7 8 in
   let anf_app_3 = ( + ) 7 anf_app_2 in
   let anf_tuple_4 = (anf_app_0, anf_app_1, anf_app_3) in
   let test5_0 = anf_tuple_4 in
   test5_0;;

  $ dune exec ./anf_demo.exe < ./manytests/typed/001fac.ml
  let rec fac_0 n_0  = let anf_app_0 = ( <= ) n_0 1 in
   let anf_ifthenelse_4 = if anf_app_0 then 1 else let anf_app_1 = ( - ) n_0 1 in
   let anf_app_2 = fac_0 anf_app_1 in
   let anf_app_3 = ( * ) n_0 anf_app_2 in
   anf_app_3 in
   anf_ifthenelse_4;;
  let  main_0  = let anf_app_0 = fac_0 4 in
   let anf_app_1 = print_int anf_app_0 in
   let () = anf_app_1 in
   0;;

  $ dune exec ./anf_demo.exe < ./manytests/typed/002fac.ml
  let  ll_0 k_0 n_0 p_0  = let anf_app_0 = ( * ) p_0 n_0 in
   let anf_app_1 = k_0 anf_app_0 in
   anf_app_1;;
  let rec fac_cps_0 n_0 k_0  = let anf_app_0 = ( = ) n_0 1 in
   let anf_ifthenelse_5 = if anf_app_0 then let anf_app_1 = k_0 1 in
   anf_app_1 else let anf_app_2 = ll_0 k_0 n_0 in
   let anf_app_3 = ( - ) n_0 1 in
   let anf_app_4 = fac_cps_0 anf_app_3 anf_app_2 in
   anf_app_4 in
   anf_ifthenelse_5;;
  let  ll_1 print_int_0  = print_int_0;;
  let  main_0  = let anf_app_0 = fac_cps_0 4 ll_1 in
   let anf_app_1 = print_int anf_app_0 in
   let () = anf_app_1 in
   0;;

  $ dune exec ./anf_demo.exe < ./manytests/typed/003fib.ml
  let rec fib_acc_0 a_0 b_0 n_0  = let anf_app_0 = ( = ) n_0 1 in
   let anf_ifthenelse_4 = if anf_app_0 then b_0 else let anf_app_1 = ( - ) n_0 1 in
   let n1_0 = anf_app_1 in
   let anf_app_2 = ( + ) a_0 b_0 in
   let ab_0 = anf_app_2 in
   let anf_app_3 = fib_acc_0 b_0 ab_0 n1_0 in
   anf_app_3 in
   anf_ifthenelse_4;;
  let rec fib_0 n_0  = let anf_app_0 = ( < ) n_0 2 in
   let anf_ifthenelse_6 = if anf_app_0 then n_0 else let anf_app_1 = ( - ) n_0 2 in
   let anf_app_2 = fib_0 anf_app_1 in
   let anf_app_3 = ( - ) n_0 1 in
   let anf_app_4 = fib_0 anf_app_3 in
   let anf_app_5 = ( + ) anf_app_4 anf_app_2 in
   anf_app_5 in
   anf_ifthenelse_6;;
  let  main_0  = let anf_app_0 = fib_acc_0 0 1 4 in
   let anf_app_1 = print_int anf_app_0 in
   let () = anf_app_1 in
   let anf_app_2 = fib_0 4 in
   let anf_app_3 = print_int anf_app_2 in
   let () = anf_app_3 in
   0;;

  $ dune exec ./anf_demo.exe < ./manytests/typed/004manyargs.ml
  let  wrap_0 f_0  = let anf_app_0 = ( = ) 1 1 in
   let anf_ifthenelse_1 = if anf_app_0 then f_0 else f_0 in
   anf_ifthenelse_1;;
  let  test3_0 a_0 b_0 c_0  = let anf_app_0 = print_int a_0 in
   let a_1 = anf_app_0 in
   let anf_app_1 = print_int b_0 in
   let b_1 = anf_app_1 in
   let anf_app_2 = print_int c_0 in
   let c_1 = anf_app_2 in
   0;;
  let  test10_0 a_2 b_2 c_2 d_0 e_0 f_1 g_0 h_0 i_0 j_0  = let anf_app_0 = ( + ) a_2 b_2 in
   let anf_app_1 = ( + ) anf_app_0 c_2 in
   let anf_app_2 = ( + ) anf_app_1 d_0 in
   let anf_app_3 = ( + ) anf_app_2 e_0 in
   let anf_app_4 = ( + ) anf_app_3 f_1 in
   let anf_app_5 = ( + ) anf_app_4 g_0 in
   let anf_app_6 = ( + ) anf_app_5 h_0 in
   let anf_app_7 = ( + ) anf_app_6 i_0 in
   let anf_app_8 = ( + ) anf_app_7 j_0 in
   anf_app_8;;
  let  main_0  = let anf_app_0 = wrap_0 test10_0 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000 in
   let rez_0 = anf_app_0 in
   let anf_app_1 = print_int rez_0 in
   let () = anf_app_1 in
   let anf_app_2 = wrap_0 test3_0 1 10 100 in
   let temp2_0 = anf_app_2 in
   0;;

  $ dune exec ./anf_demo.exe < ./manytests/typed/005fix.ml
  let rec fix_0 f_0 x_0  = let anf_app_0 = fix_0 f_0 in
   let anf_app_1 = f_0 anf_app_0 x_0 in
   anf_app_1;;
  let  fac_0 self_0 n_0  = let anf_app_0 = ( <= ) n_0 1 in
   let anf_ifthenelse_4 = if anf_app_0 then 1 else let anf_app_1 = ( - ) n_0 1 in
   let anf_app_2 = self_0 anf_app_1 in
   let anf_app_3 = ( * ) n_0 anf_app_2 in
   anf_app_3 in
   anf_ifthenelse_4;;
  let  main_0  = let anf_app_0 = fix_0 fac_0 6 in
   let anf_app_1 = print_int anf_app_0 in
   let () = anf_app_1 in
   0;;

  $ dune exec ./anf_demo.exe < ./manytests/typed/006partial.ml
  let  ll_0 foo_1  = let anf_app_0 = ( + ) foo_1 2 in
   anf_app_0;;
  let  ll_1 foo_2  = let anf_app_0 = ( * ) foo_2 10 in
   anf_app_0;;
  let  foo_0 b_0  = let anf_ifthenelse_0 = if b_0 then ll_0 else ll_1 in
   anf_ifthenelse_0;;
  let  foo_3 x_0  = let anf_app_0 = foo_0 false x_0 in
   let anf_app_1 = foo_0 true anf_app_0 in
   let anf_app_2 = foo_0 false anf_app_1 in
   let anf_app_3 = foo_0 true anf_app_2 in
   anf_app_3;;
  let  main_0  = let anf_app_0 = foo_3 11 in
   let anf_app_1 = print_int anf_app_0 in
   let () = anf_app_1 in
   0;;

  $ dune exec ./anf_demo.exe < ./manytests/typed/006partial2.ml
  let  foo_0 a_0 b_0 c_0  = let anf_app_0 = print_int a_0 in
   let () = anf_app_0 in
   let anf_app_1 = print_int b_0 in
   let () = anf_app_1 in
   let anf_app_2 = print_int c_0 in
   let () = anf_app_2 in
   let anf_app_3 = ( * ) b_0 c_0 in
   let anf_app_4 = ( + ) a_0 anf_app_3 in
   anf_app_4;;
  let  main_0  = let anf_app_0 = foo_0 1 in
   let foo_1 = anf_app_0 in
   let anf_app_1 = foo_1 2 in
   let foo_2 = anf_app_1 in
   let anf_app_2 = foo_2 3 in
   let foo_3 = anf_app_2 in
   let anf_app_3 = print_int foo_3 in
   let () = anf_app_3 in
   0;;

  $ dune exec ./anf_demo.exe < ./manytests/typed/006partial3.ml
  let  ll_1 c_0  = let anf_app_0 = print_int c_0 in
   anf_app_0;;
  let  ll_0 b_0  = let anf_app_0 = print_int b_0 in
   let () = anf_app_0 in
   ll_1;;
  let  foo_0 a_0  = let anf_app_0 = print_int a_0 in
   let () = anf_app_0 in
   ll_0;;
  let  main_0  = let anf_app_0 = foo_0 4 8 9 in
   let () = anf_app_0 in
   0;;

  $ dune exec ./anf_demo.exe < ./manytests/typed/007order.ml
  let  _start_0 () () a_0 () b_0 _c_0 () d_0 ___0  = let anf_app_0 = ( + ) a_0 b_0 in
   let anf_app_1 = print_int anf_app_0 in
   let () = anf_app_1 in
   let anf_app_2 = print_int ___0 in
   let () = anf_app_2 in
   let anf_app_3 = ( * ) a_0 b_0 in
   let anf_app_4 = ( / ) anf_app_3 _c_0 in
   let anf_app_5 = ( + ) anf_app_4 d_0 in
   anf_app_5;;
  let  main_0  = let anf_app_0 = print_int (-1) in
   let anf_app_1 = print_int 4 in
   let anf_app_2 = print_int 2 in
   let anf_app_3 = print_int 1 in
   let anf_app_4 = _start_0 anf_app_3 anf_app_2 3 anf_app_1 100 1000 anf_app_0 10000 (-555555) in
   let anf_app_5 = print_int anf_app_4 in
   anf_app_5;;

  $ dune exec ./anf_demo.exe < ./manytests/typed/008ascription.ml
  let  addi_0 f_0 g_0 x_0  = let anf_app_0 = g_0 x_0 in
   let anf_constraint_1 = (anf_app_0 : bool) in
   let anf_app_2 = f_0 x_0 anf_constraint_1 in
   let anf_constraint_3 = (anf_app_2 : int) in
   anf_constraint_3;;
  let  ll_0 x_1 b_0  = let anf_ifthenelse_2 = if b_0 then let anf_app_0 = ( + ) x_1 1 in
   anf_app_0 else let anf_app_1 = ( * ) x_1 2 in
   anf_app_1 in
   anf_ifthenelse_2;;
  let  ll_1 _start_0  = let anf_app_0 = ( / ) _start_0 2 in
   let anf_app_1 = ( = ) anf_app_0 0 in
   anf_app_1;;
  let  main_0  = let anf_app_0 = addi_0 ll_0 ll_1 4 in
   let anf_app_1 = print_int anf_app_0 in
   let () = anf_app_1 in
   0;;

  $ dune exec ./anf_demo.exe < ./manytests/typed/009let_poly.ml
  let  ll_0 x_0  = x_0;;
  let  temp_0  = let anf_app_0 = ll_0 1 in
   let anf_app_1 = ll_0 true in
   let anf_tuple_2 = (anf_app_0, anf_app_1) in
   anf_tuple_2;;

  $ dune exec ./anf_demo.exe < ./manytests/typed/015tuples.ml
  let rec fix_0 f_0 x_0  = let anf_app_0 = fix_0 f_0 in
   let anf_app_1 = f_0 anf_app_0 x_0 in
   anf_app_1;;
  let  map_0 f_0 p_0  = let (a_0, b_0) = p_0 in
   let anf_app_0 = f_0 a_0 in
   let anf_app_1 = f_0 b_0 in
   let anf_tuple_2 = (anf_app_0, anf_app_1) in
   anf_tuple_2;;
  let  ll_1 l_1 self_0 li_0 x_0  = let anf_app_0 = self_0 l_1 in
   let anf_app_1 = li_0 anf_app_0 x_0 in
   anf_app_1;;
  let  ll_0 self_0 l_1  = let anf_app_0 = ll_1 l_1 self_0 in
   let anf_app_1 = map_0 anf_app_0 l_1 in
   anf_app_1;;
  let  fixpoly_0 l_0  = let anf_app_0 = fix_0 ll_0 l_0 in
   anf_app_0;;
  let  feven_0 p_1 n_0  = let (e_0, o_0) = p_1 in
   let anf_app_0 = ( == ) n_0 0 in
   let anf_ifthenelse_3 = if anf_app_0 then 1 else let anf_app_1 = ( - ) n_0 1 in
   let anf_app_2 = o_0 anf_app_1 in
   anf_app_2 in
   anf_ifthenelse_3;;
  let  fodd_0 p_2 n_1  = let (e_1, o_1) = p_2 in
   let anf_app_0 = ( == ) n_1 0 in
   let anf_ifthenelse_3 = if anf_app_0 then 0 else let anf_app_1 = ( - ) n_1 1 in
   let anf_app_2 = e_1 anf_app_1 in
   anf_app_2 in
   anf_ifthenelse_3;;
  let  tie_0  = let anf_tuple_0 = (feven_0, fodd_0) in
   let anf_app_1 = fixpoly_0 anf_tuple_0 in
   anf_app_1;;
  let rec  meven_0 n_2  = let anf_app_0 = ( = ) n_2 0 in
   let anf_ifthenelse_3 = if anf_app_0 then 1 else let anf_app_1 = ( - ) n_2 1 in
   let anf_app_2 = modd_0 anf_app_1 in
   anf_app_2 in
   anf_ifthenelse_3  and  modd_0 n_3  = let anf_app_4 = ( = ) n_3 0 in
   let anf_ifthenelse_7 = if anf_app_4 then 1 else let anf_app_5 = ( - ) n_3 1 in
   let anf_app_6 = meven_0 anf_app_5 in
   anf_app_6 in
   anf_ifthenelse_7 
  let  main_0  = let anf_app_0 = modd_0 1 in
   let anf_app_1 = print_int anf_app_0 in
   let () = anf_app_1 in
   let anf_app_2 = meven_0 2 in
   let anf_app_3 = print_int anf_app_2 in
   let () = anf_app_3 in
   let (even_0, odd_0) = tie_0 in
   let anf_app_4 = odd_0 3 in
   let anf_app_5 = print_int anf_app_4 in
   let () = anf_app_5 in
   let anf_app_6 = even_0 4 in
   let anf_app_7 = print_int anf_app_6 in
   let () = anf_app_7 in
   0;;

  $ dune exec ./anf_demo.exe < ./manytests/typed/016lists.ml
  let rec length_0 xs_0  = let anf_matching_2 = match xs_0 with
  | [] -> 0
  | (h_0 :: tl_0) -> let anf_app_0 = length_0 tl_0 in
   let anf_app_1 = ( + ) 1 anf_app_0 in
   anf_app_1 in
   anf_matching_2;;
  let rec ll_0 acc_0 xs_0  = let anf_matching_2 = match xs_0 with
  | [] -> acc_0
  | (h_0 :: tl_0) -> let anf_app_0 = ( + ) acc_0 1 in
   let anf_app_1 = ll_0 anf_app_0 tl_0 in
   anf_app_1 in
   anf_matching_2;;
  let  length_tail_0  = let anf_app_0 = ll_0 0 in
   anf_app_0;;
  let rec map_0 f_0 xs_0  = let anf_matching_21 = match xs_0 with
  | [] -> []
  | (a_0 :: []) -> let anf_app_0 = f_0 a_0 in
   let anf_app_1 = ( :: ) anf_app_0 [] in
   anf_app_1
  | (a_1 :: (b_0 :: [])) -> let anf_app_2 = f_0 b_0 in
   let anf_app_3 = ( :: ) anf_app_2 [] in
   let anf_app_4 = f_0 a_1 in
   let anf_app_5 = ( :: ) anf_app_4 anf_app_3 in
   anf_app_5
  | (a_2 :: (b_1 :: (c_0 :: []))) -> let anf_app_6 = f_0 c_0 in
   let anf_app_7 = ( :: ) anf_app_6 [] in
   let anf_app_8 = f_0 b_1 in
   let anf_app_9 = ( :: ) anf_app_8 anf_app_7 in
   let anf_app_10 = f_0 a_2 in
   let anf_app_11 = ( :: ) anf_app_10 anf_app_9 in
   anf_app_11
  | (a_3 :: (b_2 :: (c_1 :: (d_0 :: tl_0)))) -> let anf_app_12 = map_0 f_0 tl_0 in
   let anf_app_13 = f_0 d_0 in
   let anf_app_14 = ( :: ) anf_app_13 anf_app_12 in
   let anf_app_15 = f_0 c_1 in
   let anf_app_16 = ( :: ) anf_app_15 anf_app_14 in
   let anf_app_17 = f_0 b_2 in
   let anf_app_18 = ( :: ) anf_app_17 anf_app_16 in
   let anf_app_19 = f_0 a_3 in
   let anf_app_20 = ( :: ) anf_app_19 anf_app_18 in
   anf_app_20 in
   anf_matching_21;;
  let rec append_0 xs_0 ys_0  = let anf_matching_2 = match xs_0 with
  | [] -> ys_0
  | (x_0 :: xs_1) -> let anf_app_0 = append_0 xs_1 ys_0 in
   let anf_app_1 = ( :: ) x_0 anf_app_0 in
   anf_app_1 in
   anf_matching_2;;
  let rec ll_1 xs_0  = let anf_matching_2 = match xs_0 with
  | [] -> []
  | (h_0 :: tl_0) -> let anf_app_0 = ll_1 tl_0 in
   let anf_app_1 = append_0 h_0 anf_app_0 in
   anf_app_1 in
   anf_matching_2;;
  let  concat_0  = ll_1;;
  let rec iter_0 f_0 xs_0  = let anf_matching_2 = match xs_0 with
  | [] -> ()
  | (h_0 :: tl_0) -> let anf_app_0 = f_0 h_0 in
   let () = anf_app_0 in
   let anf_app_1 = iter_0 f_0 tl_0 in
   anf_app_1 in
   anf_matching_2;;
  let  ll_2 h_0 a_0  = let anf_tuple_0 = (h_0, a_0) in
   anf_tuple_0;;
  let rec cartesian_0 xs_0 ys_0  = let anf_matching_4 = match xs_0 with
  | [] -> []
  | (h_0 :: tl_0) -> let anf_app_0 = cartesian_0 tl_0 ys_0 in
   let anf_app_1 = ll_2 h_0 in
   let anf_app_2 = map_0 anf_app_1 ys_0 in
   let anf_app_3 = append_0 anf_app_2 anf_app_0 in
   anf_app_3 in
   anf_matching_4;;
  let  main_0  = let anf_app_0 = ( :: ) 3 [] in
   let anf_app_1 = ( :: ) 2 anf_app_0 in
   let anf_app_2 = ( :: ) 1 anf_app_1 in
   let anf_app_3 = iter_0 print_int anf_app_2 in
   let () = anf_app_3 in
   let anf_app_4 = ( :: ) 4 [] in
   let anf_app_5 = ( :: ) 3 anf_app_4 in
   let anf_app_6 = ( :: ) 2 anf_app_5 in
   let anf_app_7 = ( :: ) 1 anf_app_6 in
   let anf_app_8 = ( :: ) 2 [] in
   let anf_app_9 = ( :: ) 1 anf_app_8 in
   let anf_app_10 = cartesian_0 anf_app_9 anf_app_7 in
   let anf_app_11 = length_0 anf_app_10 in
   let anf_app_12 = print_int anf_app_11 in
   let () = anf_app_12 in
   0;;

  $ dune exec ./anf_demo.exe << EOF
  > let test a1 a2 a3 = a1 + a2 + a3
  > let test_var = test (5 + 5) (6 + 6) (7 + 7)
  > EOF
  let  test_0 a1_0 a2_0 a3_0  = let anf_app_0 = ( + ) a1_0 a2_0 in
   let anf_app_1 = ( + ) anf_app_0 a3_0 in
   anf_app_1;;
  let  test_var_0  = let anf_app_0 = ( + ) 7 7 in
   let anf_app_1 = ( + ) 6 6 in
   let anf_app_2 = ( + ) 5 5 in
   let anf_app_3 = test_0 anf_app_2 anf_app_1 anf_app_0 in
   anf_app_3;;

  $ dune exec ./anf_demo.exe << EOF
  > let funct a = a + 5
  > let g a = (funct 5)
  > let funct a = a + 500
  > let () = print_int (g 5) 
  > EOF
  let  funct_0 a_0  = let anf_app_0 = ( + ) a_0 5 in
   anf_app_0;;
  let  g_0 a_1  = let anf_app_0 = funct_0 5 in
   anf_app_0;;
  let  funct_1 a_2  = let anf_app_0 = ( + ) a_2 500 in
   anf_app_0;;
  let  ()  = let anf_app_0 = g_0 5 in
   let anf_app_1 = print_int anf_app_0 in
   anf_app_1;;

  $ dune exec ./anf_demo.exe << EOF
  > let rec even n =
  > match n with
  >   | 0 -> true
  >   | x -> odd (x - 1)
  > and odd n =
  > match n with
  >   | 0 -> false
  >   | x -> even (x - 1)
  > EOF
  let rec  even_0 n_0  = let anf_matching_2 = match n_0 with
  | 0 -> true
  | x_0 -> let anf_app_0 = ( - ) x_0 1 in
   let anf_app_1 = odd_0 anf_app_0 in
   anf_app_1 in
   anf_matching_2  and  odd_0 n_1  = let anf_matching_5 = match n_1 with
  | 0 -> false
  | x_1 -> let anf_app_3 = ( - ) x_1 1 in
   let anf_app_4 = even_0 anf_app_3 in
   anf_app_4 in
   anf_matching_5 

  $ dune exec ./anf_demo.exe << EOF
  > let _ = a + b
  > let ( + ) a b = a - b
  > let test_var = test (5 + 5) (6 + 6) (7 + 7)
  > EOF
  let  _  = let anf_app_0 = ( + ) unbound_a_0 unbound_b_0 in
   anf_app_0;;
  let  ( + )_0 a_0 b_0  = let anf_app_0 = ( - ) a_0 b_0 in
   anf_app_0;;
  let  test_var_0  = let anf_app_0 = ( + )_0 7 7 in
   let anf_app_1 = ( + )_0 6 6 in
   let anf_app_2 = ( + )_0 5 5 in
   let anf_app_3 = unbound_test_0 anf_app_2 anf_app_1 anf_app_0 in
   anf_app_3;;
