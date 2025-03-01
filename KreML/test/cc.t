  $ dune exec closure_conv <<- EOF
  > let add x y = x + y
  > let main =
  >   let add5 = add 5 in
  >   let res = add5 10 in
  > 0
  add(x_0 y_1 ) {x_0 + y_1} 
  
  main() {let add5_2 = { name: add, arity: 2 env_size: 0, arrange [ ]}  ([ 
                       5  ]) in 
           let res_3 = add5_2 ([ 10  ]) in  0  } 
  

  $ dune exec closure_conv <<- EOF
  > let f x y z =
  > x + y + z
  > let main = f 3 5 6
  f(x_0 y_1 z_2 ) {let t_0 = x_0 + y_1 in  t_0 + z_2 } 
  
  main() {{ name: f, arity: 3 env_size: 0, arrange [ ]}  ([ 3 5 6  ])} 
  

  $ dune exec closure_conv <<- EOF
  > let f a =
  >  let b = a in
  >  let c  = a in
  >  fun q w e r -> q + e + b + a
  fresh_fun_0(q_3 w_4 e_5 r_6 a_0 b_1 ) {let t_1 = q_3 + e_5 in 
                                          let t_2 = t_1 + b_1 in  t_2 + a_0  } 
  
  f(a_0 ) {let b_1 = a_0 in 
            let c_2 = a_0 in 
             { name: fresh_fun_0, arity: 4 env_size: 2, arrange [4: a_0; 
             5: b_1;  ]}   } 
  

  $ dune exec closure_conv <<- EOF
  > let rec fac x = if x < 1 then 1 else x * fac (x - 1) 
  fac(x_0 ) {let t_0 = x_0 < 1 in 
              if t_0  then 1  else 
               let t_1 = x_0 - 1 in 
                let t_2 = { name: fac, arity: 1 env_size: 0, arrange [ ]}  ([ 
                          t_1  ]) in 
                 x_0 * t_2    } 
  

  $ dune exec closure_conv <<-EOF
  > let f x =
  > let a = 5 in
  > fun y -> x + a + y
  fresh_fun_0(y_2 a_1 x_0 ) {let t_1 = x_0 + a_1 in  t_1 + y_2 } 
  
  f(x_0 ) {let a_1 = 5 in 
            { name: fresh_fun_0, arity: 1 env_size: 2, arrange [1: a_1; 
            2: x_0;  ]}  } 
  
  $ dune exec closure_conv <<- EOF
  > let f x y =
  > let a = 10 in
  > let b = a in
  > fun u v ->  u + v * a
  fresh_fun_0(u_4 v_5 a_2 ) {let t_1 = v_5 * a_2 in  u_4 + t_1 } 
  
  f(x_0 y_1 ) {let a_2 = 10 in 
                let b_3 = a_2 in 
                 { name: fresh_fun_0, arity: 2 env_size: 1, arrange [2: a_2;  ]}   } 
  
  $ dune exec closure_conv <<- EOF
  > let f x =
  >   let a = x in
  >   let b = x in
  >   fun q ->
  >     if q > 0 then a + q
  >     else q - b
  fresh_fun_0(q_3 a_1 b_2 ) {let t_1 = q_3 > 0 in 
                              if t_1  then a_1 + q_3  else  q_3 - b_2  } 
  
  f(x_0 ) {let a_1 = x_0 in 
            let b_2 = x_0 in 
             { name: fresh_fun_0, arity: 1 env_size: 2, arrange [1: a_1; 
             2: b_2;  ]}   } 
  

  $ dune exec closure_conv <<- EOF
  > let fac n =
  >  let rec helper acc n =
  >    if n < 1 then 1 else helper (acc * n) (n - 1) in
  >  helper 1 n
  helper_1(acc_2 n_3 ) {let t_1 = n_3 < 1 in 
                         if t_1  then 1  else 
                          let t_2 = acc_2 * n_3 in 
                           let t_3 = n_3 - 1 in 
                            { name: helper_1, arity: 2 env_size: 0, arrange [ ]}  ([ 
                            t_2 t_3  ])    } 
  
  fac(n_0 ) {{ name: helper_1, arity: 2 env_size: 0, arrange [ ]}  ([ 1 n_0  ])} 
  

  $ dune exec closure_conv <<- EOF
  > let rec is_even n =
  >   if n = 0 then true else is_odd (n - 1)
  > and is_odd n =
  >   if n = 1 then true else is_even (n - 1)
  is_even(n_0 ) {let t_0 = n_0 = 0 in 
                  if t_0  then true  else 
                   let t_1 = n_0 - 1 in 
                    { name: is_odd, arity: 1 env_size: 0, arrange [ ]}  ([ 
                    t_1  ])   } 
  
  is_odd(n_1 ) {let t_3 = n_1 = 1 in 
                 if t_3  then true  else 
                  let t_4 = n_1 - 1 in 
                   { name: is_even, arity: 1 env_size: 0, arrange [ ]}  ([ 
                   t_4  ])   } 
  

  $ dune exec closure_conv <<- EOF
  > let f x =
  >   let a = 5 in
  >   let rec helper y =
  >     if y < 0 then 1 else helper (a + y) in
  >   helper 10 
  helper_2(y_3 a_1 ) {let t_1 = y_3 < 0 in 
                       if t_1  then 1  else 
                        let t_2 = a_1 + y_3 in 
                         { name: helper_2, arity: 1 env_size: 1, arrange [
                         1: a_1;  ]}  ([ t_2  ])   } 
  
  f(x_0 ) {let a_1 = 5 in 
            { name: helper_2, arity: 1 env_size: 1, arrange [1: a_1;  ]}  ([ 
            10  ]) } 
  

  $ dune exec closure_conv <<- EOF
  > let f x =
  >   let a = x + 5 in
  >   let inner y = a + x + y in
  >   inner
  > 
  > let main =
  >   let res = f 10 11 in
  >   let () = print_int res in
  >   0 
  inner_3(y_2 a_1 x_0 ) {let t_0 = a_1 + x_0 in  t_0 + y_2 } 
  
  f(x_0 ) {let a_1 = x_0 + 5 in 
            { name: inner_3, arity: 1 env_size: 2, arrange [1: a_1; 2: x_0;  ]}  } 
  
  main() {let res_4 = { name: f, arity: 1 env_size: 0, arrange [ ]}  ([ 
                      10 11  ]) in 
           { name: print_int, arity: 1 env_size: 0, arrange [ ]}  ([ res_4  ]); 
            0  } 
  

  $ dune exec closure_conv <<- EOF
  > let map_point f p =
  >   let a, b = p in
  >   f a, f b
  map_point(f_0 p_1 ) {let a_2 = getfield 0 p_1 in 
                        let b_3 = getfield 1 p_1 in 
                         let t_0 = f_0 ([ a_2  ]) in 
                          let t_1 = f_0 ([ b_3  ]) in  [ t_0 t_1  ]    } 
  
  $ dune exec closure_conv <<- EOF
  > let add x y = x + y
  > let add5 = add 5
  > let main =
  >   let () = print_int (add5 1) in
  >   0
  add(x_0 y_1 ) {x_0 + y_1} 
  
  add5() {{ name: add, arity: 2 env_size: 0, arrange [ ]}  ([ 5  ])} 
  
  main() {let t_2 = { name: add5, arity: 0 env_size: 0, arrange [ ]}  ([ 1  ]) in 
           { name: print_int, arity: 1 env_size: 0, arrange [ ]}  ([ t_2  ]); 
            0  } 
  

  $ dune exec closure_conv <<- EOF
  > let add x y = x + y
  > let main =
  >   let add5 = add 5 in
  >   let () = print_int (add5 1) in
  >   0
  add(x_0 y_1 ) {x_0 + y_1} 
  
  main() {let add5_2 = { name: add, arity: 2 env_size: 0, arrange [ ]}  ([ 
                       5  ]) in 
           let t_1 = add5_2 ([ 1  ]) in 
            { name: print_int, arity: 1 env_size: 0, arrange [ ]}  ([ t_1  ]); 
             0   } 
  
  $ dune exec closure_conv <<- EOF
  > let adder a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 =
  >   a1 + a2 + a3 + a4 + a5 + a6+ a7+ a8 +a9 + a10
  > let main =
  >   let () = print_int (adder 1 2 3 4 5 6 7 8 9 10) in
  >   0
  adder(a1_0 a2_1 a3_2 a4_3 a5_4 a6_5 a7_6 a8_7 a9_8 a10_9 ) {let t_0 = 
                                                              a1_0 + a2_1 in 
                                                               let t_1 = 
                                                               t_0 + a3_2 in 
                                                                let t_2 = 
                                                                t_1 + a4_3 in 
                                                                 let t_3 = 
                                                                 t_2 + a5_4 in 
                                                                  let t_4 = 
                                                                  t_3 + a6_5 in 
                                                                   let t_5 = 
                                                                   t_4 + a7_6 in 
                                                                    let t_6 = 
                                                                    t_5 + a8_7 in 
                                                                     let t_7 = 
                                                                     t_6 + a9_8 in 
                                                                      t_7 + a10_9        } 
  
  main() {let t_9 = { name: adder, arity: 10 env_size: 0, arrange [ ]}  ([ 
                    1 2 3 4 5 6 7 8 9 10  ]) in 
           { name: print_int, arity: 1 env_size: 0, arrange [ ]}  ([ t_9  ]); 
            0  } 
  

  $ dune exec closure_conv <<- EOF
  > let rec map f list =
  > match list with
  > | [] -> []
  > | x::xs -> (f x)::(map f xs)
  > let rec iter action list =
  >  match list with
  >  | [] -> ()
  >  | x::xs -> let () = action x in let () =  iter action xs in ()
  > let main =
  >  let square x = x * x in
  >   let list = [1; 2; 3; 4; 5] in
  >   let squared = map square list in
  >   let () = iter print_int squared in
  >   0
  map(f_0 list_1 ) {let t_0 = list_1 = [] in 
                     if t_0  then []  else 
                      let t_1 = list_1 <> [] in 
                       if t_1 
                        then let x_2 = getfield 0 list_1 in 
                              let xs_3 = getfield 1 list_1 in 
                               let t_2 = f_0 ([ x_2  ]) in 
                                let t_3 = { name: map, arity: 2 env_size: 0, arrange [ ]}  ([ 
                                          f_0 xs_3  ]) in 
                                 t_2 :: t_3     
                        else 
                        { name: partial_match, arity: 1 env_size: 0, arrange [ ]}  ([ 
                        list_1  ])    } 
  
  iter(action_4 list_5 ) {let t_8 = list_5 = [] in 
                           if t_8  then ()  else 
                            let t_9 = list_5 <> [] in 
                             if t_9 
                              then let x_6 = getfield 0 list_5 in 
                                    let xs_7 = getfield 1 list_5 in 
                                     action_4 ([ x_6  ]); 
                                      { name: iter, arity: 2 env_size: 0, arrange [ ]}  ([ 
                                      action_4 xs_7  ]);  ()     
                              else 
                              { name: partial_match, arity: 1 env_size: 0, arrange [ ]}  ([ 
                              list_5  ])    } 
  
  square_11(x_10 ) {x_10 * x_10} 
  
  main() {let t_17 = 5 :: [] in 
           let t_18 = 4 :: t_17 in 
            let t_19 = 3 :: t_18 in 
             let t_20 = 2 :: t_19 in 
              let list_12 = 1 :: t_20 in 
               let squared_13 = { name: map, arity: 2 env_size: 0, arrange [ ]}  ([ 
                                { name: square_11, arity: 1 env_size: 0, arrange [ ]}  
                                list_12  ]) in 
                { name: iter, arity: 2 env_size: 0, arrange [ ]}  ([ { name: print_int, arity: 1 env_size: 0, arrange [ ]}  
                squared_13  ]);  0       } 
  
  $ dune exec closure_conv < manytests/typed/007order.ml
  _start(unused_0 unused_1 a_2 unused_3 b_4 _c_5 unused_6 d_7 ___8 ) {let t_4 = 
                                                                      a_2 + b_4 in 
                                                                       
                                                                      { name: print_int, arity: 1 env_size: 0, arrange [ ]}  ([ 
                                                                      t_4  ]); 
                                                                       
                                                                      { name: print_int, arity: 1 env_size: 0, arrange [ ]}  ([ 
                                                                      ___8  ]); 
                                                                       
                                                                      let t_0 = 
                                                                      a_2 * b_4 in 
                                                                       
                                                                      let t_1 = 
                                                                      t_0 / _c_5 in 
                                                                       
                                                                      t_1 + d_7     } 
  
  main() {let t_6 = { name: print_int, arity: 1 env_size: 0, arrange [ ]}  ([ 
                    1  ]) in 
           let t_7 = { name: print_int, arity: 1 env_size: 0, arrange [ ]}  ([ 
                     2  ]) in 
            let t_8 = { name: print_int, arity: 1 env_size: 0, arrange [ ]}  ([ 
                      4  ]) in 
             let t_9 = { name: print_int, arity: 1 env_size: 0, arrange [ ]}  ([ 
                       -1  ]) in 
              let t_10 = { name: _start, arity: 9 env_size: 0, arrange [ ]}  ([ 
                         t_6 t_7 3 t_8 100 1000 t_9 10000 -555555  ]) in 
               { name: print_int, arity: 1 env_size: 0, arrange [ ]}  ([ 
               t_10  ])     } 
  
  $ dune exec closure_conv <<- EOF
  > let f a =
  >   let x = 5 in
  >   fun y -> a + x + y
  fresh_fun_0(y_2 a_0 x_1 ) {let t_1 = a_0 + x_1 in  t_1 + y_2 } 
  
  f(a_0 ) {let x_1 = 5 in 
            { name: fresh_fun_0, arity: 1 env_size: 2, arrange [1: a_0; 
            2: x_1;  ]}  } 
  
  $ dune exec closure_conv < manytests/typed/004manyargs.ml
  wrap(f_0 ) {let t_0 = 1 = 1 in  if t_0  then f_0  else  f_0  } 
  
  test3(a_1 b_2 c_3 ) {let a_4 = { name: print_int, arity: 1 env_size: 0, arrange [ ]}  ([ 
                                 a_1  ]) in 
                        let b_5 = { name: print_int, arity: 1 env_size: 0, arrange [ ]}  ([ 
                                  b_2  ]) in 
                         let c_6 = { name: print_int, arity: 1 env_size: 0, arrange [ ]}  ([ 
                                   c_3  ]) in 
                          0   } 
  
  test10(a_7 b_8 c_9 d_10 e_11 f_12 g_13 h_14 i_15 j_16 ) {let t_4 = a_7 + b_8 in 
                                                            let t_5 = t_4 + c_9 in 
                                                             let t_6 = 
                                                             t_5 + d_10 in 
                                                              let t_7 = 
                                                              t_6 + e_11 in 
                                                               let t_8 = 
                                                               t_7 + f_12 in 
                                                                let t_9 = 
                                                                t_8 + g_13 in 
                                                                 let t_10 = 
                                                                 t_9 + h_14 in 
                                                                  let t_11 = 
                                                                  t_10 + i_15 in 
                                                                   t_11 + j_16        } 
  
  main() {let rez_17 = { name: wrap, arity: 1 env_size: 0, arrange [ ]}  ([ 
                       { name: test10, arity: 10 env_size: 0, arrange [ ]}  
                       1 10 100 1000 10000 100000 1000000 10000000 100000000 
                       1000000000  ]) in 
           { name: print_int, arity: 1 env_size: 0, arrange [ ]}  ([ rez_17  ]); 
            let temp2_19 = { name: wrap, arity: 1 env_size: 0, arrange [ ]}  ([ 
                           { name: test3, arity: 3 env_size: 0, arrange [ ]}  
                           1 10 100  ]) in 
             0   } 
  

  $ dune exec closure_conv <<- EOF
  > let main =
  >   let rec fib n k = if n<2 then 1 else fib (n-1) (fun a -> fib (n-2) (fun b -> k (a+b))) in
  >   let x = fib 5 (fun x -> x) in
  >   let () = print_int x in
  >   0
  fresh_fun_2(b_4 a_3 k_2 ) {let t_10 = a_3 + b_4 in  k_2 ([ t_10  ]) } 
  
  fresh_fun_1(a_3 fib_0 k_2 n_1 ) {let t_7 = n_1 - 2 in 
                                    let t_8 = { name: fresh_fun_2, arity: 1 env_size: 2, arrange [
                                              1: a_3; 2: k_2;  ]}  in 
                                     { name: fib_0, arity: 2 env_size: 0, arrange [ ]}  ([ 
                                     t_7 t_8  ])  } 
  
  fib_0(n_1 k_2 ) {let t_3 = n_1 < 2 in 
                    if t_3  then 1  else 
                     let t_4 = n_1 - 1 in 
                      let t_5 = { name: fresh_fun_1, arity: 1 env_size: 3, arrange [
                                1: fib_0; 2: k_2; 3: n_1;  ]}  in 
                       { name: fib_0, arity: 2 env_size: 0, arrange [ ]}  ([ 
                       t_4 t_5  ])    } 
  
  fresh_fun_0(x_5 ) {x_5} 
  
  main() {let t_1 = { name: fresh_fun_0, arity: 1 env_size: 0, arrange [ ]}  in 
           let x_6 = { name: fib_0, arity: 2 env_size: 0, arrange [ ]}  ([ 
                     5 t_1  ]) in 
            { name: print_int, arity: 1 env_size: 0, arrange [ ]}  ([ x_6  ]); 
             0   } 
  
