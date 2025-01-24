  $ dune exec closure_conv <<- EOF
  > let add x y = x + y
  > let main =
  >   let add5 = add 5 in
  >   let res = add5 10 in
  > 0
  add(x_0 y_1 ) {x_0 + y_1} 
  
  main() {let add5_2 = { name: add, arity: 2 env_size: 1, arrange [ ]}  ([ 
                       5  ]) in 
           let res_3 = add5_2 ([ 10  ]) in  0  } 
  

  $ dune exec closure_conv <<- EOF
  > let f x y z =
  > x + y + z
  > let main = f 3 5 6
  f(x_0 y_1 z_2 ) {let t_0 = x_0 + y_1 in  t_0 + z_2 } 
  
  main() {{ name: f, arity: 3 env_size: 2, arrange [ ]}  ([ 3 5 6  ])} 
  

  $ dune exec closure_conv <<- EOF
  > let f a =
  >  let b = a in
  >  let c  = a in
  >  fun q w e r -> q + e + b + a
  fresh_fun_0(q_3 w_4 e_5 a_0 b_1 r_6 ) {let t_0 = q_3 + e_5 in 
                                          let t_1 = t_0 + b_1 in  t_1 + a_0  } 
  
  f(a_0) {let b_1 = a_0 in 
           let c_2 = a_0 in 
            { name: fresh_fun_0, arity: 4 env_size: 5, arrange [3: a_0; 
            4: b_1;  ]}   } 
  

  $ dune exec closure_conv <<- EOF
  > let rec fac x = if x < 1 then 1 else x * fac (x - 1) 
  fac(x_0) {let t_0 = x_0 < 1 in 
             if t_0  then 1  else 
              let t_1 = x_0 - 1 in 
               let t_2 = { name: fac, arity: 1 env_size: 0, arrange [ ]}  ([ 
                         t_1  ]) in 
                x_0 * t_2    } 
  

  $ dune exec closure_conv <<-EOF
  > let f x =
  > let a = 5 in
  > fun y -> x + a + y
  fresh_fun_0(a_1 x_0 y_2 ) {let t_0 = x_0 + a_1 in  t_0 + y_2 } 
  
  f(x_0) {let a_1 = 5 in 
           { name: fresh_fun_0, arity: 1 env_size: 2, arrange [0: a_1; 
           1: x_0;  ]}  } 
  
  $ dune exec closure_conv <<- EOF
  > let f x y =
  > let a = 10 in
  > let b = a in
  > fun u v ->  u + v * a
  fresh_fun_0(u_4 a_2 v_5 ) {let t_0 = v_5 * a_2 in  u_4 + t_0 } 
  
  f(x_0 y_1 ) {let a_2 = 10 in 
                let b_3 = a_2 in 
                 { name: fresh_fun_0, arity: 2 env_size: 2, arrange [1: a_2;  ]}   } 
  
  $ dune exec closure_conv <<- EOF
  > let f x =
  >   let a = x in
  >   let b = x in
  >   fun q ->
  >     if q > 0 then a + q
  >     else q - b
  fresh_fun_0(a_1 b_2 q_3 ) {let t_0 = q_3 > 0 in 
                              if t_0  then a_1 + q_3  else  q_3 - b_2  } 
  
  f(x_0) {let a_1 = x_0 in 
           let b_2 = x_0 in 
            { name: fresh_fun_0, arity: 1 env_size: 2, arrange [0: a_1; 
            1: b_2;  ]}   } 
  

  $ dune exec closure_conv <<- EOF
  > let fac n =
  >  let rec helper acc n =
  >    if n < 1 then 1 else helper (acc * n) (n - 1) in
  >  helper 1 n
  helper_1(acc_2 n_3 ) {let t_1 = n_3 < 1 in 
                         if t_1  then 1  else 
                          let t_2 = acc_2 * n_3 in 
                           let t_3 = n_3 - 1 in 
                            { name: helper_1, arity: 2 env_size: 1, arrange [ ]}  ([ 
                            t_2 t_3  ])    } 
  
  fac(n_0) {{ name: helper_1, arity: 2 env_size: 1, arrange [ ]}  ([ 1 n_0  ])} 
  

  $ dune exec closure_conv <<- EOF
  > let rec is_even n =
  >   if n = 0 then true else is_odd (n - 1)
  > and is_odd n =
  >   if n = 1 then true else is_even (n - 1)
  is_even(n_1) {let t_0 = n_1 = 0 in 
                 if t_0  then true  else 
                  let t_1 = n_1 - 1 in 
                   { name: is_odd, arity: 1 env_size: 0, arrange [ ]}  ([ 
                   t_1  ])   } 
  
  is_odd(n_0) {let t_3 = n_0 = 1 in 
                if t_3  then true  else 
                 let t_4 = n_0 - 1 in 
                  { name: is_even, arity: 1 env_size: 0, arrange [ ]}  ([ 
                  t_4  ])   } 
  

  $ dune exec closure_conv <<- EOF
  > let f x =
  >   let a = 5 in
  >   let rec helper y =
  >     if y < 0 then 1 else helper (a + y) in
  >   helper 10 
  helper_2(a_1 y_3 ) {let t_1 = y_3 < 0 in 
                       if t_1  then 1  else 
                        let t_2 = a_1 + y_3 in 
                         { name: helper_2, arity: 1 env_size: 1, arrange [
                         0: a_1;  ]}  ([ t_2  ])   } 
  
  f(x_0) {let a_1 = 5 in 
           { name: helper_2, arity: 1 env_size: 1, arrange [0: a_1;  ]}  ([ 
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
  inner_3(a_1 x_0 y_2 ) {let t_0 = a_1 + x_0 in  t_0 + y_2 } 
  
  f(x_0) {let a_1 = x_0 + 5 in 
           { name: inner_3, arity: 1 env_size: 2, arrange [0: a_1; 1: x_0;  ]}  } 
  
  main() {let res_4 = { name: f, arity: 1 env_size: 0, arrange [ ]}  ([ 
                      10 11  ]) in 
           let unused_5 = print_int ([ res_4  ]) in  0  } 
  
