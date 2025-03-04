  $ dune exec rv_regalloc <<- EOF
  > let add x y z =
  >   x + y + z
  add(x_0 y_1 z_2 ) {let t_0 = x_0 + y_1 in  t_0 + z_2 } 
  
  add: 
   t_0: t0
  

  $ dune exec rv_regalloc <<- EOF
  > let add a b c d e f g =
  >   let temp = a + b in
  >    a + b + c + d + e + f + g + temp
  add(a_0 b_1 c_2 d_3 e_4 f_5 g_6 ) {let temp_7 = a_0 + b_1 in 
                                      let t_0 = a_0 + b_1 in 
                                       let t_1 = t_0 + c_2 in 
                                        let t_2 = t_1 + d_3 in 
                                         let t_3 = t_2 + e_4 in 
                                          let t_4 = t_3 + f_5 in 
                                           let t_5 = t_4 + g_6 in 
                                            t_5 + temp_7       } 
  
  add: 
   t_0: t1
  t_1: t1
  t_2: t1
  t_3: t1
  t_4: t1
  t_5: t1
  temp_7: t0
  
  $ dune exec rv_regalloc <<- EOF
  > let rec fac x = if x < 1 then 1 else x * fac (x-1)
  > let main =
  >   let () = print_int (fac 10) in
  >   0
  fac(x_0 ) {let t_0 = x_0 < 1 in 
              if t_0  then 1  else 
               let t_1 = x_0 - 1 in 
                let t_2 = { name: fac, arity: 1 env_size: 0, arrange [ ]}  ([ 
                          t_1  ]) in 
                 x_0 * t_2    } 
  
  main() {let t_4 = { name: fac, arity: 1 env_size: 0, arrange [ ]}  ([ 10  ]) in 
           { name: print_int, arity: 1 env_size: 0, arrange [ ]}  ([ t_4  ]); 
            0  } 
  
  fac: 
   t_0: t0
  t_1: t0
  t_2: t0
  
  main: 
   t_4: t0
  

  $ dune exec rv_regalloc <<- EOF
  > let f x =
  >   let a = 1 in
  >   let b = 2 in
  >   let c = 3 in
  >   let d = 4 in
  >   let e = 5 in
  >   let f = 6 in
  >   let g = 7 in
  >   let ab = a + b in
  >   let cd = c + d in
  >   let ef = e + f in
  >   ab + cd + ef + g
  f(x_0 ) {let a_1 = 1 in 
            let b_2 = 2 in 
             let c_3 = 3 in 
              let d_4 = 4 in 
               let e_5 = 5 in 
                let f_6 = 6 in 
                 let g_7 = 7 in 
                  let ab_8 = a_1 + b_2 in 
                   let cd_9 = c_3 + d_4 in 
                    let ef_10 = e_5 + f_6 in 
                     let t_0 = ab_8 + cd_9 in 
                      let t_1 = t_0 + ef_10 in  t_1 + g_7            } 
  
  f: 
   a_1: t0
  ab_8: t1
  b_2: t1
  c_3: t2
  cd_9: t2
  d_4: t3
  e_5: t4
  ef_10: t5
  f_6: t5
  g_7: t6
  t_0: t2
  t_1: t5
  

  $ dune exec rv_regalloc <<- EOF
  > let f x =
  >   let a = 1 in
  >   let b = 2 in
  >   let c = 3 in
  >   let d = 4 in
  >   let e = 5 in
  >   let f = 6 in
  >   let g = 7 in
  >   let h = 8 in
  >   let ab = a + b in
  >   let cd = c + d in
  >   let ef = e + f in
  >   let gh = g + h in
  >   ab + cd + ef + gh
  f(x_0 ) {let a_1 = 1 in 
            let b_2 = 2 in 
             let c_3 = 3 in 
              let d_4 = 4 in 
               let e_5 = 5 in 
                let f_6 = 6 in 
                 let g_7 = 7 in 
                  let h_8 = 8 in 
                   let ab_9 = a_1 + b_2 in 
                    let cd_10 = c_3 + d_4 in 
                     let ef_11 = e_5 + f_6 in 
                      let gh_12 = g_7 + h_8 in 
                       let t_0 = ab_9 + cd_10 in 
                        let t_1 = t_0 + ef_11 in  t_1 + gh_12              } 
  
  f: 
   a_1: t0
  ab_9: t0
  b_2: t1
  c_3: t2
  cd_10: t3
  d_4: t3
  e_5: t4
  ef_11: t4
  f_6: t5
  g_7: t6
  gh_12: s1
  h_8: s1
  t_0: t0
  t_1: t4
  

  $ dune exec rv_regalloc <<- EOF
  > let f x =
  >   let a = 1 in
  >   let b = 2 in
  >   let c = 3 in
  >   let d = 4 in
  >   let e = 5 in
  >   let f = 6 in
  >   let g = 7 in
  >   let h = 8 in
  >   let gh = g + h in
  >   let cd = c + d in
  >   let ef = e + f in
  >   let ab = a + b in
  >   ab + cd + ef + gh
  f(x_0 ) {let a_1 = 1 in 
            let b_2 = 2 in 
             let c_3 = 3 in 
              let d_4 = 4 in 
               let e_5 = 5 in 
                let f_6 = 6 in 
                 let g_7 = 7 in 
                  let h_8 = 8 in 
                   let gh_9 = g_7 + h_8 in 
                    let cd_10 = c_3 + d_4 in 
                     let ef_11 = e_5 + f_6 in 
                      let ab_12 = a_1 + b_2 in 
                       let t_0 = ab_12 + cd_10 in 
                        let t_1 = t_0 + ef_11 in  t_1 + gh_9              } 
  
  f: 
   a_1: t0
  ab_12: t1
  b_2: t1
  c_3: t2
  cd_10: t3
  d_4: t3
  e_5: t4
  ef_11: t4
  f_6: t5
  g_7: t6
  gh_9: t6
  h_8: s1
  t_0: t3
  t_1: t4
  

  $ dune exec rv_regalloc <<- EOF
  > let adder a b c d e f g h j k = a + b + c + d + e + f + g+ h + j + k
  > let main =
  >  let a = 1 in
  >  let b = 2 in
  >  let c = 3 in
  >  let d = 4 in
  >  let e = 5 in
  >  let f = 6 in
  >  let g = 7 in
  >  let h = 8 in
  >  let j = 9 in
  >  let k = 10 in
  >  adder a b c d e f g h j k  
  adder(a_0 b_1 c_2 d_3 e_4 f_5 g_6 h_7 j_8 k_9 ) {let t_0 = a_0 + b_1 in 
                                                    let t_1 = t_0 + c_2 in 
                                                     let t_2 = t_1 + d_3 in 
                                                      let t_3 = t_2 + e_4 in 
                                                       let t_4 = t_3 + f_5 in 
                                                        let t_5 = t_4 + g_6 in 
                                                         let t_6 = t_5 + h_7 in 
                                                          let t_7 = t_6 + j_8 in 
                                                           t_7 + k_9        } 
  
  main() {let a_10 = 1 in 
           let b_11 = 2 in 
            let c_12 = 3 in 
             let d_13 = 4 in 
              let e_14 = 5 in 
               let f_15 = 6 in 
                let g_16 = 7 in 
                 let h_17 = 8 in 
                  let j_18 = 9 in 
                   let k_19 = 10 in 
                    { name: adder, arity: 10 env_size: 0, arrange [ ]}  ([ 
                    a_10 b_11 c_12 d_13 e_14 f_15 g_16 h_17 j_18 k_19  ])          } 
  
  adder: 
   t_0: t0
  t_1: t0
  t_2: t0
  t_3: t0
  t_4: t0
  t_5: t0
  t_6: t0
  t_7: t0
  
  main: 
   a_10: t0
  b_11: t1
  c_12: t2
  d_13: t3
  e_14: t4
  f_15: t5
  g_16: t6
  h_17: s1
  j_18: s2
  k_19: s3
  
