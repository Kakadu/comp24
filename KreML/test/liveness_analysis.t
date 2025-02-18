  $ dune exec liveness_analysis <<- EOF
  > let f a = a
  f: a_0: [0, 1]
    

  $ dune exec liveness_analysis <<- EOF
  > let f a =
  >  let x = a + 1 in
  >  let x = x + a in
  >  let x = a + 5 in
  >  0
  f: x_2: [2, 2]
  x_1: [1, 2]
  x_3: [3, 3]
  a_0: [0, 3]
    

  $ dune exec liveness_analysis <<- EOF
  > let f a =
  >  let x = a + 1 in
  >  let helper y = y + x + a in
  >  helper 0
  helper_3: y_2: [0, 1]
  x_1: [0, 1]
  t_1: [1, 2]
  a_0: [0, 2]
    
  f: x_1: [1, 2]
  a_0: [0, 2]
    

  $ dune exec liveness_analysis <<- EOF
  > let f x =
  >  let a = x + 1 in
  >  let b = a + 1 in
  >  let c = b + 1 in
  >  let d = x + 1 in
  >  d
  f: a_1: [1, 2]
  c_3: [3, 3]
  b_2: [2, 3]
  x_0: [0, 4]
  d_4: [4, 5]
    

  $ dune exec liveness_analysis <<- EOF
  > let f x =
  >   let a = 0 in
  >   fun x y ->
  >     let temp = x + y in
  >     fun z -> z + temp + x
  fresh_fun_1: z_5: [0, 1]
  temp_4: [0, 1]
  x_2: [0, 2]
  t_0: [1, 2]
    
  fresh_fun_0: y_3: [0, 1]
  x_2: [0, 2]
  temp_4: [1, 2]
    
  f: x_0: [0, 0]
  a_1: [1, 1]
  
