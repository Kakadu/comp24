  $ dune exec liveness_analysis <<- EOF
  > let f a = a
  f:   

  $ dune exec liveness_analysis <<- EOF
  > let f a =
  >  let x = a + 1 in
  >  let x = x + a in
  >  let x = a + 5 in
  >  0
  f: x_1: [1, 2]
  x_2: [2, 2]
  x_3: [3, 3]
    

  $ dune exec liveness_analysis <<- EOF
  > let f a =
  >  let x = a + 1 in
  >  let helper y = y + x + a in
  >  helper 0
  helper_3: t_1: [1, 2]
    
  f: x_1: [1, 2]
    

  $ dune exec liveness_analysis <<- EOF
  > let f x =
  >  let a = x + 1 in
  >  let b = a + 1 in
  >  let c = b + 1 in
  >  let d = x + 1 in
  >  d
  f: a_1: [1, 2]
  b_2: [2, 3]
  c_3: [3, 3]
  d_4: [4, 5]
    

  $ dune exec liveness_analysis <<- EOF
  > let f x =
  >   let a = 0 in
  >   fun x y ->
  >     let temp = x + y in
  >     fun z -> z + temp + x
  fresh_fun_1: t_2: [1, 2]
    
  fresh_fun_0: temp_4: [1, 2]
    
  f: a_1: [1, 1]
    

  $ dune exec liveness_analysis <<- EOF
  > let add a b c d e f g =
  >   let temp = a + b in
  >    a + b + c + d + e + f + g + temp
  add: temp_7: [1, 8]
  t_0: [2, 3]
  t_1: [3, 4]
  t_2: [4, 5]
  t_3: [5, 6]
  t_4: [6, 7]
  t_5: [7, 8]
    
