  $ dune exec transformers <<- EOF
  > let main = 1
  > EOF
  let main = 1 
  $ dune exec transformers <<- EOF
  > let a, b = 5, 6
  let b = 6 
  let a = 5 

  $ dune exec transformers <<- EOF
  > let map f p =
  >   let a, b = p in
  >   f a, f b
  let map = fun f_0 -> 
              fun p_1 -> 
                let t_4 =  access_tuple(p_1, 0) in 
                 let a_2 =  t_4 in 
                  let t_3 =  access_tuple(p_1, 1) in 
                   let b_3 =  t_3 in 
                    let t_0 =  f_0(a_2) in  let t_1 =  f_0(b_3) in  t_0, t_1
                 

  $ dune exec transformers <<- EOF
  > let main = let a = 5 in (a + 6) - (a / 8)
  > EOF
  let main = let a_0 =  5 in 
              let t_0 =  a_0 + 6  in  let t_1 =  a_0 / 8  in  t_0 - t_1  
  $ dune exec transformers <<- EOF
  > let rec fac x = if x < 1 then 1 else x * fac (x - 1)
  > EOF
  let rec fac = fun x_0 -> 
                  let t_0 =  x_0 < 1  in 
                   if t_0  then 1  else 
                      let t_1 =  x_0 - 1  in 
                       let t_2 =  fac(t_1) in  x_0 * t_2  
                      
                   
  $ dune exe transformers <<- EOF
  > let f x =
  >   if x < 0 then
  >     let a = 535 + x in
  >      let b = a * x in 
  >     a / b
  >    else
  >      let a = 20 in
  >       a + 5 * 30
  let f = fun x_0 -> 
            let t_0 =  x_0 < 0  in 
             if t_0 
                then let t_3 =  535 + x_0  in 
                      let a_1 =  t_3 in 
                       let t_2 =  a_1 * x_0  in  let b_2 =  t_2 in  a_1 / b_2  
                else  let a_3 =  20 in  let t_4 =  5 * 30  in  a_3 + t_4   
             
  $ dune exe transformers <<- EOF
  > let f (a, b) = a +1, b + 1
  let f = fun t_3 -> 
            let t_4 =  access_tuple(t_3, 0) in 
             let t_5 =  access_tuple(t_3, 1) in 
              let a_0 =  t_4 in 
               let b_1 =  t_5 in 
                let t_0 =  a_0 + 1  in  let t_1 =  b_1 + 1  in  t_0, t_1
             
  $ dune exe transformers <<- EOF    
  > let f (a, b) (x::y) ((z,w)::zws) =
  >  a + b + x + z + w       
  let f = fun t_13 -> 
            let t_14 =  access_tuple(t_13, 0) in 
             let t_15 =  access_tuple(t_13, 1) in 
              let a_0 =  t_14 in 
               let b_1 =  t_15 in 
                fun t_10 -> 
                  let t_11 =  list_head(t_10) in 
                   let t_12 =  list_tail(t_10) in 
                    let x_2 =  t_11 in 
                     let y_3 =  t_12 in 
                      fun t_4 -> 
                        let t_5 =  list_head(t_4) in 
                         let t_6 =  access_tuple(t_5, 0) in 
                          let t_7 =  list_head(t_4) in 
                           let t_8 =  access_tuple(t_7, 1) in 
                            let t_9 =  list_tail(t_4) in 
                             let z_4 =  t_6 in 
                              let w_5 =  t_8 in 
                               let zws_6 =  t_9 in 
                                let t_0 =  a_0 + b_1  in 
                                 let t_1 =  t_0 + x_2  in 
                                  let t_2 =  t_1 + z_4  in  t_2 + w_5 
                         



$ dune exec transformers < manytests/typed/001fac.ml
$ dune exec transformers < manytests/typed/002fac.ml
$ dune exec transformers < manytests/typed/003fib.ml
$ dune exec transformers < manytests/typed/004manyargs.ml
$ dune exec transformers < manytests/typed/005fix.ml
$ dune exec transformers < manytests/typed/006partial.ml
$ dune exec transformers < manytests/typed/006partial2.ml
$ dune exec transformers < manytests/typed/006partial3.ml
$ dune exec transformers < manytests/typed/008ascription.ml
$ dune exec transformers < manytests/typed/009let_poly.ml
$ dune exec transformers < manytests/typed/015tuples.ml
$ dune exec transformers < manytests/typed/016lists.ml
