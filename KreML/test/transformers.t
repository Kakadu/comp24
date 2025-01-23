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
                let t_4 =   getfield 0 p_1  in 
                 let a_2 =  t_4 in 
                  let t_3 =   getfield 1 p_1  in 
                   let b_3 =  t_3 in 
                    let t_0 =  f_0 a_2 in  let t_1 =  f_0 b_3 in  t_0, t_1
                 

  $ dune exec transformers <<- EOF
  > let main = let a = 5 in (a + 6) - (a / 8)
  > EOF
  let main = let a_0 =  5 in 
              let t_0 =  a_0 + 6  in 
               let t_1 =  a_0 / 8  in  let t_2 =  t_0 - t_1  in  t_2 
  $ dune exec transformers <<- EOF
  > let rec fac x = if x < 1 then 1 else x * fac (x - 1)
  > EOF
  let rec fac = fun x_0 -> 
                  let t_0 =  x_0 < 1  in 
                   if t_0  then 1  else 
                      let t_1 =  x_0 - 1  in 
                       let t_2 =  fac t_1 in  let t_3 =  x_0 * t_2  in  t_3 
                      
                   
  $ dune exec transformers <<- EOF
  > let rec fac x =
  >  let rec helper acc x =
  >    if x < 1 then acc
  >    else helper (x * acc) (x - 1)
  >  in helper 1 x
  let rec fac = fun x_0 -> 
                  let rec helper_1 = 
                   fun acc_2 -> 
                     fun x_3 -> 
                       let t_1 =  x_3 < 1  in 
                        if t_1  then acc_2  else 
                           let t_2 =  x_3 * acc_2  in 
                            let t_3 =  x_3 - 1  in  helper_1 t_2 t_3 
                           
                        in 
                   helper_1 1 x_0 

  $ dune exec transformers <<- EOF
  > let rec is_even n =
  >    if n = 0 then true
  >    else is_odd (n - 1)
  > and is_odd n =
  > if n = 1 then true
  > else is_even (n - 1)
  > let main =
  >  let res = is_even 600 in
  > if res then
  >   print_int 1
  > else print_int 0
  let rec is_even = fun n_1 -> 
                      let t_0 =  n_1 = 0  in 
                       if t_0  then true  else 
                          let t_1 =  n_1 - 1  in  is_odd t_1  
                       
  and is_odd = fun n_0 -> 
                 let t_3 =  n_0 = 1  in 
                  if t_3  then true  else  let t_4 =  n_0 - 1  in  is_even t_4 
                     
                  
  let main = let t_8 =  is_even 600 in 
              let res_2 =  t_8 in 
               if res_2  then print_int 1  else  print_int 0   

  $ dune exec transformers <<- EOF
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
                       let t_2 =  a_1 * x_0  in 
                        let b_2 =  t_2 in  let t_1 =  a_1 / b_2  in  t_1 
                else 
                let a_3 =  20 in 
                 let t_4 =  5 * 30  in  let t_5 =  a_3 + t_4  in  t_5 
                
             
  $ dune exec transformers <<- EOF
  > let f x =
  >  (fun x -> x + 1) x, (fun x -> x + 2) x
  let f = fun x_0 -> 
            let fresh_fun_0 =  fun x_1 -> let t_4 =  x_1 + 1  in  t_4 in 
             let t_0 =  fresh_fun_0 x_0 in 
              let fresh_fun_1 =  fun x_2 -> let t_3 =  x_2 + 2  in  t_3 in 
               let t_1 =  fresh_fun_1 x_0 in  t_0, t_1
             
  $ dune exec transformers <<- EOF
  > let apply x f = f x
  > let a =
  >   let b = 5 in
  >   apply b (fun x -> x / 2)
  let apply = fun x_0 -> fun f_1 -> f_1 x_0 
  let a = let b_2 =  5 in 
           let fresh_fun_0 =  fun x_3 -> let t_2 =  x_3 / 2  in  t_2 in 
            apply b_2 fresh_fun_0 

  $ dune exec transformers <<- EOF
  > let f (a, b) = a +1, b + 1
  let f = fun fresh_param_0 -> 
            let t_3 =   getfield 0 fresh_param_0  in 
             let t_4 =   getfield 1 fresh_param_0  in 
              let a_0 =  t_3 in 
               let b_1 =  t_4 in 
                let t_0 =  a_0 + 1  in  let t_1 =  b_1 + 1  in  t_0, t_1
             
  $ dune exec transformers <<- EOF    
  > let f (a, b) (x::y) ((z,w)::zws) =
  >  a + b + x + z + w       
  let f = fun fresh_param_2 -> 
            fun fresh_param_1 -> 
              fun fresh_param_0 -> 
                let t_11 =   getfield 0 fresh_param_2  in 
                 let t_12 =   getfield 1 fresh_param_2  in 
                  let a_0 =  t_11 in 
                   let b_1 =  t_12 in 
                    let t_9 =   getfield 0 fresh_param_1  in 
                     let t_10 =   getfield 1 fresh_param_1  in 
                      let x_2 =  t_9 in 
                       let y_3 =  t_10 in 
                        let t_4 =   getfield 0 fresh_param_0  in 
                         let t_5 =   getfield 0 t_4  in 
                          let t_6 =   getfield 0 fresh_param_0  in 
                           let t_7 =   getfield 1 t_6  in 
                            let t_8 =   getfield 1 fresh_param_0  in 
                             let z_4 =  t_5 in 
                              let w_5 =  t_7 in 
                               let zws_6 =  t_8 in 
                                let t_0 =  a_0 + b_1  in 
                                 let t_1 =  t_0 + x_2  in 
                                  let t_2 =  t_1 + z_4  in 
                                   let t_3 =  t_2 + w_5  in  t_3
                 
  $ dune exec transformers  <<- EOF
  > let (x::xs) = [1;2;3;4]
  let xs = let t_0 =   4 :: []  in 
            let t_1 =   3 :: t_0  in  let t_2 =   2 :: t_1  in  t_2 
  let x = 1 

  $ dune exec transformers <<- EOF
  > let f x =
  >   match x with | 0 -> 0 | 1 -> -1 | n -> n
  let f = fun x_0 -> 
            let t_0 =  x_0 = 0  in 
             if t_0  then 0  else 
                let t_1 =  x_0 = 1  in  if t_1  then -1  else  true    
             

  $ dune exec transformers <<- EOF
  > let rec map f list =
  >   match list with
  >   | [] -> []
  >   | x::xs -> x::(map f xs)
  let rec map = fun f_0 -> 
                  fun list_1 -> 
                    let t_0 =  list_1 = []  in 
                     if t_0  then []  else 
                        let t_1 =  list_1 <> []  in 
                         if t_1 
                            then let t_2 =  map f_0 xs_3 in 
                                  let t_3 =   x_2 :: t_2  in  t_3 
                            else 
                            Runtime error: expression list_1  does not match to any of provided patterns. 
                            () 
                             
                        
                     

  $ dune exec transformers <<- EOF
  > let disj x y =
  >   match x, y with
  >   | true, _ -> true
  >   | _, true -> true
  >   | false, false -> false   
  let disj = fun x_0 -> 
               fun y_1 -> 
                 let t_0 =  x_0 = true  in 
                  if t_0  then true  else 
                     let t_1 =  y_1 = true  in 
                      if t_1  then true  else 
                         let t_2 =  x_0 = false  in 
                          let t_3 =  y_1 = false  in 
                           let t_4 =  t_2 && t_3  in 
                            if t_4  then false  else 
                               Runtime error: expression (x_0, y_1)  does not match to any of provided patterns. 
                               () 
                                
                          
                     
                  

  $ dune exec transformers <<- EOF
  > let rec sumps list =
  >   match list with
  >   | [] ->  []
  >   | (a, b)::xs -> (a + b)::(sumps xs)
  let rec sumps = fun list_0 -> 
                    let t_0 =  list_0 = []  in 
                     if t_0  then []  else 
                        let t_1 =  list_0 <> []  in 
                         if t_1 
                            then let t_2 =  a_1 + b_2  in 
                                  let t_3 =  sumps xs_3 in 
                                   let t_4 =   t_2 :: t_3  in  t_4 
                            else 
                            Runtime error: expression list_0  does not match to any of provided patterns. 
                            () 
                             
                        
                     


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
