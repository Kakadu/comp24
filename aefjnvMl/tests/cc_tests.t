  $ dune exec ./cc_runner.exe << EOF
  > let prog = 5 + 5
  > EOF
  let prog = (( + ) 5) 5
  ;;
  

  $ dune exec ./cc_runner.exe << EOF
  > let test1 x = let test2 y = x + y in test2
  > EOF
  let test1 x =
    let test2 cc0_x y =
      (( + ) cc0_x) y in
      test2 x
  ;;
  

  $ dune exec ./cc_runner.exe << EOF
  > let test1 (x, y) = let test2 i = (x, y, i) in test2
  > EOF
  let test1 (x, y) =
    let test2 cc0_y cc1_x i =
      (cc1_x, cc0_y, i) in
      (test2 y) x
  ;;
  

  $ dune exec ./cc_runner.exe << EOF
  > let test1 (x, y) = let test2 i = (x, y, i) in test2
  > EOF
  let test1 (x, y) =
    let test2 cc0_y cc1_x i =
      (cc1_x, cc0_y, i) in
      (test2 y) x
  ;;
  

  $ dune exec ./cc_runner.exe << EOF
  > let rec facCPS n k = match n with
  >   | 0 -> k 1
  >   | n -> facCPS (n - 1) (fun t -> k (n * t))
  > EOF
  let rec facCPS n k =
    match n with
      | 0 -> 
        k 1
      | cc_ac0_n -> 
        (facCPS ((( - ) cc_ac0_n) 1)) (((fun cc0_k cc1_cc_ac0_n t -> cc0_k ((( * ) cc1_cc_ac0_n) t)) k) cc_ac0_n)
  ;;
  

  $ dune exec ./cc_runner.exe << EOF
  > let nested1 = let nested2 = 5 in 
  > let nested3 = 6 in
  > let nested4 x = x + (fun i -> nested2 + nested3) 8 in nested4 55
  > EOF
  let nested1 = let nested2 = 5 in
      let nested3 = 6 in
        let nested4 cc0_nested3 cc1_nested2 x =
          (( + ) x) ((((fun cc2_nested3 cc3_nested2 i -> (( + ) cc3_nested2) cc2_nested3) cc0_nested3) cc1_nested2) 8) in
          ((nested4 nested3) nested2) 55
  ;;
  

  $ dune exec ./cc_runner.exe << EOF
  > let rec facCPS n k = match n with
  >   | 0 -> k 1
  >   | n -> facCPS (n - 1) (fun t -> k (n * t))
  > EOF
  let rec facCPS n k =
    match n with
      | 0 -> 
        k 1
      | cc_ac0_n -> 
        (facCPS ((( - ) cc_ac0_n) 1)) (((fun cc0_k cc1_cc_ac0_n t -> cc0_k ((( * ) cc1_cc_ac0_n) t)) k) cc_ac0_n)
  ;;
  

  $ dune exec ./cc_runner.exe << EOF
  > let _start () () ()  () =
  > let () = print_int 5 in 5
  > let main = print_int (_start (print_int 1) (print_int 2)  (print_int 3)   (print_int 4))
  > EOF
  let cc_ac0__start () () () () =
    let () = print_int 5 in
      5
  ;;
  
  let cc_ac1_main = print_int ((((cc_ac0__start (print_int 1)) (print_int 2)) (print_int 3)) (print_int 4))
  ;;
  

  $ dune exec ./cc_runner.exe << EOF
  > let rec even n =
  > match n with
  >  | 0 -> true
  >  | x -> odd (x-1)
  > and odd n =
  > match n with
  >  | 0 -> false
  >  | x -> even (x-1);;
  let rec even n =
    match n with
      | 0 -> 
        true
      | x -> 
        odd ((( - ) x) 1)
  and odd cc_ac0_n =
    match cc_ac0_n with
      | 0 -> 
        false
      | cc_ac1_x -> 
        even ((( - ) cc_ac1_x) 1)
  ;;
  

  $ ./cc_runner.exe < manytests/typed/005fix.ml
  let rec fix f x =
    (f (fix f)) x
  ;;
  
  let fac self n =
    (if (( <= ) n) 1
    then
      1
    else
      (( * ) n) (self ((( - ) n) 1)))
  ;;
  
  let cc_ac0_main = let () = print_int ((fix fac) 6) in
      0
  ;;
  

  $ ./cc_runner.exe < manytests/typed/006partial2.ml
  let foo a b c =
    let () = print_int a in
      let () = print_int b in
        let () = print_int c in
          (( + ) a) ((( * ) b) c)
  ;;
  
  let cc_ac3_main = let cc_ac0_foo = foo 1 in
      let cc_ac1_foo cc0_cc_ac0_foo =
        cc0_cc_ac0_foo 2 in
        let cc_ac2_foo cc1_cc_ac1_foo =
          (cc1_cc_ac1_foo cc_ac0_foo) 3 in
          let () = print_int cc_ac2_foo in
            0
  ;;
  

  $ ./cc_runner.exe < manytests/typed/007order.ml
  let cc_ac0__start () () a () b _c () d __ =
    let () = print_int ((( + ) a) b) in
      let () = print_int __ in
        (( + ) (( / ) (( * ) a) b) _c) d
  ;;
  
  let cc_ac1_main = print_int (((((((((cc_ac0__start (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int (~- 1))) 10000) (~- 555555))
  ;;
  

  $ ./cc_runner.exe < manytests/typed/008ascription.ml
  let addi f g x =
    (f x) (g x)
  ;;
  
  let cc_ac2_main = let () = print_int (((addi (fun cc_ac0_x b -> (if b
      then
        (( + ) cc_ac0_x) 1
      else
        (( * ) cc_ac0_x) 2))) (fun cc_ac1__start -> (( = ) (( / ) cc_ac1__start) 2) 0)) 4) in
      0
  ;;
  

  $ ./cc_runner.exe < manytests/typed/015tuples.ml
  let rec fix f x =
    (f (fix f)) x
  ;;
  
  let map cc_ac0_f p =
    let (a, b) = p in
      (cc_ac0_f a, cc_ac0_f b)
  ;;
  
  let fixpoly l =
    (fix (fun self cc_ac1_l -> (map (((fun cc0_self cc1_cc_ac1_l li cc_ac2_x -> (li (cc0_self cc1_cc_ac1_l)) cc_ac2_x) self) cc_ac1_l)) cc_ac1_l)) l
  ;;
  
  let feven cc_ac3_p n =
    let (e, o) = cc_ac3_p in
      (if (( == ) n) 0
      then
        1
      else
        o ((( - ) n) 1))
  ;;
  
  let fodd cc_ac4_p cc_ac5_n =
    let (cc_ac6_e, cc_ac7_o) = cc_ac4_p in
      (if (( == ) cc_ac5_n) 0
      then
        0
      else
        cc_ac6_e ((( - ) cc_ac5_n) 1))
  ;;
  
  let tie = fixpoly (feven, fodd)
  ;;
  
  let rec cc_ac_meven cc_ac8_n =
    (if (( = ) cc_ac8_n) 0
    then
      1
    else
      modd ((( - ) cc_ac8_n) 1))
  and modd cc_ac9_n =
    (if (( = ) cc_ac9_n) 0
    then
      1
    else
      cc_ac_meven ((( - ) cc_ac9_n) 1))
  ;;
  
  let cc_ac10_main = let () = print_int (modd 1) in
      let () = print_int (cc_ac_meven 2) in
        let (even, odd) = tie in
          let () = print_int (odd 3) in
            let () = print_int (even 4) in
              0
  ;;
  
