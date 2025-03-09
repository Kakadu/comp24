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
  
