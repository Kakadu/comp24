  $ dune exec ./cc_runner.exe << EOF
  > let prog = 5 + 5
  > EOF
  let prog = (( + ) 5) 5;;

  $ dune exec ./cc_runner.exe << EOF
  > let test1 x = let test2 y = x + y in test2
  > EOF
  let test1 x =
   let test2 x y =
   (( + ) x) y in
  test2 x;;

  $ dune exec ./cc_runner.exe << EOF
  > let test1 (x, y) = let test2 i = (x, y, i) in test2
  > EOF
  let test1 (x, y) =
   let test2 y x i =
   (x, y, i) in
  (test2 y) x;;

  $ dune exec ./cc_runner.exe << EOF
  > let test1 (x, y) = let test2 i = (x, y, i) in test2
  > EOF
  let test1 (x, y) =
   let test2 y x i =
   (x, y, i) in
  (test2 y) x;;

  $ dune exec ./cc_runner.exe << EOF
  > let rec facCPS n k = match n with
  >   | 0 -> k 1
  >   | n -> facCPS (n - 1) (fun t -> k (n * t))
  > EOF
  let rec facCPS n k =
   match n with
    | 0 -> k 1
    | ac0_n -> (facCPS ((( - ) ac0_n) 1)) (((fun k t ac0_n -> k ((( * ) ac0_n) t)) k) ac0_n);;

  $ dune exec ./cc_runner.exe << EOF
  > let nested1 = let nested2 = 5 in 
  > let nested3 = 6 in
  > let nested4 x = x + (fun i -> nested2 + nested3) 8 in nested4 55
  > EOF
  let nested1 = let nested2 = 5 in
  let nested3 = 6 in
  let nested4 nested3 nested2 x =
   (( + ) x) ((((fun nested3 nested2 i -> (( + ) nested2) nested3) nested3) nested2) 8) in
  ((nested4 nested3) nested2) 55;;

  $ dune exec ./cc_runner.exe << EOF
  > let rec facCPS n k = match n with
  >   | 0 -> k 1
  >   | n -> facCPS (n - 1) (fun t -> k (n * t))
  > EOF
  let rec facCPS n k =
   match n with
    | 0 -> k 1
    | ac0_n -> (facCPS ((( - ) ac0_n) 1)) (((fun k t ac0_n -> k ((( * ) ac0_n) t)) k) ac0_n);;

  $ dune exec ./cc_runner.exe << EOF
  > let _start () () ()  () =
  > let () = print_int 5 in 5
  > let main = print_int (_start (print_int 1) (print_int 2)  (print_int 3)   (print_int 4))
  > EOF
  let _start () () () () =
   let () = print_int 5 in
  5;;
  let ac0_main = print_int ((((_start (print_int 1)) (print_int 2)) (print_int 3)) (print_int 4));;
