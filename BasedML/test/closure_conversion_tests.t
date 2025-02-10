  $ dune exec ./closure_conversion_demo.exe << EOF
  > let prog = 5 + 5
  > EOF
  let  prog = ((( + ) 5) 5)

  $ dune exec ./closure_conversion_demo.exe << EOF
  > let test1 x = let test2 y = x + y in test2
  > EOF
  let  test1 = (fun x -> (let  test2_arg_1 = x in (let  test2 = (fun x -> (fun y -> ((( + ) x) y))) in (test2 test2_arg_1))))

  $ dune exec ./closure_conversion_demo.exe << EOF
  > let test1 (x, y) = let test2 i = (x, y, i) in test2
  > EOF
  let  test1 = (fun (x, y) -> (let  test2_arg_2 = y in (let  test2_arg_1 = x in (let  test2 = (fun x -> (fun y -> (fun i -> (x, y, i)))) in ((test2 test2_arg_1) test2_arg_2)))))

  $ dune exec ./closure_conversion_demo.exe << EOF
  > let test1 (x, y) = let test2 i = (x, y, i) in test2
  > EOF
  let  test1 = (fun (x, y) -> (let  test2_arg_2 = y in (let  test2_arg_1 = x in (let  test2 = (fun x -> (fun y -> (fun i -> (x, y, i)))) in ((test2 test2_arg_1) test2_arg_2)))))

  $ dune exec ./closure_conversion_demo.exe << EOF
  > let rec facCPS n k = match n with
  >   | 0 -> k 1
  >   | n -> facCPS (n - 1) (fun t -> k (n * t))
  > EOF
  let rec facCPS = (fun n -> (fun k -> (match n with
  | 0 -> (k 1)
  | n -> ((facCPS ((( - ) n) 1)) (((fun k -> (fun n -> (fun t -> (k ((( * ) n) t))))) k) n)))))

  $ dune exec ./closure_conversion_demo.exe << EOF
  > let nested1 = let nested2 = 5 in 
  > let nested3 = 6 in
  > let nested4 x = x + (fun i -> nested2 + nested3) 8 in nested4 55
  > EOF
  let  nested1 = (let  nested2 = 5 in (let  nested3 = 6 in (let  nested4_arg_2 = nested3 in (let  nested4_arg_1 = nested2 in (let  nested4 = (fun nested2 -> (fun nested3 -> (fun x -> ((( + ) x) ((((fun nested2 -> (fun nested3 -> (fun i -> ((( + ) nested2) nested3)))) nested2) nested3) 8))))) in (((nested4 nested4_arg_1) nested4_arg_2) 55))))))

  $ dune exec ./closure_conversion_demo.exe << EOF
  > let rec facCPS n k = match n with
  >   | 0 -> k 1
  >   | n -> facCPS (n - 1) (fun t -> k (n * t))
  > EOF
  let rec facCPS = (fun n -> (fun k -> (match n with
  | 0 -> (k 1)
  | n -> ((facCPS ((( - ) n) 1)) (((fun k -> (fun n -> (fun t -> (k ((( * ) n) t))))) k) n)))))

  $ dune exec ./closure_conversion_demo.exe << EOF
  > let _start () () ()  () =
  > let () = print_int 5 in 5
  > let main = print_int (_start (print_int 1) (print_int 2)  (print_int 3)   (print_int 4))
  > EOF
  let  _start = (fun () -> (fun () -> (fun () -> (fun () -> (let  () = (print_int 5) in 5)))))
  let  main = (print_int ((((_start (print_int 1)) (print_int 2)) (print_int 3)) (print_int 4)))



z
