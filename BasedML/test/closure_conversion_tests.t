  $ dune exec ./closure_conversion_demo.exe << EOF
  > let prog = 5 + 5
  > EOF
  let  prog = ((( + ) 5) 5)

  $ dune exec ./closure_conversion_demo.exe << EOF
  > let test1 x = let test2 y = x + y in test2
  > EOF
  let  test1 = (fun x -> (let  test2 = (fun x -> (fun y -> ((( + ) x) y))) in (test2 x)))

  $ dune exec ./closure_conversion_demo.exe << EOF
  > let test1 (x, y) = let test2 i = (x, y, i) in test2
  > EOF
  let  test1 = (fun (x, y) -> (let  test2 = (fun x -> (fun y -> (fun i -> (x, y, i)))) in ((test2 x) y)))

  $ dune exec ./closure_conversion_demo.exe << EOF
  > let test1 (x, y) = let test2 i = (x, y, i) in test2
  > EOF
  let  test1 = (fun (x, y) -> (let  test2 = (fun x -> (fun y -> (fun i -> (x, y, i)))) in ((test2 x) y)))

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
  let  nested1 = (let  nested2 = 5 in (let  nested3 = 6 in (let  nested4 = (fun nested2 -> (fun nested3 -> (fun x -> ((( + ) x) ((((fun nested2 -> (fun nested3 -> (fun i -> ((( + ) nested2) nested3)))) nested2) nested3) 8))))) in (((nested4 nested2) nested3) 55))))

  $ dune exec ./closure_conversion_demo.exe << EOF
  > let rec facCPS n k = match n with
  >   | 0 -> k 1
  >   | n -> facCPS (n - 1) (fun t -> k (n * t))
  > EOF
  let rec facCPS = (fun n -> (fun k -> (match n with
  | 0 -> (k 1)
  | n -> ((facCPS ((( - ) n) 1)) (((fun k -> (fun n -> (fun t -> (k ((( * ) n) t))))) k) n)))))

  $ dune exec ./closure_conversion_demo.exe << EOF
  > let fibo = fun n -> let rec fiboCPS = fun n acc -> match n with
  >   | 0 -> acc 0
  >   | 1 -> acc 1
  >   | _ -> fiboCPS (n - 1) (fun x -> fiboCPS (n - 2) (fun y -> acc (x + y)))
  > in
  > fiboCPS n (fun x -> x)
  > EOF
  let  fibo = (fun n -> (let rec fiboCPS = (fun n -> (fun acc -> (match n with
  | 0 -> (acc 0)
  | 1 -> (acc 1)
  | _ -> ((fiboCPS ((( - ) n) 1)) ((((fun acc -> (fun fiboCPS -> (fun n -> (fun x -> ((fiboCPS ((( - ) n) 2)) (((fun acc -> (fun x -> (fun y -> (acc ((( + ) x) y))))) acc) x)))))) acc) fiboCPS) n))))) in ((fiboCPS n) (fun x -> x))))



