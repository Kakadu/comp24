  $ dune exec ./lifting_demo.exe << EOF
  > let prog = 5 + 5
  > EOF
  let  prog  = ((( + ) 5) 5)

  $ dune exec ./lifting_demo.exe << EOF
  > let test1 x = let test2 y = x + y in test2
  > EOF
  let  ll_0 ( + ) x y  = ((( + ) x) y)
  let  test1 x  = ((ll_0 ( + )) x)

  $ dune exec ./lifting_demo.exe << EOF
  > let test1 (x, y) = let test2 i = (x, y, i) in test2
  > EOF
  let  ll_0 x y i  = (x, y, i)
  let  test1 (x, y)  = ((ll_0 x) y)

  $ dune exec ./lifting_demo.exe << EOF
  > let test1 (x, y) = let test2 i = (x, y, i) in test2
  > EOF
  let  ll_0 x y i  = (x, y, i)
  let  test1 (x, y)  = ((ll_0 x) y)

  $ dune exec ./lifting_demo.exe << EOF
  > let rec facCPS n k = match n with
  >   | 0 -> k 1
  >   | n -> facCPS (n - 1) (fun t -> k (n * t))
  > EOF
  let  ll_0 ( * ) k n t  = (k ((( * ) n) t))
  let rec facCPS n k  = (match n with 
  | 0 -> (k 1)
  | n -> ((facCPS ((( - ) n) 1)) (((ll_0 ( * )) k) n)))

  $ dune exec ./lifting_demo.exe << EOF
  > let nested1 = let nested2 = 5 in 
  > let nested3 = 6 in
  > let nested4 x = x + (fun i -> nested2 + nested3) 8 in nested4 55
  > EOF
  let  ll_1 ( + ) nested2 nested3 i  = ((( + ) nested2) nested3)
  let  ll_0 ( + ) nested2 nested3 x  = ((( + ) x) ((((ll_1 ( + )) nested2) nested3) 8))
  let  nested1  = (let  nested2 = 5 in (let  nested3 = 6 in ((((ll_0 ( + )) nested2) nested3) 55)))

  $ dune exec ./lifting_demo.exe << EOF
  > let rec facCPS n k = match n with
  >   | 0 -> k 1
  >   | n -> facCPS (n - 1) (fun t -> k (n * t))
  > EOF
  let  ll_0 ( * ) k n t  = (k ((( * ) n) t))
  let rec facCPS n k  = (match n with 
  | 0 -> (k 1)
  | n -> ((facCPS ((( - ) n) 1)) (((ll_0 ( * )) k) n)))

  $ dune exec ./lifting_demo.exe << EOF
  > let fibo = fun n -> let rec fiboCPS = fun n acc -> match n with
  >   | 0 -> acc 0
  >   | 1 -> acc 1
  >   | _ -> fiboCPS (n - 1) (fun x -> fiboCPS (n - 2) (fun y -> acc (x + y)))
  > in
  > fiboCPS n (fun x -> x)
  > EOF
  let  ll_2 ( + ) acc x y  = (acc ((( + ) x) y))
  let  ll_1 fiboCPS ( + ) ( - ) acc n x  = ((((fiboCPS ( + )) ( - )) ((( - ) n) 2)) (((ll_2 ( + )) acc) x))
  let rec ll_0 ( + ) ( - ) n acc  = (match n with 
  | 0 -> (acc 0)
  | 1 -> (acc 1)
  | _ -> ((((ll_0 ( + )) ( - )) ((( - ) n) 1)) (((((ll_1 ll_0) ( + )) ( - )) acc) n)))
  let  ll_3 x  = x
  let  fibo n  = ((((ll_0 ( + )) ( - )) n) ll_3)



