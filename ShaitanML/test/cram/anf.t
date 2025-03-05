  $ anf << EOF
  > let fac n =
  >   let rec fack n k =
  >     if n <= 1 then k 1
  >     else fack (n-1) ((fun k n m -> k (m * n)) k n)
  >   in
  >   fack n (fun x -> x)
  let a4 = (fun a1 a2 m -> (a1 ((( * ) m) a2)))
  
  let rec a3 = (fun a0 k -> if ((( <= ) a0) 1)
  then (k 1)
  else ((a3 ((( - ) a0) 1)) ((a4 k) a0)))
  
  let a5 = (fun x -> x)
  
  let fac = (fun n -> ((a3 n) a5))

  $ anf << EOF
  > let sum x =
  >   let new_sum y = x + y in
  >   new_sum 5
  > EOF
  let a0 = (fun x y -> ((( + ) x) y))
  
  let sum = (fun x -> ((a0 x) 5))

  $ anf << EOF
  >   let x y =
  >   let z a = a (y + 1) in
  >   z (fun x -> x)
  let a1 = (fun y a -> (a ((( + ) y) 1)))
  
  let a2 = (fun a0 -> a0)
  
  let x = (fun y -> ((a1 y) a2))

  $ anf << EOF
  >     let fibo n =
  >       let rec fibo_cps n acc =
  >       if n < 3
  >       then acc 1
  >       else fibo_cps (n - 1) (fun x -> fibo_cps (n - 2) (fun y -> acc (x + y)))
  >       in
  >       fibo_cps n (fun x -> x)
  let a3 = (fun acc x y -> (acc ((( + ) x) y)))
  
  let a2 = (fun a0 acc fibo_cps x -> ((fibo_cps ((( - ) a0) 2)) ((a3 acc) x)))
  
  let rec a1 = (fun a0 acc -> if ((( < ) a0) 3)
  then (acc 1)
  else ((a1 ((( - ) a0) 1)) (((a2 a0) acc) a1)))
  
  let a4 = (fun x -> x)
  
  let fibo = (fun n -> ((a1 n) a4))

  $ anf << EOF
  > let test_cons =
  >   let (h::t) = [1; 2; 3] in
  >   h + 1
  let test_cons = let a0 = (1::(2::(3::[]))) in
  if ((( > ) (list_len a0)) 0)
  then let h = (list_head a0) in
  let t = (list_tail a0) in
  ((( + ) h) 1)
  else fail_match


  $ anf << EOF
  > let test_tuple =
  >   let (x, (y, z)) = (5, (10, 15)) in
  >   x + y + z
  let test_tuple = let a0 = (5, (10, 15)) in
  let x = ((tuple_element a0) 0) in
  let y = ((tuple_element ((tuple_element a0) 1)) 0) in
  let z = ((tuple_element ((tuple_element a0) 1)) 1) in
  ((( + ) ((( + ) x) y)) z)


  $ anf << EOF
  > let test_list =
  >   let lst = [1; 2; 3; 4; 5] in
  >   let rec sum lst = match lst with
  >   | [] -> 0
  >   | h::t -> h + (sum t)
  > in
  > sum lst
  let rec a1 = (fun a0 -> if ((( = ) a0) [])
  then 0
  else if ((( > ) (list_len a0)) 0)
  then let h = (list_head a0) in
  let t = (list_tail a0) in
  ((( + ) h) (a1 t))
  else fail_match)
  
  let test_list = let lst = (1::(2::(3::(4::(5::[]))))) in
  (a1 lst)


  $ anf << EOF
  > let test_multiple_cases x =
  >   match x with
  >   | 0 -> "zero"
  >   | 1 -> "one"
  >   | _ -> "other"
  let test_multiple_cases = (fun x -> if ((( = ) x) 0)
  then zero
  else if ((( = ) x) 1)
  then one
  else other)

  $ anf < manytests/typed/001fac.ml
  let rec fac = (fun n -> if ((( <= ) n) 1)
  then 1
  else ((( * ) n) (fac ((( - ) n) 1))))
  
  let main = let () = (print_int (fac 4)) in
  0

  $ anf < manytests/typed/002fac.ml
  let a1 = (fun k n p -> (k ((( * ) p) n)))
  
  let rec fac_cps = (fun n k -> if ((( = ) n) 1)
  then (k 1)
  else ((fac_cps ((( - ) n) 1)) ((a1 k) n)))
  
  let a2 = (fun a0 -> a0)
  
  let main = let () = (print_int ((fac_cps 4) a2)) in
  0

  $ anf < manytests/typed/003fib.ml
  let rec fib_acc = (fun a b n -> if ((( = ) n) 1)
  then b
  else let n1 = ((( - ) n) 1) in
  let ab = ((( + ) a) b) in
  (((fib_acc b) ab) n1))
  
  let rec fib = (fun n -> if ((( < ) n) 2)
  then n
  else ((( + ) (fib ((( - ) n) 1))) (fib ((( - ) n) 2))))
  
  let main = let () = (print_int (((fib_acc 0) 1) 4)) in
  let () = (print_int (fib 4)) in
  0

  $ anf < manytests/typed/004manyargs.ml
  let wrap = (fun f -> if ((( = ) 1) 1)
  then f
  else f)
  
  let test3 = (fun a0 a1 a2 a b c -> let a0 = (print_int a0) in
  let a1 = (print_int a1) in
  let a2 = (print_int a2) in
  0)
  
  let test10 = (fun a b c d e f g h i j -> ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) a) b)) c)) d)) e)) f)) g)) h)) i)) j))
  
  let main = let rez = (((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000) in
  let () = (print_int rez) in
  let temp2 = ((((wrap test3) 1) 10) 100) in
  0

  $ anf < manytests/typed/005fix.ml
  let rec fix = (fun f x -> ((f (fix f)) x))
  
  let fac = (fun self n -> if ((( <= ) n) 1)
  then 1
  else ((( * ) n) (self ((( - ) n) 1))))
  
  let main = let () = (print_int ((fix fac) 6)) in
  0

  $ anf < manytests/typed/006partial.ml
  let a3 = (fun a0 -> ((( + ) a0) 2))
  
  let a4 = (fun a1 -> ((( * ) a1) 10))
  
  let foo = (fun b -> if b
  then a3
  else a4)
  
  let a2 = (fun a2 x -> ((a2 true) ((a2 false) ((a2 true) ((a2 false) x)))))
  
  let main = let () = (print_int (a2 11)) in
  0

  $ anf < manytests/typed/006partial2.ml
  let foo = (fun a b c -> let () = (print_int a) in
  let () = (print_int b) in
  let () = (print_int c) in
  ((( + ) a) ((( * ) b) c)))
  
  let main = let a0 = (a0 1) in
  let a1 = (a1 2) in
  let a2 = (a2 3) in
  let () = (print_int a2) in
  0

  $ anf < manytests/typed/006partial3.ml
  let a1 = (fun c -> (print_int c))
  
  let a0 = (fun b -> let () = (print_int b) in
  a1)
  
  let foo = (fun a -> let () = (print_int a) in
  a0)
  
  let main = let () = (((foo 4) 8) 9) in
  0

  $ anf < manytests/typed/007order.ml
  let _start = (fun () () a () b _c () d __ -> let () = (print_int ((( + ) a) b)) in
  let () = (print_int __) in
  ((( + ) ((( / ) ((( * ) a) b)) _c)) d))
  
  let main = print_int

  $ anf < manytests/typed/008ascription.ml
  let addi = (fun f g x -> ((f x) (g x)))
  
  let a0 = (fun x b -> if b
  then ((( + ) x) 1)
  else ((( * ) x) 2))
  
  let a1 = (fun _start -> ((( = ) ((( / ) _start) 2)) 0))
  
  let main = let () = (print_int (((addi a0) a1) 4)) in
  0

  $ anf < manytests/typed/009let_poly.ml
  let a0 = (fun x -> x)
  
  let temp = ((a0 1), (a0 true))
