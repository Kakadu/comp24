  $ alpha << EOF
  > let a = 1
  > EOF
  let a = 1

  $ alpha << EOF
  > let a, b = 5, 6
  let a0 = (5, 6)
  
  let a = ((tuple_element a0) 0)
  
  let b = ((tuple_element a0) 1)

  $ alpha << EOF
  > let rec fac x = if x < 1 then 1 else x * fac (x - 1)
  > EOF
  let rec fac = (fun x -> if ((( < ) x) 1)
  then 1
  else ((( * ) x) (fac ((( - ) x) 1))))

  $ alpha << EOF
  > let rec fac x =
  >  let rec helper acc x =
  >    if x < 1 then acc
  >    else helper (x * acc) (x - 1)
  >  in helper 1 x
  let rec fac = (fun x -> let rec helper = (fun acc a0 -> if ((( < ) a0) 1)
  then acc
  else ((helper ((( * ) a0) acc)) ((( - ) a0) 1))) in
  ((helper 1) x))

  $ alpha << EOF
  > let f x =
  >  (fun x -> x + 1) x, (fun x -> x + 2) x
  let f = (fun x -> (((fun a0 -> ((( + ) a0) 1)) x), ((fun a1 -> ((( + ) a1) 2)) x)))

  $ alpha << EOF
  > let f (a, b) = a + 1, b + 1
  let f = (fun a0 -> let a = ((tuple_element a0) 0) in
  let b = ((tuple_element a0) 1) in
  (((( + ) a) 1), ((( + ) b) 1)))

  $ alpha << EOF
  > let f (a, b) (x::y) ((z,w)::zws) =
  >  a + b + x + z + w
  let f = (fun a0 a1 a2 -> let a3 = (a0, a1, a2) in
  if ((( && ) ((( > ) (list_len ((tuple_element a3) 1))) 0)) ((( > ) (list_len ((tuple_element a3) 2))) 0))
  then let a = ((tuple_element ((tuple_element a3) 0)) 0) in
  let b = ((tuple_element ((tuple_element a3) 0)) 1) in
  let w = (list_tail ((tuple_element a3) 1)) in
  let x = (list_head ((tuple_element a3) 1)) in
  let y = (list_tail ((tuple_element a3) 1)) in
  let z = (list_tail ((tuple_element a3) 1)) in
  let zws = (list_tail ((tuple_element a3) 1)) in
  ((( + ) ((( + ) ((( + ) ((( + ) a) b)) x)) z)) w)
  else fail_match)

  $ alpha << EOF
  > let rec map f list =
  >   match list with
  >   | [] -> []
  >   | x::xs -> x::(map f xs)
  let rec map = (fun f list -> if ((( = ) list) [])
  then []
  else if ((( > ) (list_len list)) 0)
  then let x = (list_head list) in
  let xs = (list_tail list) in
  (x::((map f) xs))
  else fail_match)


  $ alpha < manytests/typed/001fac.ml
  let rec fac = (fun n -> if ((( <= ) n) 1)
  then 1
  else ((( * ) n) (fac ((( - ) n) 1))))
  
  let main = let () = (print_int (fac 4)) in
  0

  $ alpha < manytests/typed/002fac.ml
  let rec fac_cps = (fun n k -> if ((( = ) n) 1)
  then (k 1)
  else ((fac_cps ((( - ) n) 1)) (fun p -> (k ((( * ) p) n)))))
  
  let main = let () = (print_int ((fac_cps 4) (fun a0 -> a0))) in
  0

  $ alpha < manytests/typed/003fib.ml
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

  $ alpha < manytests/typed/004manyargs.ml
  let wrap = (fun f -> if ((( = ) 1) 1)
  then f
  else f)
  
  let test3 = (fun a b c -> let a0 = (print_int a0) in
  let a1 = (print_int a1) in
  let a2 = (print_int a2) in
  0)
  
  let test10 = (fun a b c d e f g h i j -> ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) a) b)) c)) d)) e)) f)) g)) h)) i)) j))
  
  let main = let rez = (((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000) in
  let () = (print_int rez) in
  let temp2 = ((((wrap test3) 1) 10) 100) in
  0

  $ alpha < manytests/typed/005fix.ml
  let rec fix = (fun f x -> ((f (fix f)) x))
  
  let fac = (fun self n -> if ((( <= ) n) 1)
  then 1
  else ((( * ) n) (self ((( - ) n) 1))))
  
  let main = let () = (print_int ((fix fac) 6)) in
  0

  $ alpha < manytests/typed/006partial.ml
  let foo = (fun b -> if b
  then (fun a0 -> ((( + ) a0) 2))
  else (fun a1 -> ((( * ) a1) 10)))
  
  let a2 = (fun x -> ((a2 true) ((a2 false) ((a2 true) ((a2 false) x)))))
  
  let main = let () = (print_int (a2 11)) in
  0

  $ alpha < manytests/typed/006partial2.ml
  let foo = (fun a b c -> let () = (print_int a) in
  let () = (print_int b) in
  let () = (print_int c) in
  ((( + ) a) ((( * ) b) c)))
  
  let main = let a0 = (a0 1) in
  let a1 = (a1 2) in
  let a2 = (a2 3) in
  let () = (print_int a2) in
  0

  $ alpha < manytests/typed/006partial3.ml
  let foo = (fun a -> let () = (print_int a) in
  (fun b -> let () = (print_int b) in
  (fun c -> (print_int c))))
  
  let main = let () = (((foo 4) 8) 9) in
  0

  $ alpha < manytests/typed/007order.ml
  let _start = (fun () () a () b _c () d __ -> let () = (print_int ((( + ) a) b)) in
  let () = (print_int __) in
  ((( + ) ((( / ) ((( * ) a) b)) _c)) d))
  
  let main = print_int

  $ alpha < manytests/typed/008ascription.ml
  let addi = (fun f g x -> ((f x) (g x)))
  
  let main = let () = (print_int (((addi (fun x b -> if b
  then ((( + ) x) 1)
  else ((( * ) x) 2))) (fun _start -> ((( = ) ((( / ) _start) 2)) 0))) 4)) in
  0

  $ alpha < manytests/typed/009let_poly.ml
  let temp = let f = (fun x -> x) in
  ((f 1), (f true))

  $ alpha < manytests/typed/001fac.ml
  let rec fac = (fun n -> if ((( <= ) n) 1)
  then 1
  else ((( * ) n) (fac ((( - ) n) 1))))
  
  let main = let () = (print_int (fac 4)) in
  0

  $ alpha < manytests/typed/002fac.ml
  let rec fac_cps = (fun n k -> if ((( = ) n) 1)
  then (k 1)
  else ((fac_cps ((( - ) n) 1)) (fun p -> (k ((( * ) p) n)))))
  
  let main = let () = (print_int ((fac_cps 4) (fun a0 -> a0))) in
  0

  $ alpha < manytests/typed/003fib.ml
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

  $ alpha < manytests/typed/004manyargs.ml
  let wrap = (fun f -> if ((( = ) 1) 1)
  then f
  else f)
  
  let test3 = (fun a b c -> let a0 = (print_int a0) in
  let a1 = (print_int a1) in
  let a2 = (print_int a2) in
  0)
  
  let test10 = (fun a b c d e f g h i j -> ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) a) b)) c)) d)) e)) f)) g)) h)) i)) j))
  
  let main = let rez = (((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000) in
  let () = (print_int rez) in
  let temp2 = ((((wrap test3) 1) 10) 100) in
  0

  $ alpha < manytests/typed/005fix.ml
  let rec fix = (fun f x -> ((f (fix f)) x))
  
  let fac = (fun self n -> if ((( <= ) n) 1)
  then 1
  else ((( * ) n) (self ((( - ) n) 1))))
  
  let main = let () = (print_int ((fix fac) 6)) in
  0

  $ alpha < manytests/typed/006partial.ml
  let foo = (fun b -> if b
  then (fun a0 -> ((( + ) a0) 2))
  else (fun a1 -> ((( * ) a1) 10)))
  
  let a2 = (fun x -> ((a2 true) ((a2 false) ((a2 true) ((a2 false) x)))))
  
  let main = let () = (print_int (a2 11)) in
  0

  $ alpha < manytests/typed/006partial2.ml
  let foo = (fun a b c -> let () = (print_int a) in
  let () = (print_int b) in
  let () = (print_int c) in
  ((( + ) a) ((( * ) b) c)))
  
  let main = let a0 = (a0 1) in
  let a1 = (a1 2) in
  let a2 = (a2 3) in
  let () = (print_int a2) in
  0

  $ alpha < manytests/typed/006partial3.ml
  let foo = (fun a -> let () = (print_int a) in
  (fun b -> let () = (print_int b) in
  (fun c -> (print_int c))))
  
  let main = let () = (((foo 4) 8) 9) in
  0

  $ alpha < manytests/typed/007order.ml
  let _start = (fun () () a () b _c () d __ -> let () = (print_int ((( + ) a) b)) in
  let () = (print_int __) in
  ((( + ) ((( / ) ((( * ) a) b)) _c)) d))
  
  let main = print_int

  $ alpha < manytests/typed/008ascription.ml
  let addi = (fun f g x -> ((f x) (g x)))
  
  let main = let () = (print_int (((addi (fun x b -> if b
  then ((( + ) x) 1)
  else ((( * ) x) 2))) (fun _start -> ((( = ) ((( / ) _start) 2)) 0))) 4)) in
  0

  $ alpha < manytests/typed/009let_poly.ml
  let temp = let f = (fun x -> x) in
  ((f 1), (f true))
