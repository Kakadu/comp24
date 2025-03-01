  $ ./parser_demo.exe < manytests/typed/001fac.ml
  let rec fac = (fun n -> if (n <= 1) then 1 else (n * fac (n - 1)));;
  let main = let () = (print_int (fac 4))
  in 0
  $ ./parser_demo.exe < manytests/typed/002fac.ml
  let rec fac_cps = (fun n -> (fun k -> if (n = 1) then (k 1) else (fac_cps (n - 1) (fun p -> k (p * n)))));;
  let main = let () = (print_int ((fac_cps 4) (fun print_int -> print_int)))
  in 0
  $ ./parser_demo.exe < manytests/typed/003fib.ml
  let rec fib_acc = (fun a -> (fun b -> (fun n -> if (n = 1) then b else let n1 = (n - 1)
  in let ab = (a + b)
  in (((fib_acc b) ab) n1))));;
  let rec fib = (fun n -> if (n < 2) then n else (fib (n - 1) + fib (n - 2)));;
  let main = let () = (print_int (((fib_acc 0) 1) 4))
  in let () = (print_int (fib 4))
  in 0
  $ ./parser_demo.exe < manytests/typed/004manyargs.ml
  let wrap = (fun f -> if (1 = 1) then f else f);;
  let test3 = (fun a -> (fun b -> (fun c -> let a = (print_int a)
  in let b = (print_int b)
  in let c = (print_int c)
  in 0)));;
  let test10 = (fun a -> (fun b -> (fun c -> (fun d -> (fun e -> (fun f -> (fun g -> (fun h -> (fun i -> (fun j -> (((((((((a + b) + c) + d) + e) + f) + g) + h) + i) + j)))))))))));;
  let main = let rez = (((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000)
  in let () = (print_int rez)
  in let temp2 = ((((wrap test3) 1) 10) 100)
  in 0
  $ ./parser_demo.exe < manytests/typed/005fix.ml
  let rec fix = (fun f -> (fun x -> ((f (fix f)) x)));;
  let fac = (fun self -> (fun n -> if (n <= 1) then 1 else (n * self (n - 1))));;
  let main = let () = (print_int ((fix fac) 6))
  in 0
  $ ./parser_demo.exe < manytests/typed/006partial.ml
  let foo = (fun b -> if b then (fun foo -> (foo + 2)) else (fun foo -> (foo * 10)));;
  let foo = (fun x -> ((foo true) ((foo false) ((foo true) ((foo false) x)))));;
  let main = let () = (print_int (foo 11))
  in 0
  $ ./parser_demo.exe < manytests/typed/006partial2.ml
  let foo = (fun a -> (fun b -> (fun c -> let () = (print_int a)
  in let () = (print_int b)
  in let () = (print_int c)
  in (a + (b * c)))));;
  let main = let foo = (foo 1)
  in let foo = (foo 2)
  in let foo = (foo 3)
  in let () = (print_int foo)
  in 0
  $ ./parser_demo.exe < manytests/typed/006partial3.ml
  let foo = (fun a -> let () = (print_int a)
  in (fun b -> let () = (print_int b)
  in (fun c -> (print_int c))));;
  let main = let () = (((foo 4) 8) 9)
  in 0
  $ ./parser_demo.exe < manytests/typed/007order.ml
  let _start = (fun () -> (fun () -> (fun a -> (fun () -> (fun b -> (fun _c -> (fun () -> (fun d -> (fun __ -> let () = print_int (a + b)
  in let () = (print_int __)
  in (((a * b) / _c) + d))))))))));;
  let main = (print_int (((((((((_start (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int (- 1))) 10000) (- 555555)))
  $ ./parser_demo.exe < manytests/typed/008ascription.ml
  let addi = (fun f -> (fun g -> (fun x -> (((f x) ((g x) : bool)) : int))));;
  let main = let () = (print_int (((addi (fun x -> (fun b -> if b then (x + 1) else (x * 2)))) (fun _start -> ((_start / 2) = 0))) 4))
  in 0
  $ ./parser_demo.exe < manytests/typed/009let_poly.ml
  let temp = let f = (fun x -> x)
  in ((f 1), (f true))
  $ ./parser_demo.exe < manytests/typed/015tuples.ml
  let rec fix = (fun f -> (fun x -> ((f (fix f)) x)));;
  let map = (fun f -> (fun p -> let (a, b) = p
  in ((f a), (f b))));;
  let fixpoly = (fun l -> ((fix (fun self -> (fun l -> ((map (fun li -> (fun x -> ((li (self l)) x)))) l)))) l));;
  let feven = (fun p -> (fun n -> let (e, o) = p
  in if (n == 0) then 1 else o (n - 1)));;
  let fodd = (fun p -> (fun n -> let (e, o) = p
  in if (n == 0) then 0 else e (n - 1)));;
  let tie = (fixpoly (feven, fodd));;
  let rec meven = (fun n -> if (n = 0) then 1 else modd (n - 1))
  and modd = (fun n -> if (n = 0) then 1 else meven (n - 1));;
  let main = let () = (print_int (modd 1))
  in let () = (print_int (meven 2))
  in let (even, odd) = tie
  in let () = (print_int (odd 3))
  in let () = (print_int (even 4))
  in 0
  $ ./parser_demo.exe < manytests/typed/016lists.ml
  let rec length = (fun xs -> match xs with
  | [] -> 0
  | h :: tl -> (1 + (length tl)));;
  let length_tail = let rec helper = (fun acc -> (fun xs -> match xs with
  | [] -> acc
  | h :: tl -> (helper (acc + 1) tl)))
  in (helper 0);;
  let rec map = (fun f -> (fun xs -> match xs with
  | [] -> []
  | a :: [] -> (f a) :: []
  | a :: b :: [] -> (f a) :: (f b) :: []
  | a :: b :: c :: [] -> (f a) :: (f b) :: (f c) :: []
  | a :: b :: c :: d :: tl -> (f a) :: (f b) :: (f c) :: (f d) :: ((map f) tl)));;
  let rec append = (fun xs -> (fun ys -> match xs with
  | [] -> ys
  | x :: xs -> x :: ((append xs) ys)));;
  let concat = let rec helper = (fun xs -> match xs with
  | [] -> []
  | h :: tl -> ((append h) (helper tl)))
  in helper;;
  let rec iter = (fun f -> (fun xs -> match xs with
  | [] -> ()
  | h :: tl -> let () = (f h)
  in ((iter f) tl)));;
  let rec cartesian = (fun xs -> (fun ys -> match xs with
  | [] -> []
  | h :: tl -> ((append ((map (fun a -> (h, a))) ys)) ((cartesian tl) ys))));;
  let main = let () = ((iter print_int) 1 :: 2 :: 3 :: [])
  in let () = (print_int (length ((cartesian 1 :: 2 :: []) 1 :: 2 :: 3 :: 4 :: [])))
  in 0
  $ ./parser_demo.exe < manytests/do_not_type/001.ml
  let recfac = (fun n -> if (n <= 1) then 1 else (n * fac (n - 1)))

  $ ./parser_demo.exe < manytests/do_not_type/002if.ml
  let main = if true then 1 else false

  $ ./parser_demo.exe < manytests/do_not_type/003occurs.ml
  let fix = (fun f -> ((fun x -> (f (fun f -> ((x x) f)))) (fun x -> (f (fun f -> ((x x) f))))))

  $ ./parser_demo.exe < manytests/do_not_type/004let_poly.ml
  let temp = ((fun f -> ((f 1), (f true))) (fun x -> x))

  $ ./parser_demo.exe < manytests/do_not_type/015tuples.ml
  let rec (a, b) = (a, b)

