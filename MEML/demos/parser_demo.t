  $ ./parser_demo.exe <<- EOF
  > let rec fibo_cps = fun n acc -> match n with
  >   | 0 -> acc 0
  >   | 1 -> acc 1
  >   | _ -> fibo_cps (n - 1) (fun x -> fibo_cps (n - 2) (fun y -> acc (x + y)))
  let rec fibo_cps = (fun n -> (fun acc -> (match n with
  | 0 -> acc 0
  | 1 -> acc 1
  | _ -> fibo_cps (n - 1) (fun x -> fibo_cps (n - 2) (fun y -> acc (x + y)))))

  $ ./parser_demo.exe <<- EOF
  > let rec fac_cps n k = match n with
  >   | 0 -> k 1
  >   | n -> fac_cps (n - 1) (fun t -> k (n * t));;
  let rec fac_cps = (fun n -> (fun k -> (match n with
  | 0 -> k 1
  | n -> fac_cps (n - 1) (fun t -> k (n * t))))

  $ ./parser_demo.exe <<- EOF
  > let a = f 
  > let b = 4
  > 
  > let (c, b) = (2, 3)
  let  a = f
  let  b = 4
  let  c, b = (2, 3)
  $ ./parser_demo.exe < manytests/do_not_type/001.ml
  let  recfac = (fun n -> 
    if (n <= 1)
    then 1
    else (n * fac n -1))
  $ ./parser_demo.exe < manytests/do_not_type/002if.ml
  let  main = 
    if true
    then 1
    else false
  $ ./parser_demo.exe < manytests/do_not_type/003occurs.ml
  let  fix = (fun f -> (fun x -> f (fun f -> x x f)) (fun x -> f (fun f -> x x f)))
  $ ./parser_demo.exe < manytests/do_not_type/004let_poly.ml
  let  temp = (fun f -> (f 1, f true)) (fun x -> x)
  $ ./parser_demo.exe < manytests/do_not_type/015tuples.ml
  let rec a, b = (a, b)
  $ ./parser_demo.exe < manytests/typed/001fac.ml
  let rec fac = (fun n -> 
    if (n <= 1)
    then 1
    else (n * fac n -1))
  let  main = 
    let  () = print_int fac 4
    in 0
  $ ./parser_demo.exe < manytests/typed/002fac.ml
  let rec fac_cps = (fun n -> (fun k -> 
    if (n = 1)
    then k 1
    else fac_cps n -1 (fun p -> k (p * n))))
  let  main = 
    let  () = print_int fac_cps 4 (fun print_int -> print_int)
    in 0
  $ ./parser_demo.exe < manytests/typed/003fib.ml
  let rec fib_acc = (fun a -> (fun b -> (fun n -> 
    if (n = 1)
    then b
    else 
    let  n1 = n -1
    in 
    let  ab = (a + b)
    in fib_acc b ab n1)))
  let rec fib = (fun n -> 
    if (n < 2)
    then n
    else (fib (n - 1) + fib (n - 2)))
  let  main = 
    let  () = print_int fib_acc 0 1 4
    in 
    let  () = print_int fib 4
    in 0
  $ ./parser_demo.exe < manytests/typed/004manyargs.ml
  let  wrap = (fun f -> 
    if (1 = 1)
    then f
    else f)
  let  test3 = (fun a -> (fun b -> (fun c -> 
    let  a = print_int a
    in 
    let  b = print_int b
    in 
    let  c = print_int c
    in 0)))
  let  test10 = (fun a -> (fun b -> (fun c -> (fun d -> (fun e -> (fun f -> (fun g -> (fun h -> (fun i -> (fun j -> (((((((((a + b) + c) + d) + e) + f) + g) + h) + i) + j)))))))))))
  let  main = 
    let  rez = wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000
    in 
    let  () = print_int rez
    in 
    let  temp2 = wrap test3 1 10 100
    in 0
  $ ./parser_demo.exe < manytests/typed/005fix.ml
  let rec fix = (fun f -> (fun x -> f fix f x))
  let  fac = (fun self -> (fun n -> 
    if (n <= 1)
    then 1
    else (n * self n -1)))
  let  main = 
    let  () = print_int fix fac 6
    in 0
  $ ./parser_demo.exe < manytests/typed/006partial.ml
  let  foo = (fun b -> 
    if b
    then (fun foo -> foo 2)
    else (fun foo -> (foo * 10)))
  let  foo = (fun x -> foo true foo false foo true foo false x)
  let  main = 
    let  () = print_int foo 11
    in 0
  $ ./parser_demo.exe < manytests/typed/006partial2.ml
  let  foo = (fun a -> (fun b -> (fun c -> 
    let  () = print_int a
    in 
    let  () = print_int b
    in 
    let  () = print_int c
    in (a + (b * c)))))
  let  main = 
    let  foo = foo 1
    in 
    let  foo = foo 2
    in 
    let  foo = foo 3
    in 
    let  () = print_int foo
    in 0
  $ ./parser_demo.exe < manytests/typed/006partial3.ml
  let  foo = (fun a -> 
    let  () = print_int a
    in (fun b -> 
    let  () = print_int b
    in (fun c -> print_int c)))
  let  main = 
    let  () = foo 4 8 9
    in 0
  $ ./parser_demo.exe < manytests/typed/007order.ml
  Parsing error : no more choices
  $ ./parser_demo.exe < manytests/typed/008ascription.ml
  Parsing error : no more choices
  $ ./parser_demo.exe < manytests/typed/009let_poly.ml
  let  temp = 
    let  f = (fun x -> x)
    in (f 1, f true)
  $ ./parser_demo.exe < manytests/typed/010sukharev.ml
  Parsing error : end_of_input
  $ ./parser_demo.exe < manytests/typed/015tuples.ml
  Parsing error : end_of_input
  $ ./parser_demo.exe < manytests/typed/016lists.ml
  Parsing error : end_of_input

  $ ./parser_demo.exe <<- EOF
  > let _1 = fun x y (a, _) -> (x + y - a) = 1