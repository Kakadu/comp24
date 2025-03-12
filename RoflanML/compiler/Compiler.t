  $ clang-16 -std=c++17 -c runtime.cpp -o runtime.o
  $ for prog in ../manytests/typed/*; do
  > echo "\n--------------- $prog\n"
  > dune exec ./Compiler.exe < $prog > out.ll
  > clang-16 -Wno-override-module -lstdc++ -std=c++17 -lffi out.ll runtime.o -o a.out
  > cat $prog
  > echo "\nOutput:\n"
  > ./a.out
  > rm out.ll
  > rm a.out
  > done
  
  --------------- ../manytests/typed/001fac.ml
  
  let rec fac n = if n<=1 then 1 else n * fac (n-1)
  
  let main =
    let () = print_int (fac 4) in
    0
  
  
  Output:
  
  24
  
  --------------- ../manytests/typed/002fac.ml
  
  let rec fac_cps n k =
    if n=1 then k 1 else
    fac_cps (n-1) (fun p -> k (p*n))
  
  let main =
    let () = print_int (fac_cps 4 (fun print_int -> print_int)) in
    0
  
  
  Output:
  
  24
  
  --------------- ../manytests/typed/003fib.ml
  
  let rec fib_acc a b n =
    if n=1 then b
    else
      let n1 = n-1 in
      let ab = a+b in
      fib_acc b ab n1
  
  let rec fib n =
    if n<2
    then n
    else fib (n - 1) + fib (n - 2) 
  
  let main =
    let () = print_int (fib_acc 0 1 4) in
    let () = print_int (fib 4) in
    0
  
  
  Output:
  
  3
  3
  
  --------------- ../manytests/typed/004manyargs.ml
  
  let wrap f = if 1 = 1 then f else f
  
  let test3 a b c =
    let a = print_int a in
    let b = print_int b in
    let c = print_int c in
    0
  
  let test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j
  
  let main =
    let rez =
        (wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000
           1000000000)
    in
    let () = print_int rez in
    let temp2 = wrap test3 1 10 100 in
    0
  
  
  Output:
  
  1111111111
  1
  10
  100
  
  --------------- ../manytests/typed/005fix.ml
  
  let rec fix f x = f (fix f) x
  
  let fac self n = if n<=1 then 1 else n * self (n-1)
  
  let main =
    let () = print_int (fix fac 6) in
    0
  
  
  Output:
  
  720
  
  --------------- ../manytests/typed/006partial.ml
  
  let foo b = if b then (fun foo -> foo+2) else (fun foo -> foo*10)
  
  let foo x = foo true (foo false (foo true (foo false x)))
  let main =
    let () = print_int (foo 11) in
    0
  Output:
  
  1122
  
  --------------- ../manytests/typed/006partial2.ml
  
  let foo a b c =
    let () = print_int a in
    let () = print_int b in
    let () = print_int c in
    a + b * c
  
  let main =
    let foo = foo 1 in
    let foo = foo 2 in
    let foo = foo 3 in
    let () = print_int foo in
    0
  Output:
  
  1
  2
  3
  7
  
  --------------- ../manytests/typed/006partial3.ml
  
  let foo a =
    let () = print_int a in fun b ->
    let () = print_int b in fun c ->
    print_int c
  
  let main =
    let () = foo 4 8 9 in
    0
  Output:
  
  4
  8
  9
  $ for prog in tests/*; do
  > echo "\n--------------- $prog\n"
  > dune exec ./Compiler.exe < $prog > out.ll
  > clang-16 -Wno-override-module -lstdc++ -std=c++17 -lffi out.ll runtime.o -o a.out
  > cat $prog
  > echo "\nOutput:\n"
  > ./a.out
  > rm out.ll
  > rm a.out
  > done
  
  --------------- tests/017op.ml
  
  let ( + ) x y z = x + y - z
  
  let ( - ) x y = x * y
  
  let ( * ) x y = x + y
  
  let main = let () = print_int ((+) 1 2 3) in 
  let () = print_int (1 - 2) in 
  let () = print_int ((2 * 3) 4) in 0
  Output:
  
  0
  2
  1
  
  --------------- tests/018print.ml
  
  let print_int x = print_bool x
  let () = print_int (true && false)
  
  Output:
  
  false
  
  --------------- tests/019and.ml
  
  let rec fac n = if n <= 1 then 1 else n * another_fac (n - 1)
  and another_fac n = if n <= 1 then 1 else n * fac (n - 1)
  
  let main =
    let () = print_int (fac 4) in
    let () = print_int (another_fac 4) in
    0
  
  
  Output:
  
  24
  24

