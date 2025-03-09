
  $ ./llvm_demo.exe < manytests/typed/001fac.ml | lli-16 -load ../lib/runtime.so
  24

  $ ./llvm_demo.exe < manytests/typed/002fac.ml | lli-16 -load ../lib/runtime.so
  24

  $ ./llvm_demo.exe < manytests/typed/003fib.ml | lli-16 -load ../lib/runtime.so
  3
  3
  $ ./llvm_demo.exe < manytests/typed/004manyargs.ml | lli-16 -load ../lib/runtime.so
  1111111111

  $ ./llvm_demo.exe < manytests/typed/005fix.ml | lli-16 -load ../lib/runtime.so
  720

  $ ./llvm_demo.exe < manytests/typed/006partial.ml | lli-16 -load ../lib/runtime.so
  1122

  $ ./llvm_demo.exe < manytests/typed/006partial2.ml | lli-16 -load ../lib/runtime.so
  1
  2
  3
  7

  $ ./llvm_demo.exe < manytests/typed/006partial3.ml | lli-16 -load ../lib/runtime.so
  4
  8
  9

  $ ./llvm_demo.exe < manytests/typed/007order.ml | lli-16 -load ../lib/runtime.so
  1
  2
  4
  -1
  103
  -555555
  10000

  $ ./llvm_demo.exe < manytests/typed/008ascription.ml | lli-16 -load ../lib/runtime.so
  8

  $ ./llvm_demo.exe << EOF | lli-16 -load ../lib/runtime.so
  > let sum a b = a + b
  > let add5 = sum 5
  > let main = print_int(add5 4)
  9

  $ ./llvm_demo.exe << EOF | lli-16 -load ../lib/runtime.so
  > let rec fib n k =
  > if n < 2
  > then k n
  > else fib (n - 1) (fun a -> fib (n - 2) (fun b -> k (a + b)))
  > let main = print_int(fib 6 (fun x -> x))
  8

  $ ./llvm_demo.exe << EOF | lli-16 -load ../lib/runtime.so
  > let sum a b = a + b 
  > let add a = match a with
  > | 1 -> sum 1 0
  > | 2 -> sum 2 0
  > | 3 -> sum 3 0
  > | 4 -> sum 4 0
  > | 5 -> sum 5 0
  > | _ -> sum 0 0
  >   
  > let main = 
  >   let () = print_int (add 0) in 
  >   let () = print_int (add 1) in
  >   let () = print_int (add 2) in 
  >   let () = print_int (add 3) in
  >   let () = print_int (add 4) in 
  >   let () = print_int (add 5) in
  >   let () = print_int (add 6) in
  > 0
  0
  1
  2
  3
  4
  5
  0

  $ ./llvm_demo.exe << EOF | lli-16 -load ../lib/runtime.so
  > let test = 5 
  > let test_fun a = a + test
  > let test = 6
  > let main = let () = print_int(test_fun 7) in 0
  12

  $ ./llvm_demo.exe << EOF | lli-16 -load ../lib/runtime.so
  > let bin_op = 1 + 2
  > let bin_op0 = 4 - 1
  > let ogogo = 1 + 2 + 3 + bin_op + bin_op0
  > let main = print_int(ogogo)
  12

  $ ./llvm_demo.exe << EOF | lli-16 -load ../lib/runtime.so
  > let app a b = a + b
  > let app1 a b = a - b
  > let ogogo = app (app1 3 2) (app 1 1)
  > let main = print_int(ogogo)
  3

  $ ./llvm_demo.exe << EOF | lli-16 -load ../lib/runtime.so
  > let a = print_int(2 + 1) 
  > let ( + ) a b = a - b
  > let main = let () = a in let () = print_int(2 + 1) in 0
  3
  1
