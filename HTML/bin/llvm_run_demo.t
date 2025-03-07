  $ clang-16 -c runtime.c -o runtime.o

  $ ./llvm_demo.exe < manytests/typed/001fac.ml
  $ clang-16 out.ll runtime.o -lffi -o demo_001fac
  $ echo $(./demo_001fac)
  24

  $ ./llvm_demo.exe < manytests/typed/002fac.ml
  $ clang-16 out.ll runtime.o -lffi -o demo_002fac
  $ echo $(./demo_002fac)
  24

  $ ./llvm_demo.exe < manytests/typed/003fib.ml
  $ clang-16 out.ll runtime.o -lffi -o demo_003fib
  $ echo $(./demo_003fib)
  3 3

  $ ./llvm_demo.exe < manytests/typed/004manyargs.ml
  $ clang-16 out.ll runtime.o -lffi -o demo_004manyargs
  $ echo $(./demo_004manyargs)
  1111111111 1 10 100

  $ ./llvm_demo.exe < manytests/typed/005fix.ml
  $ clang-16 out.ll runtime.o -lffi -o demo_005fix
  $ echo $(./demo_005fix)
  720

  $ ./llvm_demo.exe < manytests/typed/006partial.ml
  $ clang-16 out.ll runtime.o -lffi -o demo_006partial
  $ echo $(./demo_006partial)
  1122

  $ ./llvm_demo.exe < manytests/typed/006partial2.ml
  $ clang-16 out.ll runtime.o -lffi -o demo_006partial2
  $ echo $(./demo_006partial2)
  1 2 3 7

  $ ./llvm_demo.exe < manytests/typed/006partial3.ml
  $ clang-16 out.ll runtime.o -lffi -o demo_006partial3
  $ echo $(./demo_006partial3)
  4 8 9

  $ ./llvm_demo.exe < manytests/typed/007order.ml
  $ clang-16 out.ll runtime.o -lffi -o demo_007order
  $ echo $(./demo_007order)
  -1 4 2 1 103 -555555 10000

  $ ./llvm_demo.exe < manytests/typed/008ascription.ml
  $ clang-16 out.ll runtime.o -lffi -o demo_008asciption
  $ echo $(./demo_008asciption)
  8

  $ ./llvm_demo.exe << EOF
  > let rec even x =
  >   if x = 0 then true
  >   else odd (x - 1)
  > and odd x =
  >   if x = 0 then false
  >   else even (x - 1)
  > let res =
  > let () = print_bool (odd 1) in
  > let () = print_bool (even 3) in
  > 0
  $ clang-16 out.ll runtime.o -lffi -o demo_008asciption
  $ echo $(./demo_008asciption)
  true false
