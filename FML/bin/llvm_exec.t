  $ ./compiler.exe < manytests/typed/001fac.ml
  $ clang-16 out.ll ../lib/llvm/runtime.o -lffi -o 001fac
  $ ./001fac
  24

  $ ./compiler.exe < manytests/typed/002fac.ml
  $ clang-16 out.ll ../lib/llvm/runtime.o -lffi -o 002fac
  $ ./002fac
  24

  $ ./compiler.exe < manytests/typed/003fib.ml
  $ clang-16 out.ll ../lib/llvm/runtime.o -lffi -o 003fib
  $ ./003fib
  33

  $ ./compiler.exe < manytests/typed/004manyargs.ml
  $ clang-16 out.ll ../lib/llvm/runtime.o -lffi -o 004manyargs
  $ ./004manyargs
  1111111111110100

  $ ./compiler.exe < manytests/typed/005fix.ml
  $ clang-16 out.ll ../lib/llvm/runtime.o -lffi -o 005fix
  $ ./005fix
  720

  $ ./compiler.exe < manytests/typed/006partial.ml
  $ clang-16 out.ll ../lib/llvm/runtime.o -lffi -o 006partial
  $ ./006partial
  1122

  $ ./compiler.exe < manytests/typed/006partial2.ml
  $ clang-16 out.ll ../lib/llvm/runtime.o -lffi -o 006partial2
  $ ./006partial2
  1237

  $ ./compiler.exe < manytests/typed/006partial3.ml
  $ clang-16 out.ll ../lib/llvm/runtime.o -lffi -o 006partial3
  $ ./006partial3
  489

  $ ./compiler.exe < manytests/typed/007order.ml
  $ clang-16 out.ll ../lib/llvm/runtime.o -lffi -o 007order
  $ ./007order
  -1421103-55555510000

  $ ./compiler.exe < manytests/typed/008ascription.ml
  $ clang-16 out.ll ../lib/llvm/runtime.o -lffi -o 008ascription
  $ ./008ascription
  8

  $ ./compiler.exe < manytests/typed/012fibcps.ml
  $ clang-16 out.ll ../lib/llvm/runtime.o -lffi -o 012fibcps && rm out.ll
  $ ./012fibcps && rm -f out.ll
  8

$ ls manytests/typed
  $ ocaml -w -26 ./x013.ml
  1111111111

  $ ./compiler.exe < ./x013.ml
  $ clang-16 out.ll ../lib/llvm/runtime.o -lffi -o 013
  $ ./013
  1111111111


