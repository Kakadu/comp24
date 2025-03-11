  $ clang-16 -c runtime.c -o runtime.o

  $ llvm_codegen < manytests/typed/001fac.ml
  $ clang-16 out.ll runtime.o -lffi -o demo_001fac
  $ ./demo_001fac
  24

  $ llvm_codegen < manytests/typed/002fac.ml
  $ clang-16 out.ll runtime.o -lffi -o demo_002fac
  $ ./demo_002fac
  24

  $ llvm_codegen < manytests/typed/003fib.ml
  $ clang-16 out.ll runtime.o -lffi -o demo_003fib
  $ ./demo_003fib
  3
  3

  $ llvm_codegen < manytests/typed/004manyargs.ml
  $ clang-16 out.ll runtime.o -lffi -o demo_004manyargs
  $ ./demo_004manyargs
  1111111111
  1
  10
  100

  $ llvm_codegen < manytests/typed/005fix.ml
  $ clang-16 out.ll runtime.o -lffi -o demo_005fix
  $ echo $(./demo_005fix)
  720

  $ llvm_codegen < manytests/typed/006partial.ml
  $ clang-16 out.ll runtime.o -lffi -o demo_006partial
  $ ./demo_006partial
  1122

  $ llvm_codegen < manytests/typed/006partial2.ml
  $ clang-16 out.ll runtime.o -lffi -o demo_006partial2
  $ ./demo_006partial2
  1
  2
  3
  7

  $ llvm_codegen < manytests/typed/006partial3.ml
  $ clang-16 out.ll runtime.o -lffi -o demo_006partial3
  $ ./demo_006partial3
  4
  8
  9

  $ llvm_codegen < manytests/typed/008ascription.ml
  $ clang-16 out.ll runtime.o -lffi -o demo_008asciption
  $ ./demo_008asciption
  8

  $ llvm_codegen < manytests/typed/012fibcps.ml
  $ clang-16 out.ll runtime.o -lffi -o demo_012fibcps
  $ ./demo_012fibcps
  8
