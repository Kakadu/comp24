  $ clang-16 -c runtime.c -o runtime.o

  $ ./compiler.exe < manytests/typed/001fac.ml
  $ clang-16 out.ll runtime.o -lffi -o 001fac
  $ ./001fac
  24

  $ ./compiler.exe < manytests/typed/002fac.ml
  $ clang-16 out.ll runtime.o -lffi -o 002fac
  $ ./002fac
  24

  $ ./compiler.exe < manytests/typed/003fib.ml
  $ clang-16 out.ll runtime.o -lffi -o 003fib
  $ ./003fib
  3
  3

  $ ./compiler.exe < manytests/typed/004manyargs.ml
  $ clang-16 out.ll runtime.o -lffi -o 004manyargs
  $ ./004manyargs
  1111111111
  1
  10
  100

  $ ./compiler.exe < manytests/typed/005fix.ml
  $ clang-16 out.ll runtime.o -lffi -o 005fix
  $ ./005fix
  720

  $ ./compiler.exe < manytests/typed/006partial.ml
  $ clang-16 out.ll runtime.o -lffi -o 006partial
  $ ./006partial
  1122

  $ ./compiler.exe < manytests/typed/006partial2.ml
  $ clang-16 out.ll runtime.o -lffi -o 006partial2
  $ ./006partial2
  1
  2
  3
  7

  $ ./compiler.exe < manytests/typed/006partial3.ml
  $ clang-16 out.ll runtime.o -lffi -o 006partial3
  $ ./006partial3
  4
  8
  9

  $ ./compiler.exe < manytests/typed/008ascription.ml
  $ clang-16 out.ll runtime.o -lffi -o 008ascription
  $ ./008ascription
  8

  $ ./compiler.exe < manytests/typed/012fibcps.ml
  $ clang-16 out.ll runtime.o -lffi -o 012fibcps
  $ ./012fibcps
  8
