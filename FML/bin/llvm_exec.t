  $ clang-16 -c runtime.c -o runtime.o

  $ ./compiler.exe < manytests/typed/001fac.ml
  $ clang-16 out.ll runtime.o -lffi -o 001fac
  $ ./001fac