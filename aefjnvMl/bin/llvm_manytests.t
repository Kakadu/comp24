  $ clang-16 -c -fPIC ../lib/llvm/runtime/rt_funcs.c -o libMLstd.a

  $ ./llvm_runner.exe < manytests/do_not_type/001.ml
  Unbound value 'fac'


  $ ./llvm_runner.exe < manytests/do_not_type/002if.ml
  This expression has type bool but an expression was expected of type int


  $ ./llvm_runner.exe < manytests/do_not_type/003occurs.ml
  The type variable 'a occurs inside 'a -> 'b


  $ ./llvm_runner.exe < manytests/do_not_type/004let_poly.ml
  This expression has type int but an expression was expected of type bool


  $ ./llvm_runner.exe < manytests/do_not_type/015tuples.ml
  Only variables are allowed as left-side of 'let rec'


  $ ./llvm_runner.exe < manytests/typed/001fac.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo001_fac.exe
  $ ./demo001_fac.exe
  24

  $ ./llvm_runner.exe < manytests/typed/002fac.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo002_fac.exe
  $ ./demo002_fac.exe
  24

  $ ./llvm_runner.exe < manytests/typed/003fib.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo003_fib.exe
  $ ./demo003_fib.exe
  3
  3

  $ ./llvm_runner.exe < manytests/typed/004manyargs.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo004_manyargs.exe
  $ ./demo004_manyargs.exe
  1111111111
  1
  10
  100

  $ ./llvm_runner.exe < manytests/typed/005fix.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo005_fix.exe
  $ ./demo005_fix.exe
  720

  $ ./llvm_runner.exe < manytests/typed/006partial2.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo006_partial2.exe
  $ ./demo006_partial2.exe
  1
  2
  3
  7

  $ ./llvm_runner.exe < manytests/typed/006partial3.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo006_partial3.exe
  $ ./demo006_partial3.exe
  4
  8
  9

  $ ./llvm_runner.exe < manytests/typed/007order.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo007_order.exe
  $ ./demo007_order.exe
  -1
  4
  2
  1
  103
  -555555
  10000

  $ ./llvm_runner.exe < manytests/typed/008ascription.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo008_ascription.exe
  $ ./demo008_ascription.exe
  8

  $ ./llvm_runner.exe < manytests/typed/009let_poly.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo009_let_poly.exe
  $ ./demo009_let_poly.exe

  $ ./llvm_runner.exe < manytests/typed/011mapcps.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo011_mapcps.exe
  $ ./demo011_mapcps.exe
  2
  3
  4

  $ ./llvm_runner.exe < manytests/typed/013foldfoldr.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo013_foldfoldr.exe
  $ ./demo013_foldfoldr.exe
  6

  $ ./llvm_runner.exe < manytests/typed/015tuples.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo015_tuples.exe
  $ ./demo015_tuples.exe
  1
  1
  1
  1

  $ ./llvm_runner.exe < manytests/typed/016lists.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo016_lists.exe
  $ ./demo016_lists.exe
  1
  2
  3
  8
