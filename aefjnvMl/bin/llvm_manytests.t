  $ clang-16 -c -fPIC ../lib/llvm/runtime/rt_funcs.c -o libMLstd.a

  $ ./llvm_runner.exe < manytests/typed/001fac.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo001_fac.exe
  $ echo $(./demo001_fac.exe)
  24
  

  $ ./llvm_runner.exe < manytests/typed/002fac.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo002_fac.exe
  $ echo $(./demo002_fac.exe)
  24
  

  $ ./llvm_runner.exe < manytests/typed/003fib.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo003_fib.exe
  $ echo $(./demo003_fib.exe)
  3
  3
  

  $ ./llvm_runner.exe < manytests/typed/004manyargs.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo004_manyargs.exe
  $ echo $(./demo004_manyargs.exe)
  1111111111
  1
  10
  100
  

  $ ./llvm_runner.exe < manytests/typed/005fix.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo005_fix.exe
  $ echo $(./demo005_fix.exe)
  720
  

  $ ./llvm_runner.exe < manytests/typed/006partial2.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo006_partial2.exe
  $ echo $(./demo006_partial2.exe)
  1
  2
  3
  7
  

  $ ./llvm_runner.exe < manytests/typed/006partial3.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo006_partial3.exe
  $ echo $(./demo006_partial3.exe)
  4
  8
  9
  

  $ ./llvm_runner.exe < manytests/typed/007order.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo007_order.exe
  $ echo $(./demo007_order.exe)
  -1
  4
  2
  1
  103
  -555555
  10000
  

  $ ./llvm_runner.exe < manytests/typed/008ascription.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo008_ascription.exe
  $ echo $(./demo008_ascription.exe)
  8
  

  $ ./llvm_runner.exe < manytests/typed/009let_poly.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo009_let_poly.exe
  $ echo $(./demo009_let_poly.exe)
  

  $ ./llvm_runner.exe < manytests/typed/011mapcps.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo011_mapcps.exe
  $ echo $(./demo011_mapcps.exe)
  2
  3
  4
  

  $ ./llvm_runner.exe < manytests/typed/013foldfoldr.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo013_foldfoldr.exe
  $ echo $(./demo013_foldfoldr.exe)
  6
  

  $ ./llvm_runner.exe < manytests/typed/015tuples.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo015_tuples.exe
  $ echo $(./demo015_tuples.exe)
  1
  1
  1
  1
  

  $ ./llvm_runner.exe < manytests/typed/016lists.ml
  $ clang-16 libMLstd.a test.ll -lffi -Wno-override-module -o demo016_lists.exe
  $ echo $(./demo016_lists.exe)
  1
  2
  3
  8
  
