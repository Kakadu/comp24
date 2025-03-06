  $ clang-16 -c runtime.c -o runtime.o

  $ ./llvm_demo.exe < manytests/typed/001fac.ml
  ---ANF---
  
  let rec fac n = let app_0 = (n <= 1) in
  let if_1 = if app_0 then 1 else let app_2 = (n - 1) in
  let app_3 = fac app_2 in
  let app_4 = (n * app_3) in
  app_4 in
  if_1
  
  ---Alpha conv.---
  
  let rec fac.1 n = let app_0.l1 = (n <= 1) in
  let if_1.l2 = if app_0.l1 then 1 else let app_2.l2 = (n - 1) in
  let app_3.l4 = fac.1 app_2.l2 in
  let app_4.l8 = (n * app_3.l4) in
  app_4.l8 in
  if_1.l2
  --LLVM--
  $ clang-16 out.ll runtime.o -lffi -o demo
  $ echo $(./demo)
  
