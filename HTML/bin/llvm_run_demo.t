  $ clang-16 -c runtime.c -o runtime.o

  $ ./llvm_demo.exe < manytests/typed/001fac.ml
  ---ANF---
  
  let rec fac n = let app_0 = (n <= 1) in
  let if_1 = if app_0 then 1 else let app_2 = (n - 1) in
  let app_3 = fac app_2 in
  let app_4 = (n * app_3) in
  app_4 in
  if_1;;
  let main  = let app_0 = fac 4 in
  let app_1 = print_int app_0 in
  let EVALUATED_0 = app_1 in
  let app_2 = (EVALUATED_0 ( = ) ()) in
  let if_3 = if app_2 then () else let app_4 = RTE_ERROR_MATCH_FAILURE () in
  app_4 in
  let _ = if_3 in
  0
  
  ---Alpha conv.---
  
  let rec fac.1 n = let app_0.l1 = (n <= 1) in
  let if_1.l2 = if app_0.l1 then 1 else let app_2.l2 = (n - 1) in
  let app_3.l4 = fac.1 app_2.l2 in
  let app_4.l8 = (n * app_3.l4) in
  app_4.l8 in
  if_1.l2;;
  let main.1  = let app_0.l3 = fac.1 4 in
  let app_1.l4 = print_int app_0.l3 in
  let EVALUATED_0.l8 = app_1.l4 in
  let app_2.l18 = (EVALUATED_0.l8 ( = ) ()) in
  let if_3.l32 = if app_2.l18 then () else let app_4.l40 = RTE_ERROR_MATCH_FAILURE () in
  app_4.l40 in
  let _.l0 = if_3.l32 in
  0
  --LLVM--
  $ clang-16 out.ll runtime.o -lffi -o demo
  /usr/bin/ld: /tmp/build_ed4f8f_dune/out-c2d0ff.o: in function `main.1':
  HTML:(.text+0x138): undefined reference to `( = )'
  /usr/bin/ld: HTML:(.text+0x189): undefined reference to `RTE_ERROR_MATCH_FAILURE'
  clang: error: linker command failed with exit code 1 (use -v to see invocation)
  [1]
  $ echo $(./demo)
  ./demo: not found
  
