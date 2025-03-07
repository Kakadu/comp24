  $ clang-16 -c runtime.c -o runtime.o

  $ ./llvm_demo.exe < test.ml
  ---ANF---
  
  let cc_ll_0 n k p = let app_0 = (p * n) in
  let app_1 = k app_0 in
  app_1;;
  let rec fac_cps n k = let app_0 = (n = 1) in
  let if_1 = if app_0 then let app_2 = k 1 in
  app_2 else let app_3 = (n - 1) in
  let app_4 = cc_ll_0 n k in
  let app_5 = fac_cps app_3 app_4 in
  app_5 in
  if_1;;
  let cc_ll_1 print_int = print_int;;
  let main  = let app_0 = fac_cps 4 cc_ll_1 in
  let app_1 = print_int app_0 in
  let EVALUATED_0 = app_1 in
  let app_2 = (EVALUATED_0 ( = ) ()) in
  let if_3 = if app_2 then () else let app_4 = RTE_ERROR_MATCH_FAILURE () in
  app_4 in
  let _ = if_3 in
  0
  
  ---Alpha conv.---
  
  let cc_ll_0.1 n k p = let app_0.l1 = (p * n) in
  let app_1.l2 = k app_0.l1 in
  app_1.l2;;
  let rec fac_cps.1 n k = let app_0.l4 = (n = 1) in
  let if_1.l4 = if app_0.l4 then let app_2.l12 = k 1 in
  app_2.l12 else let app_3.l4 = (n - 1) in
  let app_4.l24 = cc_ll_0.1 n k in
  let app_5.l16 = fac_cps.1 app_3.l4 app_4.l24 in
  app_5.l16 in
  if_1.l4;;
  let cc_ll_1.1 print_int = print_int;;
  let main  = let app_0.l7 = fac_cps.1 4 cc_ll_1.1 in
  let app_1.l10 = print_int app_0.l7 in
  let EVALUATED_0.l16 = app_1.l10 in
  let app_2.l36 = (EVALUATED_0.l16 ( = ) ()) in
  let if_3.l64 = if app_2.l36 then () else let app_4.l72 = RTE_ERROR_MATCH_FAILURE () in
  app_4.l72 in
  let _.l0 = if_3.l64 in
  0
  --LLVM--
  $ clang-16 out.ll runtime.o -lffi -o demo
  $ echo $(./demo)
  24

