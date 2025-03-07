  $ clang-16 -c runtime.c -o runtime.o

  $ ./test_llvm.sh test.ml
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
  llvm_demo.exe
  manytests
  out.ll
  runtime.c
  runtime.h
  runtime.o
  test.ml
  test_llvm.sh
  target triple = "x86_64-pc-linux-gnu"
  
  declare i64 @print_int(i64)
  
  declare i64 @print_bool(i64)
  
  declare i64 @print_newline(i64)
  
  declare i64 @"base +"(i64)
  
  declare i64 @"base -"(i64)
  
  declare i64 @"<="(i64, i64)
  
  declare i64 @"<"(i64, i64)
  
  declare i64 @">="(i64, i64)
  
  declare i64 @">"(i64, i64)
  
  declare i64 @"="(i64, i64)
  
  declare i64 @"!="(i64, i64)
  
  declare i64 @"&&"(i64, i64)
  
  declare i64 @"||"(i64, i64)
  
  declare i64 @RTE_ERROR_MATCH_FAILURE(i64)
  
  declare i64 @"( = )"(i64, i64)
  
  declare i64 @"( != )"(i64, i64)
  
  declare i64 @"( && )"(i64, i64)
  
  declare i64 @leq(i64, i64)
  
  declare i64 @eq(i64, i64)
  
  declare i64 @create_closure(i64, i64, i64)
  
  declare i64 @apply_args_to_closure(i64, i64, i64)
  
  define i64 @cc_ll_0.1(i64 %n, i64 %k, i64 %p) {
  entry:
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 8
    %k2 = alloca i64, align 8
    store i64 %k, ptr %k2, align 8
    %p3 = alloca i64, align 8
    store i64 %p, ptr %p3, align 8
    %p4 = load i64, ptr %p3, align 8
    %n5 = load i64, ptr %n1, align 8
    %mul = mul i64 %p4, %n5
    %app_0.l1 = alloca i64, align 8
    store i64 %mul, ptr %app_0.l1, align 8
    %k6 = load i64, ptr %k2, align 8
    %app_0.l17 = load i64, ptr %app_0.l1, align 8
    %application_result = call i64 @apply_args_to_closure(i64 %k6, i64 1, i64 %app_0.l17)
    %app_1.l2 = alloca i64, align 8
    store i64 %application_result, ptr %app_1.l2, align 8
    %app_1.l28 = load i64, ptr %app_1.l2, align 8
    ret i64 %app_1.l28
  }
  
  define i64 @fac_cps.1(i64 %n, i64 %k) {
  entry:
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 8
    %k2 = alloca i64, align 8
    store i64 %k, ptr %k2, align 8
    %created_empty_closure = call i64 @create_closure(i64 ptrtoint (ptr @eq to i64), i64 2, i64 0)
    %n3 = load i64, ptr %n1, align 8
    %application_result = call i64 @apply_args_to_closure(i64 %created_empty_closure, i64 1, i64 %n3)
    %application_result4 = call i64 @apply_args_to_closure(i64 %application_result, i64 1, i64 1)
    %app_0.l4 = alloca i64, align 8
    store i64 %application_result4, ptr %app_0.l4, align 8
    %app_0.l45 = load i64, ptr %app_0.l4, align 8
    %ifcondition = icmp ne i64 %app_0.l45, 0
    br i1 %ifcondition, label %then_br, label %else_br
  
  then_br:                                          ; preds = %entry
    %k6 = load i64, ptr %k2, align 8
    %application_result7 = call i64 @apply_args_to_closure(i64 %k6, i64 1, i64 1)
    %app_2.l12 = alloca i64, align 8
    store i64 %application_result7, ptr %app_2.l12, align 8
    %app_2.l128 = load i64, ptr %app_2.l12, align 8
    br label %ifcontext
  
  else_br:                                          ; preds = %entry
    %n9 = load i64, ptr %n1, align 8
    %sub = sub i64 %n9, 1
    %app_3.l4 = alloca i64, align 8
    store i64 %sub, ptr %app_3.l4, align 8
    %created_empty_closure10 = call i64 @create_closure(i64 ptrtoint (ptr @cc_ll_0.1 to i64), i64 3, i64 0)
    %n11 = load i64, ptr %n1, align 8
    %application_result12 = call i64 @apply_args_to_closure(i64 %created_empty_closure10, i64 1, i64 %n11)
    %k13 = load i64, ptr %k2, align 8
    %application_result14 = call i64 @apply_args_to_closure(i64 %application_result12, i64 1, i64 %k13)
    %app_4.l24 = alloca i64, align 8
    store i64 %application_result14, ptr %app_4.l24, align 8
    %created_empty_closure15 = call i64 @create_closure(i64 ptrtoint (ptr @fac_cps.1 to i64), i64 2, i64 0)
    %app_3.l416 = load i64, ptr %app_3.l4, align 8
    %application_result17 = call i64 @apply_args_to_closure(i64 %created_empty_closure15, i64 1, i64 %app_3.l416)
    %app_4.l2418 = load i64, ptr %app_4.l24, align 8
    %application_result19 = call i64 @apply_args_to_closure(i64 %application_result17, i64 1, i64 %app_4.l2418)
    %app_5.l16 = alloca i64, align 8
    store i64 %application_result19, ptr %app_5.l16, align 8
    %app_5.l1620 = load i64, ptr %app_5.l16, align 8
    br label %ifcontext
  
  ifcontext:                                        ; preds = %else_br, %then_br
    %ifphi = phi i64 [ %app_2.l128, %then_br ], [ %app_5.l1620, %else_br ]
    %if_1.l4 = alloca i64, align 8
    store i64 %ifphi, ptr %if_1.l4, align 8
    %if_1.l421 = load i64, ptr %if_1.l4, align 8
    ret i64 %if_1.l421
  }
  
  define i64 @cc_ll_1.1(i64 %print_int) {
  entry:
    %print_int1 = alloca i64, align 8
    store i64 %print_int, ptr %print_int1, align 8
    %print_int2 = load i64, ptr %print_int1, align 8
    ret i64 %print_int2
  }
  
  define i64 @main() {
  entry:
    %created_empty_closure = call i64 @create_closure(i64 ptrtoint (ptr @fac_cps.1 to i64), i64 2, i64 0)
    %application_result = call i64 @apply_args_to_closure(i64 %created_empty_closure, i64 1, i64 4)
    %created_empty_closure1 = call i64 @create_closure(i64 ptrtoint (ptr @cc_ll_1.1 to i64), i64 1, i64 0)
    %application_result2 = call i64 @apply_args_to_closure(i64 %application_result, i64 1, i64 %created_empty_closure1)
    %app_0.l7 = alloca i64, align 8
    store i64 %application_result2, ptr %app_0.l7, align 8
    %created_empty_closure3 = call i64 @create_closure(i64 ptrtoint (ptr @print_int to i64), i64 1, i64 0)
    %app_0.l74 = load i64, ptr %app_0.l7, align 8
    %application_result5 = call i64 @apply_args_to_closure(i64 %created_empty_closure3, i64 1, i64 %app_0.l74)
    %app_1.l10 = alloca i64, align 8
    store i64 %application_result5, ptr %app_1.l10, align 8
    %app_1.l106 = load i64, ptr %app_1.l10, align 8
    %EVALUATED_0.l16 = alloca i64, align 8
    store i64 %app_1.l106, ptr %EVALUATED_0.l16, align 8
    %created_empty_closure7 = call i64 @create_closure(i64 ptrtoint (ptr @eq to i64), i64 2, i64 0)
    %EVALUATED_0.l168 = load i64, ptr %EVALUATED_0.l16, align 8
    %application_result9 = call i64 @apply_args_to_closure(i64 %created_empty_closure7, i64 1, i64 %EVALUATED_0.l168)
    %application_result10 = call i64 @apply_args_to_closure(i64 %application_result9, i64 1, i64 0)
    %app_2.l36 = alloca i64, align 8
    store i64 %application_result10, ptr %app_2.l36, align 8
    %app_2.l3611 = load i64, ptr %app_2.l36, align 8
    %ifcondition = icmp ne i64 %app_2.l3611, 0
    br i1 %ifcondition, label %then_br, label %else_br
  
  then_br:                                          ; preds = %entry
    br label %ifcontext
  
  else_br:                                          ; preds = %entry
    %created_empty_closure12 = call i64 @create_closure(i64 ptrtoint (ptr @RTE_ERROR_MATCH_FAILURE to i64), i64 1, i64 0)
    %application_result13 = call i64 @apply_args_to_closure(i64 %created_empty_closure12, i64 1, i64 0)
    %app_4.l72 = alloca i64, align 8
    store i64 %application_result13, ptr %app_4.l72, align 8
    %app_4.l7214 = load i64, ptr %app_4.l72, align 8
    br label %ifcontext
  
  ifcontext:                                        ; preds = %else_br, %then_br
    %ifphi = phi i64 [ 0, %then_br ], [ %app_4.l7214, %else_br ]
    %if_3.l64 = alloca i64, align 8
    store i64 %ifphi, ptr %if_3.l64, align 8
    %if_3.l6415 = load i64, ptr %if_3.l64, align 8
    %_.l0 = alloca i64, align 8
    store i64 %if_3.l6415, ptr %_.l0, align 8
    ret i64 0
  }
