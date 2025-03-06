  $ clang-16 -c runtime.c -o runtime.o

  $ ./test_llvm.sh manytests/typed/001fac.ml
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
  llvm_demo.exe
  manytests
  out.ll
  runtime.c
  runtime.h
  runtime.o
  test_llvm.sh
  target triple = "x86_64-pc-linux-gnu"
  
  declare i64 @print_int(i64)
  
  declare i64 @print_newline(i64)
  
  declare i64 @"base +"(i64)
  
  declare i64 @"base -"(i64)
  
  declare i64 @"=="(i64, i64)
  
  declare i64 @RTE_ERROR_MATCH_FAILURE(i64)
  
  declare i64 @"( = )"(i64, i64)
  
  declare i64 @"( != )"(i64, i64)
  
  declare i64 @"( && )"(i64, i64)
  
  declare i64 @create_closure(i64, i64, i64)
  
  declare i64 @apply_args_to_closure(i64, i64, i64)
  
  define i64 @fac.1(i64 %n) {
  entry:
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 8
    %n2 = load i64, ptr %n1, align 8
    %leq = icmp sle i64 %n2, 1
    %app_0.l1 = alloca i64, align 8
    store i1 %leq, ptr %app_0.l1, align 1
    %app_0.l13 = load i64, ptr %app_0.l1, align 8
    %ifcondition = icmp ne i64 %app_0.l13, 0
    br i1 %ifcondition, label %then_br, label %else_br
  
  then_br:                                          ; preds = %entry
    br label %ifcontext
  
  else_br:                                          ; preds = %entry
    %n4 = load i64, ptr %n1, align 8
    %sub = sub i64 %n4, 1
    %app_2.l2 = alloca i64, align 8
    store i64 %sub, ptr %app_2.l2, align 8
    %created_empty_closure = call i64 @create_closure(i64 ptrtoint (ptr @fac.1 to i64), i64 1, i64 0)
    %app_2.l25 = load i64, ptr %app_2.l2, align 8
    %application_result = call i64 @apply_args_to_closure(i64 %created_empty_closure, i64 1, i64 %app_2.l25)
    %app_3.l4 = alloca i64, align 8
    store i64 %application_result, ptr %app_3.l4, align 8
    %n6 = load i64, ptr %n1, align 8
    %app_3.l47 = load i64, ptr %app_3.l4, align 8
    %mul = mul i64 %n6, %app_3.l47
    %app_4.l8 = alloca i64, align 8
    store i64 %mul, ptr %app_4.l8, align 8
    %app_4.l88 = load i64, ptr %app_4.l8, align 8
    br label %ifcontext
  
  ifcontext:                                        ; preds = %else_br, %then_br
    %ifphi = phi i64 [ 1, %then_br ], [ %app_4.l88, %else_br ]
    %if_1.l2 = alloca i64, align 8
    store i64 %ifphi, ptr %if_1.l2, align 8
    %if_1.l29 = load i64, ptr %if_1.l2, align 8
    ret i64 %if_1.l29
  }
  
  define i64 @main.1() {
  entry:
    %created_empty_closure = call i64 @create_closure(i64 ptrtoint (ptr @fac.1 to i64), i64 1, i64 0)
    %application_result = call i64 @apply_args_to_closure(i64 %created_empty_closure, i64 1, i64 4)
    %app_0.l3 = alloca i64, align 8
    store i64 %application_result, ptr %app_0.l3, align 8
    %created_empty_closure1 = call i64 @create_closure(i64 ptrtoint (ptr @print_int to i64), i64 1, i64 0)
    %app_0.l32 = load i64, ptr %app_0.l3, align 8
    %application_result3 = call i64 @apply_args_to_closure(i64 %created_empty_closure1, i64 1, i64 %app_0.l32)
    %app_1.l4 = alloca i64, align 8
    store i64 %application_result3, ptr %app_1.l4, align 8
    %app_1.l44 = load i64, ptr %app_1.l4, align 8
    %EVALUATED_0.l8 = alloca i64, align 8
    store i64 %app_1.l44, ptr %EVALUATED_0.l8, align 8
    %created_empty_closure5 = call i64 @create_closure(i64 ptrtoint (ptr @"( = )" to i64), i64 2, i64 0)
    %EVALUATED_0.l86 = load i64, ptr %EVALUATED_0.l8, align 8
    %application_result7 = call i64 @apply_args_to_closure(i64 %created_empty_closure5, i64 1, i64 %EVALUATED_0.l86)
    %application_result8 = call i64 @apply_args_to_closure(i64 %application_result7, i64 1, i64 0)
    %app_2.l18 = alloca i64, align 8
    store i64 %application_result8, ptr %app_2.l18, align 8
    %app_2.l189 = load i64, ptr %app_2.l18, align 8
    %ifcondition = icmp ne i64 %app_2.l189, 0
    br i1 %ifcondition, label %then_br, label %else_br
  
  then_br:                                          ; preds = %entry
    br label %ifcontext
  
  else_br:                                          ; preds = %entry
    %created_empty_closure10 = call i64 @create_closure(i64 ptrtoint (ptr @RTE_ERROR_MATCH_FAILURE to i64), i64 1, i64 0)
    %application_result11 = call i64 @apply_args_to_closure(i64 %created_empty_closure10, i64 1, i64 0)
    %app_4.l40 = alloca i64, align 8
    store i64 %application_result11, ptr %app_4.l40, align 8
    %app_4.l4012 = load i64, ptr %app_4.l40, align 8
    br label %ifcontext
  
  ifcontext:                                        ; preds = %else_br, %then_br
    %ifphi = phi i64 [ 0, %then_br ], [ %app_4.l4012, %else_br ]
    %if_3.l32 = alloca i64, align 8
    store i64 %ifphi, ptr %if_3.l32, align 8
    %if_3.l3213 = load i64, ptr %if_3.l32, align 8
    %_.l0 = alloca i64, align 8
    store i64 %if_3.l3213, ptr %_.l0, align 8
    ret i64 0
  }
