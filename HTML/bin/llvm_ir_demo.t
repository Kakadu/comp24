  $ clang-16 -c runtime.c -o runtime.o

  $ ./test_llvm.sh manytests/typed/001fac.ml
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
