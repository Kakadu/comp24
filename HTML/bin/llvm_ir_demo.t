  $ clang-16 -c runtime.c -o runtime.o

  $ ./llvm_demo.exe < manytests/typed/001fac.ml
  $ cat out.ll | grep -E 'source_filename|target datalayout|target triple|ModuleID' --invert-match
  
  declare i64 @create_closure(i64, i64, i64)
  
  declare i64 @apply_args_to_closure(i64, i64, i64)
  
  declare i64 @print_int(i64)
  
  declare i64 @print_bool(i64)
  
  declare i64 @print_newline(i64)
  
  declare i64 @uplus(i64)
  
  declare i64 @uminus(i64)
  
  declare i64 @leq(i64, i64)
  
  declare i64 @less(i64, i64)
  
  declare i64 @geq(i64, i64)
  
  declare i64 @gre(i64, i64)
  
  declare i64 @eq(i64, i64)
  
  declare i64 @neq(i64, i64)
  
  declare i64 @and(i64, i64)
  
  declare i64 @or(i64, i64)
  
  declare i64 @RTE_ERROR_MATCH_FAILURE(i64)
  
  define i64 @fac.1(i64 %n) {
  entry:
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 8
    %created_empty_closure = call i64 @create_closure(i64 ptrtoint (ptr @leq to i64), i64 2, i64 0)
    %n2 = load i64, ptr %n1, align 8
    %application_result = call i64 @apply_args_to_closure(i64 %created_empty_closure, i64 1, i64 %n2)
    %application_result3 = call i64 @apply_args_to_closure(i64 %application_result, i64 1, i64 1)
    %app_0.l1 = alloca i64, align 8
    store i64 %application_result3, ptr %app_0.l1, align 8
    %app_0.l14 = load i64, ptr %app_0.l1, align 8
    %ifcondition = icmp ne i64 %app_0.l14, 0
    br i1 %ifcondition, label %then_br, label %else_br
  
  then_br:                                          ; preds = %entry
    br label %ifcontext
  
  else_br:                                          ; preds = %entry
    %n5 = load i64, ptr %n1, align 8
    %sub = sub i64 %n5, 1
    %app_2.l2 = alloca i64, align 8
    store i64 %sub, ptr %app_2.l2, align 8
    %created_empty_closure6 = call i64 @create_closure(i64 ptrtoint (ptr @fac.1 to i64), i64 1, i64 0)
    %app_2.l27 = load i64, ptr %app_2.l2, align 8
    %application_result8 = call i64 @apply_args_to_closure(i64 %created_empty_closure6, i64 1, i64 %app_2.l27)
    %app_3.l4 = alloca i64, align 8
    store i64 %application_result8, ptr %app_3.l4, align 8
    %n9 = load i64, ptr %n1, align 8
    %app_3.l410 = load i64, ptr %app_3.l4, align 8
    %mul = mul i64 %n9, %app_3.l410
    %app_4.l8 = alloca i64, align 8
    store i64 %mul, ptr %app_4.l8, align 8
    %app_4.l811 = load i64, ptr %app_4.l8, align 8
    br label %ifcontext
  
  ifcontext:                                        ; preds = %else_br, %then_br
    %ifphi = phi i64 [ 1, %then_br ], [ %app_4.l811, %else_br ]
    %if_1.l2 = alloca i64, align 8
    store i64 %ifphi, ptr %if_1.l2, align 8
    %if_1.l212 = load i64, ptr %if_1.l2, align 8
    ret i64 %if_1.l212
  }
  
  define i64 @main() {
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
    %created_empty_closure5 = call i64 @create_closure(i64 ptrtoint (ptr @eq to i64), i64 2, i64 0)
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
