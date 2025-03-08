  $ clang-16 -c runtime.c -o runtime.o

  $ ./llvm_demo.exe < manytests/typed/001fac.ml
  $ cat out.ll | grep -E 'source_filename|target datalayout|target triple|ModuleID' --invert-match
  
  @main_1 = global i64 0
  
  define i64 @main() {
  entry:
    %created_empty_closure = call i64 @create_closure(i64 ptrtoint (ptr @fac_1 to i64), i64 1, i64 0)
    %application_result = call i64 (i64, i64, ...) @apply_args_to_closure(i64 %created_empty_closure, i64 1, i64 4)
    %app_0 = alloca i64, align 8
    store i64 %application_result, ptr %app_0, align 8
    %created_empty_closure1 = call i64 @create_closure(i64 ptrtoint (ptr @print_int to i64), i64 1, i64 0)
    %app_02 = load i64, ptr %app_0, align 8
    %application_result3 = call i64 (i64, i64, ...) @apply_args_to_closure(i64 %created_empty_closure1, i64 1, i64 %app_02)
    %app_1 = alloca i64, align 8
    store i64 %application_result3, ptr %app_1, align 8
    %app_14 = load i64, ptr %app_1, align 8
    %evaluated_0 = alloca i64, align 8
    store i64 %app_14, ptr %evaluated_0, align 8
    %created_empty_closure5 = call i64 @create_closure(i64 ptrtoint (ptr @eq to i64), i64 2, i64 0)
    %evaluated_06 = load i64, ptr %evaluated_0, align 8
    %application_result7 = call i64 (i64, i64, ...) @apply_args_to_closure(i64 %created_empty_closure5, i64 2, i64 %evaluated_06, i64 0)
    %app_2 = alloca i64, align 8
    store i64 %application_result7, ptr %app_2, align 8
    %app_28 = load i64, ptr %app_2, align 8
    %ifcondition = icmp ne i64 %app_28, 0
    br i1 %ifcondition, label %then_br, label %else_br
  
  then_br:                                          ; preds = %entry
    br label %ifcontext
  
  else_br:                                          ; preds = %entry
    %created_empty_closure9 = call i64 @create_closure(i64 ptrtoint (ptr @rte_error_match_failure to i64), i64 1, i64 0)
    %application_result10 = call i64 (i64, i64, ...) @apply_args_to_closure(i64 %created_empty_closure9, i64 1, i64 0)
    %app_4 = alloca i64, align 8
    store i64 %application_result10, ptr %app_4, align 8
    %app_411 = load i64, ptr %app_4, align 8
    br label %ifcontext
  
  ifcontext:                                        ; preds = %else_br, %then_br
    %ifphi = phi i64 [ 0, %then_br ], [ %app_411, %else_br ]
    %if_3 = alloca i64, align 8
    store i64 %ifphi, ptr %if_3, align 8
    %if_312 = load i64, ptr %if_3, align 8
    %_ = alloca i64, align 8
    store i64 %if_312, ptr %_, align 8
    store i64 0, ptr @main_1, align 8
    ret i64 0
  }
  
  declare i64 @create_closure(i64, i64, i64)
  
  declare i64 @apply_args_to_closure(i64, i64, i64)
  
  declare i64 @print_int(i64)
  
  declare i64 @print_bool(i64)
  
  declare i64 @print_newline(i64)
  
  declare i64 @uplus(i64)
  
  declare i64 @uminus(i64)
  
  declare i64 @add(i64, i64)
  
  declare i64 @sub(i64, i64)
  
  declare i64 @mul(i64, i64)
  
  declare i64 @divd(i64, i64)
  
  declare i64 @leq(i64, i64)
  
  declare i64 @less(i64, i64)
  
  declare i64 @geq(i64, i64)
  
  declare i64 @gre(i64, i64)
  
  declare i64 @eq(i64, i64)
  
  declare i64 @neq(i64, i64)
  
  declare i64 @and(i64, i64)
  
  declare i64 @or(i64, i64)
  
  declare i64 @rte_error_match_failure(i64)
  
  define i64 @fac_1(i64 %n) {
  entry:
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 8
    %created_empty_closure = call i64 @create_closure(i64 ptrtoint (ptr @leq to i64), i64 2, i64 0)
    %n2 = load i64, ptr %n1, align 8
    %application_result = call i64 (i64, i64, ...) @apply_args_to_closure(i64 %created_empty_closure, i64 2, i64 %n2, i64 1)
    %app_0 = alloca i64, align 8
    store i64 %application_result, ptr %app_0, align 8
    %app_03 = load i64, ptr %app_0, align 8
    %ifcondition = icmp ne i64 %app_03, 0
    br i1 %ifcondition, label %then_br, label %else_br
  
  then_br:                                          ; preds = %entry
    br label %ifcontext
  
  else_br:                                          ; preds = %entry
    %n4 = load i64, ptr %n1, align 8
    %sub = sub i64 %n4, 1
    %app_2 = alloca i64, align 8
    store i64 %sub, ptr %app_2, align 8
    %created_empty_closure5 = call i64 @create_closure(i64 ptrtoint (ptr @fac_1 to i64), i64 1, i64 0)
    %app_26 = load i64, ptr %app_2, align 8
    %application_result7 = call i64 (i64, i64, ...) @apply_args_to_closure(i64 %created_empty_closure5, i64 1, i64 %app_26)
    %app_3 = alloca i64, align 8
    store i64 %application_result7, ptr %app_3, align 8
    %n8 = load i64, ptr %n1, align 8
    %app_39 = load i64, ptr %app_3, align 8
    %mul = mul i64 %n8, %app_39
    %app_4 = alloca i64, align 8
    store i64 %mul, ptr %app_4, align 8
    %app_410 = load i64, ptr %app_4, align 8
    br label %ifcontext
  
  ifcontext:                                        ; preds = %else_br, %then_br
    %ifphi = phi i64 [ 1, %then_br ], [ %app_410, %else_br ]
    %if_1 = alloca i64, align 8
    store i64 %ifphi, ptr %if_1, align 8
    %if_111 = load i64, ptr %if_1, align 8
    ret i64 %if_111
  }
