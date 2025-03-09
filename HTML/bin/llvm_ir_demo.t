  $ clang-16 -c runtime.c -o runtime.o

  $ ./llvm_demo.exe < manytests/typed/001fac.ml
  $ cat out.ll | grep -E 'source_filename|target datalayout|target triple|ModuleID' --invert-match
  
  @main_1 = global i64 0
  
  define i64 @main() {
  entry:
    %created_empty_closure = call i64 @create_closure(i64 ptrtoint (ptr @fac_1 to i64), i64 1, i64 0)
    %application_result = call i64 (i64, i64, ...) @apply_args_to_closure(i64 %created_empty_closure, i64 1, i64 4)
    %created_empty_closure1 = call i64 @create_closure(i64 ptrtoint (ptr @print_int to i64), i64 1, i64 0)
    %application_result2 = call i64 (i64, i64, ...) @apply_args_to_closure(i64 %created_empty_closure1, i64 1, i64 %application_result)
    %created_empty_closure3 = call i64 @create_closure(i64 ptrtoint (ptr @eq to i64), i64 2, i64 0)
    %application_result4 = call i64 (i64, i64, ...) @apply_args_to_closure(i64 %created_empty_closure3, i64 2, i64 %application_result2, i64 0)
    %ifcondition = icmp ne i64 %application_result4, 0
    br i1 %ifcondition, label %then_br, label %else_br
  
  then_br:                                          ; preds = %entry
    br label %ifcontext
  
  else_br:                                          ; preds = %entry
    %created_empty_closure5 = call i64 @create_closure(i64 ptrtoint (ptr @rte_error_match_failure to i64), i64 1, i64 0)
    %application_result6 = call i64 (i64, i64, ...) @apply_args_to_closure(i64 %created_empty_closure5, i64 1, i64 0)
    br label %ifcontext
  
  ifcontext:                                        ; preds = %else_br, %then_br
    %ifphi = phi i64 [ 0, %then_br ], [ %application_result6, %else_br ]
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
    %created_empty_closure = call i64 @create_closure(i64 ptrtoint (ptr @leq to i64), i64 2, i64 0)
    %application_result = call i64 (i64, i64, ...) @apply_args_to_closure(i64 %created_empty_closure, i64 2, i64 %n, i64 1)
    %ifcondition = icmp ne i64 %application_result, 0
    br i1 %ifcondition, label %then_br, label %else_br
  
  then_br:                                          ; preds = %entry
    br label %ifcontext
  
  else_br:                                          ; preds = %entry
    %sub = sub i64 %n, 1
    %created_empty_closure1 = call i64 @create_closure(i64 ptrtoint (ptr @fac_1 to i64), i64 1, i64 0)
    %application_result2 = call i64 (i64, i64, ...) @apply_args_to_closure(i64 %created_empty_closure1, i64 1, i64 %sub)
    %mul = mul i64 %n, %application_result2
    br label %ifcontext
  
  ifcontext:                                        ; preds = %else_br, %then_br
    %ifphi = phi i64 [ 1, %then_br ], [ %mul, %else_br ]
    ret i64 %ifphi
  }
