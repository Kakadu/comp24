  $ clang-16 -c runtime.c -o runtime.o

  $ llvm_codegen < manytests/typed/001fac.ml
  $ cat out.ll | grep -E 'source_filename|target datalayout|target triple|ModuleID' --invert-match
  
  @main.1 = global i64 0
  
  define i64 @main() {
  entry:
    %empty_closure = call i64 @create_closure(i64 ptrtoint (ptr @fac to i64), i64 1, i64 0)
    %applied_closure = call i64 (i64, i64, ...) @apply_args_to_closure(i64 %empty_closure, i64 1, i64 4)
    %empty_closure1 = call i64 @create_closure(i64 ptrtoint (ptr @print_int to i64), i64 1, i64 0)
    %applied_closure2 = call i64 (i64, i64, ...) @apply_args_to_closure(i64 %empty_closure1, i64 1, i64 %applied_closure)
    store i64 0, ptr @main.1, align 4
    ret i64 0
  }
  
  declare i64 @create_closure(i64, i64, i64)
  
  declare i64 @apply_args_to_closure(i64, i64, i64, ...)
  
  declare i64 @print_int(i64)
  
  declare i64 @print_bool(i64)
  
  declare i64 @add(i64, i64)
  
  declare i64 @sub(i64, i64)
  
  declare i64 @mul(i64, i64)
  
  declare i64 @div(i64, i64)
  
  declare i64 @leq(i64, i64)
  
  declare i64 @less(i64, i64)
  
  declare i64 @geq(i64, i64)
  
  declare i64 @gre(i64, i64)
  
  declare i64 @eq(i64, i64)
  
  declare i64 @neq(i64, i64)
  
  declare i64 @and(i64, i64)
  
  declare i64 @or(i64, i64)
  
  declare i64 @fail_match(i64)
  
  define i64 @fac(i64 %n) {
  entry:
    %empty_closure = call i64 @create_closure(i64 ptrtoint (ptr @leq to i64), i64 2, i64 0)
    %applied_closure = call i64 (i64, i64, ...) @apply_args_to_closure(i64 %empty_closure, i64 2, i64 %n, i64 1)
    %cond_val = icmp ne i64 %applied_closure, 0
    br i1 %cond_val, label %then, label %else
  
  then:                                             ; preds = %entry
    br label %merge
  
  else:                                             ; preds = %entry
    %sub = sub i64 %n, 1
    %empty_closure1 = call i64 @create_closure(i64 ptrtoint (ptr @fac to i64), i64 1, i64 0)
    %applied_closure2 = call i64 (i64, i64, ...) @apply_args_to_closure(i64 %empty_closure1, i64 1, i64 %sub)
    %mul = mul i64 %n, %applied_closure2
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %phi = phi i64 [ 1, %then ], [ %mul, %else ]
    ret i64 %phi
  }
