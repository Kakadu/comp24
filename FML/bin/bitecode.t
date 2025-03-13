  $ ./compiler.exe < manytests/typed/001fac.ml
  $  cat < out.ll
  ; ModuleID = 'FML'
  source_filename = "FML"
  target triple = "x86_64-pc-linux-gnu"
  
  declare i64 @create_closure(i64, i64, i64)
  
  declare i64 @apply_args(i64, i64, i64, ...)
  
  declare i64 @print_int(i64)
  
  declare i64 @rt_add(i64, i64)
  
  declare i64 @rt_sub(i64, i64)
  
  declare i64 @rt_mul(i64, i64)
  
  declare i64 @rt_div(i64, i64)
  
  declare i64 @rt_leq(i64, i64)
  
  declare i64 @rt_less(i64, i64)
  
  declare i64 @rt_geq(i64, i64)
  
  declare i64 @rt_gre(i64, i64)
  
  declare i64 @rt_eq(i64, i64)
  
  declare i64 @rt_neq(i64, i64)
  
  declare i64 @rt_and(i64, i64)
  
  declare i64 @rt_or(i64, i64)
  
  declare i64 @fail_match(i64)
  
  define i64 @fac(i64 %n) {
  entry:
    %sle = icmp sle i64 %n, 1
    %sle_i64t = zext i1 %sle to i64
    %cond_v = icmp ne i64 %sle_i64t, 0
    br i1 %cond_v, label %then, label %else
  
  then:                                             ; preds = %entry
    br label %merge
  
  else:                                             ; preds = %entry
    %sub = sub i64 %n, 1
    %call = call i64 @fac(i64 %sub)
    %mul = mul i64 %n, %call
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %phi = phi i64 [ 1, %then ], [ %mul, %else ]
    ret i64 %phi
  }
  
  define i64 @main() {
  entry:
    %call = call i64 @fac(i64 4)
    %call1 = call i64 @print_int(i64 %call)
    ret i64 0
  }
 
  $ ./compiler.exe < manytests/typed/002fac.ml
  $  cat < out.ll
  ; ModuleID = 'FML'
  source_filename = "FML"
  target triple = "x86_64-pc-linux-gnu"
  
  declare i64 @create_closure(i64, i64, i64)
  
  declare i64 @apply_args(i64, i64, i64, ...)
  
  declare i64 @print_int(i64)
  
  declare i64 @rt_add(i64, i64)
  
  declare i64 @rt_sub(i64, i64)
  
  declare i64 @rt_mul(i64, i64)
  
  declare i64 @rt_div(i64, i64)
  
  declare i64 @rt_leq(i64, i64)
  
  declare i64 @rt_less(i64, i64)
  
  declare i64 @rt_geq(i64, i64)
  
  declare i64 @rt_gre(i64, i64)
  
  declare i64 @rt_eq(i64, i64)
  
  declare i64 @rt_neq(i64, i64)
  
  declare i64 @rt_and(i64, i64)
  
  declare i64 @rt_or(i64, i64)
  
  declare i64 @fail_match(i64)
  
  define i64 @a1(i64 %k, i64 %n, i64 %p) {
  entry:
    %mul = mul i64 %p, %n
    %applied_closure = call i64 (i64, i64, ...) @apply_args(i64 %k, i64 1, i64 %mul)
    ret i64 %applied_closure
  }
  
  define i64 @fac_cps(i64 %n, i64 %k) {
  entry:
    %eq = icmp eq i64 %n, 1
    %eq_i64t = zext i1 %eq to i64
    %cond_v = icmp ne i64 %eq_i64t, 0
    br i1 %cond_v, label %then, label %else
  
  then:                                             ; preds = %entry
    %applied_closure = call i64 (i64, i64, ...) @apply_args(i64 %k, i64 1, i64 1)
    br label %merge
  
  else:                                             ; preds = %entry
    %empty_closure = call i64 @create_closure(i64 ptrtoint (ptr @a1 to i64), i64 3, i64 0)
    %applied_closure1 = call i64 (i64, i64, ...) @apply_args(i64 %empty_closure, i64 2, i64 %k, i64 %n)
    %sub = sub i64 %n, 1
    %call = call i64 @fac_cps(i64 %sub, i64 %applied_closure1)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %phi = phi i64 [ %applied_closure, %then ], [ %call, %else ]
    ret i64 %phi
  }
  
  define i64 @a2(i64 %a0) {
  entry:
    ret i64 %a0
  }
  
  define i64 @main() {
  entry:
    %empty_closure = call i64 @create_closure(i64 ptrtoint (ptr @a2 to i64), i64 1, i64 0)
    %call = call i64 @fac_cps(i64 4, i64 %empty_closure)
    %call1 = call i64 @print_int(i64 %call)
    ret i64 0
  }
