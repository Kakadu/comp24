  $ dune exec test_codegen <<- EOF
  > let rec fac_cps n k = if n=1 then k 1 else fac_cps (n-1) (fun p -> k (p*n))
  > let main = let () = print_int (fac_cps 4 (fun print_int -> print_int)) in 0
  > EOF
  ; ModuleID = 'BDSML'
  source_filename = "BDSML"
  target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
  target triple = "x86_64-pc-linux-gnu"
  
  @__var_main = global ptr null
  
  declare ptr @create_int(i64 %0)
  
  declare ptr @create_bool(i1 %0)
  
  declare ptr @create_unit()
  
  declare ptr @create_char(i8 %0)
  
  declare ptr @create_string(i64 %0, ...)
  
  declare ptr @apply(ptr %0, i64 %1, ...)
  
  declare ptr @create_tuple(i64 %0, ...)
  
  declare ptr @create_constructor(i64 %0, ptr %1)
  
  declare ptr @create_function(ptr %0, i64 %1)
  
  declare i64 @get_int(ptr %0)
  
  declare i1 @get_bool(ptr %0)
  
  declare i8 @get_char(ptr %0)
  
  declare ptr @op_plus(ptr %0, ptr %1)
  
  declare ptr @op_minus(ptr %0, ptr %1)
  
  declare ptr @op_mult(ptr %0, ptr %1)
  
  declare ptr @op_div(ptr %0, ptr %1)
  
  declare ptr @op_neg(ptr %0)
  
  declare ptr @op_pos(ptr %0)
  
  declare ptr @op_not(ptr %0)
  
  declare ptr @op_gt(ptr %0, ptr %1)
  
  declare ptr @op_ge(ptr %0, ptr %1)
  
  declare ptr @op_lt(ptr %0, ptr %1)
  
  declare ptr @op_le(ptr %0, ptr %1)
  
  declare ptr @op_eq(ptr %0, ptr %1)
  
  declare ptr @op_neq(ptr %0, ptr %1)
  
  declare ptr @op_or(ptr %0, ptr %1)
  
  declare ptr @op_and(ptr %0, ptr %1)
  
  declare ptr @op_phys_eq(ptr %0, ptr %1)
  
  declare void @print_int(ptr %0)
  
  declare void @print_string(ptr %0)
  
  declare ptr @disassemble(ptr %0)
  
  declare ptr @get_from_tuple(ptr %0, ptr %1)
  
  declare ptr @same_cons(ptr %0, ptr %1)
  
  declare ptr @exception(ptr %0)
  
  define ptr @lifted_1(ptr %__var_k0, ptr %__var_n1, ptr %__reserved_2) {
  entry:
    %boxed_create_function = call ptr @create_function(ptr @op_mult, i64 2)
    %boxed_apply = call ptr (ptr, i64, ...) @apply(ptr %boxed_create_function, i64 2, ptr %__reserved_2, ptr %__var_n1)
    %boxed_apply1 = call ptr (ptr, i64, ...) @apply(ptr %__var_k0, i64 1, ptr %boxed_apply)
    ret ptr %boxed_apply1
  }
  
  define ptr @__var_fac_cps(ptr %__reserved_0, ptr %__reserved_1) {
  entry:
    %boxed_create_function = call ptr @create_function(ptr @op_eq, i64 2)
    %boxed_create_int = call ptr @create_int(i64 1)
    %boxed_apply = call ptr (ptr, i64, ...) @apply(ptr %boxed_create_function, i64 2, ptr %__reserved_0, ptr %boxed_create_int)
    %cond_bool = call i1 @get_bool(ptr %boxed_apply)
    br i1 %cond_bool, label %then, label %else
  
  then:                                             ; preds = %entry
    %boxed_create_int1 = call ptr @create_int(i64 1)
    %boxed_apply2 = call ptr (ptr, i64, ...) @apply(ptr %__reserved_1, i64 1, ptr %boxed_create_int1)
    br label %merge
  
  else:                                             ; preds = %entry
    %boxed_create_function3 = call ptr @create_function(ptr @op_minus, i64 2)
    %boxed_create_int4 = call ptr @create_int(i64 1)
    %boxed_apply5 = call ptr (ptr, i64, ...) @apply(ptr %boxed_create_function3, i64 2, ptr %__reserved_0, ptr %boxed_create_int4)
    %boxed_create_function6 = call ptr @create_function(ptr @lifted_1, i64 3)
    %boxed_apply7 = call ptr (ptr, i64, ...) @apply(ptr %boxed_create_function6, i64 2, ptr %__reserved_1, ptr %__reserved_0)
    %boxed_create_function8 = call ptr @create_function(ptr @__var_fac_cps, i64 2)
    %boxed_apply9 = call ptr (ptr, i64, ...) @apply(ptr %boxed_create_function8, i64 2, ptr %boxed_apply5, ptr %boxed_apply7)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %ite_result = phi ptr [ %boxed_apply2, %then ], [ %boxed_apply9, %else ]
    ret ptr %ite_result
  }
  
  define ptr @lifted_0(ptr %__reserved_3) {
  entry:
    ret ptr %__reserved_3
  }
  
  define i64 @main() {
  entry:
    %boxed_create_function = call ptr @create_function(ptr @__var_fac_cps, i64 2)
    %boxed_create_int = call ptr @create_int(i64 4)
    %boxed_create_function1 = call ptr @create_function(ptr @lifted_0, i64 1)
    %boxed_apply = call ptr (ptr, i64, ...) @apply(ptr %boxed_create_function, i64 2, ptr %boxed_create_int, ptr %boxed_create_function1)
    %boxed_create_function2 = call ptr @create_function(ptr @print_int, i64 1)
    %boxed_apply3 = call ptr (ptr, i64, ...) @apply(ptr %boxed_create_function2, i64 1, ptr %boxed_apply)
    %boxed_create_function4 = call ptr @create_function(ptr @op_eq, i64 2)
    %boxed_create_unit = call ptr @create_unit()
    %boxed_apply5 = call ptr (ptr, i64, ...) @apply(ptr %boxed_create_function4, i64 2, ptr %boxed_apply3, ptr %boxed_create_unit)
    %boxed_create_int6 = call ptr @create_int(i64 0)
    store ptr %boxed_create_int6, ptr @__var_main, align 8
    ret i64 0
  }
 
