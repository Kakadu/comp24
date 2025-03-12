  $ dune exec test_codegen <<- EOF
  > let rec fac n = if n<=1 then 1 else n * fac (n-1)
  > let main =
  > let () = print_int (fac 4) in
  > 0 
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
  
  declare ptr @create_string(...)
  
  declare ptr @apply(ptr %0, ptr %1, ...)
  
  declare ptr @create_tuple(i64 %0, ...)
  
  declare ptr @create_list(ptr %0, ptr %1)
  
  declare ptr @create_empty_list()
  
  declare ptr @create_cons(i1 %0, ptr %1)
  
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
  
  declare ptr @disassemble(ptr %0)
  
  declare ptr @get_from_tuple(ptr %0, ptr %1)
  
  declare ptr @same_cons(ptr %0, ptr %1)
  
  declare ptr @get_cons_param(ptr %0)
  
  declare ptr @exception(ptr %0)
  
  define ptr @__var_fac(ptr %__reserved_0) {
  entry:
    %__reserved_01 = load ptr, ptr %__reserved_0, align 8
    %boxed_create_function = call ptr @create_function(ptr @op_le, i64 2)
    %__var_n = load ptr, ptr %__reserved_01, align 8
    %boxed_create_int = call ptr @create_int(i64 1)
    %boxed_create_int2 = call ptr @create_int(i64 2)
    %boxed_apply = call ptr (ptr, ptr, ...) @apply(ptr %boxed_create_function, ptr %boxed_create_int2, ptr %__var_n, ptr %boxed_create_int)
    %__anf_0 = load ptr, ptr %boxed_apply, align 8
    %cond_bool = call i1 @get_bool(ptr %__anf_0)
    br i1 %cond_bool, label %then, label %else
  
  then:                                             ; preds = %entry
    %boxed_create_int3 = call ptr @create_int(i64 1)
    br label %merge
  
  else:                                             ; preds = %entry
    %boxed_create_function4 = call ptr @create_function(ptr @op_minus, i64 2)
    %__var_n5 = load ptr, ptr %__reserved_01, align 8
    %boxed_create_int6 = call ptr @create_int(i64 1)
    %boxed_create_int7 = call ptr @create_int(i64 2)
    %boxed_apply8 = call ptr (ptr, ptr, ...) @apply(ptr %boxed_create_function4, ptr %boxed_create_int7, ptr %__var_n5, ptr %boxed_create_int6)
    %boxed_create_function9 = call ptr @create_function(ptr @__var_fac, i64 1)
    %__anf_1 = load ptr, ptr %boxed_apply8, align 8
    %boxed_create_int10 = call ptr @create_int(i64 1)
    %boxed_apply11 = call ptr (ptr, ptr, ...) @apply(ptr %boxed_create_function9, ptr %boxed_create_int10, ptr %__anf_1)
    %boxed_create_function12 = call ptr @create_function(ptr @op_mult, i64 2)
    %__var_n13 = load ptr, ptr %__reserved_01, align 8
    %__anf_2 = load ptr, ptr %boxed_apply11, align 8
    %boxed_create_int14 = call ptr @create_int(i64 2)
    %boxed_apply15 = call ptr (ptr, ptr, ...) @apply(ptr %boxed_create_function12, ptr %boxed_create_int14, ptr %__var_n13, ptr %__anf_2)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %ite_result = phi ptr [ %boxed_create_int3, %then ], [ %boxed_apply15, %else ]
    ret ptr %ite_result
  }
  
  define i64 @main() {
  entry:
    %boxed_create_function = call ptr @create_function(ptr @__var_fac, i64 1)
    %boxed_create_int = call ptr @create_int(i64 4)
    %boxed_create_int1 = call ptr @create_int(i64 1)
    %boxed_apply = call ptr (ptr, ptr, ...) @apply(ptr %boxed_create_function, ptr %boxed_create_int1, ptr %boxed_create_int)
    %boxed_create_function2 = call ptr @create_function(ptr @print_int, i64 1)
    %__anf_3 = load ptr, ptr %boxed_apply, align 8
    %boxed_create_int3 = call ptr @create_int(i64 1)
    %boxed_apply4 = call ptr (ptr, ptr, ...) @apply(ptr %boxed_create_function2, ptr %boxed_create_int3, ptr %__anf_3)
    %boxed_create_function5 = call ptr @create_function(ptr @op_eq, i64 2)
    %__anf_4 = load ptr, ptr %boxed_apply4, align 8
    %boxed_create_unit = call ptr @create_unit()
    %boxed_create_int6 = call ptr @create_int(i64 2)
    %boxed_apply7 = call ptr (ptr, ptr, ...) @apply(ptr %boxed_create_function5, ptr %boxed_create_int6, ptr %__anf_4, ptr %boxed_create_unit)
    %boxed_create_int8 = call ptr @create_int(i64 0)
    store ptr %boxed_create_int8, ptr @__var_main, align 8
    ret i64 0
  }
