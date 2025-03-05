  $ ./compiler.exe < manytests/typed/001fac.ml
  $ cat out.ll | grep -E 'source_filename|target datalayout|target triple|ModuleID' --invert-match
  
  @a1 = global ptr null
  
  declare ptr @create_int(i64)
  
  declare ptr @create_bool(i1)
  
  declare ptr @create_unit()
  
  declare i64 @get_int(ptr)
  
  declare i1 @get_bool(ptr)
  
  declare ptr @create_closure(ptr, i64)
  
  declare ptr @apply(ptr, ptr)
  
  declare ptr @print_int(ptr)
  
  declare ptr @print_bool(ptr)
  
  declare ptr @create_empty_list()
  
  declare ptr @list_cons(ptr, ptr)
  
  declare ptr @list_hd(ptr)
  
  declare ptr @list_tl(ptr)
  
  declare ptr @list_length(ptr)
  
  declare ptr @create_tuple(i32, ...)
  
  declare ptr @unpack_tuple(ptr, ptr)
  
  declare ptr @match_failure(ptr, ptr)
  
  declare ptr @uplus(ptr)
  
  declare ptr @uminus(ptr)
  
  declare ptr @add(ptr, ptr)
  
  declare ptr @sub(ptr, ptr)
  
  declare ptr @mul(ptr, ptr)
  
  declare ptr @div_op(ptr, ptr)
  
  declare ptr @and_op(ptr, ptr)
  
  declare ptr @or_op(ptr, ptr)
  
  declare ptr @gt(ptr, ptr)
  
  declare ptr @lt(ptr, ptr)
  
  declare ptr @le(ptr, ptr)
  
  declare ptr @ge(ptr, ptr)
  
  declare ptr @eq(ptr, ptr)
  
  declare ptr @ne(ptr, ptr)
  
  declare ptr @phys_eq(ptr, ptr)
  
  declare ptr @phys_ne(ptr, ptr)
  
  define ptr @fac(ptr %n) {
  entry:
    %closure = call ptr @create_closure(ptr @le, i64 2)
    %apply_result = call ptr @apply(ptr %closure, ptr %n)
    %boxed_int = call ptr @create_int(i64 1)
    %apply_result1 = call ptr @apply(ptr %apply_result, ptr %boxed_int)
    %cond_bool = call i1 @get_bool(ptr %apply_result1)
    br i1 %cond_bool, label %then, label %else
  
  then:                                             ; preds = %entry
    %boxed_int2 = call ptr @create_int(i64 1)
    br label %merge
  
  else:                                             ; preds = %entry
    %closure3 = call ptr @create_closure(ptr @sub, i64 2)
    %apply_result4 = call ptr @apply(ptr %closure3, ptr %n)
    %boxed_int5 = call ptr @create_int(i64 1)
    %apply_result6 = call ptr @apply(ptr %apply_result4, ptr %boxed_int5)
    %closure7 = call ptr @create_closure(ptr @fac, i64 1)
    %apply_result8 = call ptr @apply(ptr %closure7, ptr %apply_result6)
    %closure9 = call ptr @create_closure(ptr @mul, i64 2)
    %apply_result10 = call ptr @apply(ptr %closure9, ptr %n)
    %apply_result11 = call ptr @apply(ptr %apply_result10, ptr %apply_result8)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %ite_result = phi ptr [ %boxed_int2, %then ], [ %apply_result11, %else ]
    ret ptr %ite_result
  }
  
  define i32 @main() {
  entry:
    %closure = call ptr @create_closure(ptr @fac, i64 1)
    %boxed_int = call ptr @create_int(i64 4)
    %apply_result = call ptr @apply(ptr %closure, ptr %boxed_int)
    %closure1 = call ptr @create_closure(ptr @print_int, i64 1)
    %apply_result2 = call ptr @apply(ptr %closure1, ptr %apply_result)
    %boxed_int3 = call ptr @create_int(i64 0)
    store ptr %boxed_int3, ptr @a1, align 8
    ret i32 0
  }
