  $ dune exec test_codegen

  ; ModuleID = 'BDSML'
  source_filename = "BDSML"
  target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
  target triple = "x86_64-pc-linux-gnu"
  
  declare ptr @create_int(i64 %0)
  
  declare ptr @create_bool(i1 %0)
  
  declare ptr @create_unit()
  
  declare ptr @create_char(i8 %0)
  
  declare ptr @create_string(...)
  
  declare ptr @create_apply(...)
  
  declare ptr @create_tuple(i64 %0, ...)
  
  declare ptr @create_list(ptr %0, ptr %1)
  
  declare ptr @create_empty_list()
  
  declare ptr @create_cons(i1 %0, ptr %1)
  
  declare ptr @create_closure(ptr %0, i64 %1)
  
  declare i64 @get_int(ptr %0)
  
  declare i1 @get_bool(ptr %0)
  
  declare i8 @get_char(ptr %0)
  
  declare i64 @__op_plus(i64 %0, i64 %1)
  
  declare i64 @__op_minus(i64 %0, i64 %1)
  
  declare i64 @__op_mult(i64 %0, i64 %1)
  
  declare i64 @__op_div(i64 %0, i64 %1)
  
  declare i64 @__op_neg(i64 %0)
  
  declare i64 @__op_pos(i64 %0)
  
  declare i1 @not(i1 %0)
  
  declare i1 @__op_gt(ptr %0, ptr %1)
  
  declare i1 @__op_ge(ptr %0, ptr %1)
  
  declare i1 @__op_lt(ptr %0, ptr %1)
  
  declare i1 @__op_le(ptr %0, ptr %1)
  
  declare i1 @__op_eq(ptr %0, ptr %1)
  
  declare i1 @__op_neq(ptr %0, ptr %1)
  
  declare i1 @__op_or(i1 %0, i1 %1)
  
  declare i1 @__op_and(i1 %0, i1 %1)
  
  declare i1 @__op_phys_eq(ptr %0, ptr %1)
  
  declare void @print_int(i64 %0)
  
  define ptr @abs_value(ptr %n) {
  entry:
    %boxed_int = call ptr @create_int(i64 52)
    %n1 = load ptr, ptr %n, align 8
    %call_tmp = call i64 @__op_plus(ptr %boxed_int, ptr %n1)
    ret i64 %call_tmp
  }
  
  define i64 @main() {
  entry:
    ret i64 0
  }

