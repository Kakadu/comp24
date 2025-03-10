  $ dune exec test_codegen
  ; ModuleID = 'BDSML'
  source_filename = "BDSML"
  target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
  target triple = "x86_64-pc-linux-gnu"
  
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
  
  define i64 @main() {
  entry:
    %x = alloca { i64, i8, i1, [4 x i8], ptr }, align 8
    store { i64, i8, i1, [4 x i8], ptr } { i64 42, i8 99, i1 true, [4 x i8] c"BDSM", ptr @unit_t }, ptr %x, align 8
    ret i64 0
  }
  
  define void @unit_t() {
  entry:
  }
