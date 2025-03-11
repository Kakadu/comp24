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
    %list_node = alloca { i64, ptr }, align 8
    %value_ptr = getelementptr inbounds { i64, ptr }, ptr %list_node, i32 0, i32 0
    %next_ptr = getelementptr inbounds { i64, ptr }, ptr %list_node, i32 0, i32 1
    store i64 2, ptr %value_ptr, align 8
    store ptr null, ptr %next_ptr, align 8
    %list_node1 = alloca { i64, ptr }, align 8
    %value_ptr2 = getelementptr inbounds { i64, ptr }, ptr %list_node1, i32 0, i32 0
    %next_ptr3 = getelementptr inbounds { i64, ptr }, ptr %list_node1, i32 0, i32 1
    store i64 1, ptr %value_ptr2, align 8
    store ptr %list_node, ptr %next_ptr3, align 8
    %x = alloca ptr, align 8
    store ptr %list_node1, ptr %x, align 8
    ret i64 0
  }
