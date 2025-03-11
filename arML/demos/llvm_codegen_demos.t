  $ make -C ../lib/runtime all > /dev/null 2>&1 && echo "Success" || echo "Failed"
  Success

  $ ./start_llvm_codegen_demos.exe < manytests/typed/001fac.ml
  ; ModuleID = 'arML'
  source_filename = "arML"
  
  define ptr @fac(ptr %0) {
  entry:
    %closure = call ptr @ct_closure(ptr @le, i32 2)
    %val_int = call ptr @ct_int_v(i32 1)
    %closure_call_result = call ptr (ptr, i32, ...) @invoke_closure_with_args(ptr %closure, i32 2, ptr %0, ptr %val_int)
    %cond_bool = call i1 @check_cond(ptr %closure_call_result)
    br i1 %cond_bool, label %then_block, label %else_block
  
  then_block:                                       ; preds = %entry
    %val_int1 = call ptr @ct_int_v(i32 1)
    br label %merge_block
  
  else_block:                                       ; preds = %entry
    %closure2 = call ptr @ct_closure(ptr @minus, i32 2)
    %val_int3 = call ptr @ct_int_v(i32 1)
    %closure_call_result4 = call ptr (ptr, i32, ...) @invoke_closure_with_args(ptr %closure2, i32 2, ptr %0, ptr %val_int3)
    %closure5 = call ptr @ct_closure(ptr @fac, i32 1)
    %closure_call_result6 = call ptr (ptr, i32, ...) @invoke_closure_with_args(ptr %closure5, i32 1, ptr %closure_call_result4)
    %closure7 = call ptr @ct_closure(ptr @multip, i32 2)
    %closure_call_result8 = call ptr (ptr, i32, ...) @invoke_closure_with_args(ptr %closure7, i32 2, ptr %0, ptr %closure_call_result6)
    br label %merge_block
  
  merge_block:                                      ; preds = %else_block, %then_block
    %if_expr_result = phi ptr [ %val_int1, %then_block ], [ %closure_call_result8, %else_block ]
    ret ptr %if_expr_result
  }
  
  define i32 @main() {
  entry:
    %closure = call ptr @ct_closure(ptr @fac, i32 1)
    %val_int = call ptr @ct_int_v(i32 4)
    %closure_call_result = call ptr (ptr, i32, ...) @invoke_closure_with_args(ptr %closure, i32 1, ptr %val_int)
    %closure1 = call ptr @ct_closure(ptr @print_int, i32 1)
    %closure_call_result2 = call ptr (ptr, i32, ...) @invoke_closure_with_args(ptr %closure1, i32 1, ptr %closure_call_result)
    %val_int3 = call ptr @ct_int_v(i32 0)
    ret i32 0
  }
  
  declare ptr @ct_int_v(i32)
  
  declare ptr @ct_bool_v(i1)
  
  declare ptr @ct_str_v(ptr)
  
  declare ptr @ct_char_v(i8)
  
  declare ptr @ct_unit_v()
  
  declare ptr @multip(ptr, ptr)
  
  declare ptr @division(ptr, ptr)
  
  declare ptr @divRemainder(ptr, ptr)
  
  declare ptr @plus(ptr, ptr)
  
  declare ptr @minus(ptr, ptr)
  
  declare ptr @uplus(ptr)
  
  declare ptr @uminus(ptr)
  
  declare ptr @unot(ptr)
  
  declare ptr @bOr(ptr, ptr)
  
  declare ptr @bAnd(ptr, ptr)
  
  declare i1 @check_cond(ptr)
  
  declare ptr @feq(ptr, ptr)
  
  declare ptr @leq(ptr, ptr)
  
  declare ptr @fneq(ptr, ptr)
  
  declare ptr @lneq(ptr, ptr)
  
  declare ptr @lt(ptr, ptr)
  
  declare ptr @le(ptr, ptr)
  
  declare ptr @gt(ptr, ptr)
  
  declare ptr @ge(ptr, ptr)
  
  declare void @print_int(ptr)
  
  declare void @print_bool(ptr)
  
  declare void @print_string(ptr)
  
  declare void @print_char(ptr)
  
  declare ptr @ct_closure(ptr, i32)
  
  declare ptr @invoke_closure_with_args(ptr, i32, ...)
  
  declare ptr @invoke_closure(ptr)
  
  declare void @pattern_matching_failure()

  $ ./start_llvm_codegen_demos.exe < manytests/typed/001fac.ml > ../lib/runtime/bin/result.ll
  $ clang -Wno-override-module -lffi ../lib/runtime/bin/result.ll ../lib/runtime/bin/runtime.o -o res.out; ./res.out
  1

  $ ./start_llvm_codegen_demos.exe < manytests/typed/002fac.ml > ../lib/runtime/bin/result.ll
  $ clang -Wno-override-module -lffi ../lib/runtime/bin/result.ll ../lib/runtime/bin/runtime.o -o res.out; ./res.out
  1

  $ ./start_llvm_codegen_demos.exe < manytests/typed/003fib.ml > ../lib/runtime/bin/result.ll
  $ clang -Wno-override-module -lffi ../lib/runtime/bin/result.ll ../lib/runtime/bin/runtime.o -o res.out; ./res.out
  1
  4

  $ ./start_llvm_codegen_demos.exe < manytests/typed/004manyargs.ml > ../lib/runtime/bin/result.ll
  $ clang -Wno-override-module -lffi ../lib/runtime/bin/result.ll ../lib/runtime/bin/runtime.o -o res.out; ./res.out
  1111111111
  1
  10
  100

  $ ./start_llvm_codegen_demos.exe < manytests/typed/005fix.ml > ../lib/runtime/bin/result.ll
  $ clang -Wno-override-module -lffi ../lib/runtime/bin/result.ll ../lib/runtime/bin/runtime.o -o res.out; ./res.out
  1

  $ ./start_llvm_codegen_demos.exe < manytests/typed/006partial.ml > ../lib/runtime/bin/result.ll
  $ clang -Wno-override-module -lffi ../lib/runtime/bin/result.ll ../lib/runtime/bin/runtime.o -o res.out; ./res.out
  19

  $ ./start_llvm_codegen_demos.exe < manytests/typed/006partial2.ml > ../lib/runtime/bin/result.ll
  $ clang -Wno-override-module -lffi ../lib/runtime/bin/result.ll ../lib/runtime/bin/runtime.o -o res.out; ./res.out
  1
  2
  3
  7

  $ ./start_llvm_codegen_demos.exe < manytests/typed/006partial3.ml > ../lib/runtime/bin/result.ll
  $ clang -Wno-override-module -lffi ../lib/runtime/bin/result.ll ../lib/runtime/bin/runtime.o -o res.out; ./res.out
  4
  8
  9

  $ ./start_llvm_codegen_demos.exe < manytests/typed/007order.ml > ../lib/runtime/bin/result.ll
  $ clang -Wno-override-module -lffi ../lib/runtime/bin/result.ll ../lib/runtime/bin/runtime.o -o res.out; ./res.out
  1
  2
  4
  -1
  103
  -555555
  10000

  $ ./start_llvm_codegen_demos.exe < manytests/typed/008ascription.ml > ../lib/runtime/bin/result.ll
  $ clang -Wno-override-module -lffi ../lib/runtime/bin/result.ll ../lib/runtime/bin/runtime.o -o res.out; ./res.out
  5

  $ ./start_llvm_codegen_demos.exe < manytests/typed/012fibcps.ml > ../lib/runtime/bin/result.ll
  $ clang -Wno-override-module -lffi ../lib/runtime/bin/result.ll ../lib/runtime/bin/runtime.o -o res.out; ./res.out
  6
