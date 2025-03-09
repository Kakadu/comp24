
  $ ./llvm_demo.exe << EOF
  > let sum a b c d = a + b + c + d
  > let main = print_int(sum 1 2 3 4)
  ; ModuleID = 'MEML'
  source_filename = "MEML"
  
  declare ptr @create_int_val(i32)
  
  declare ptr @create_bool_val(i1)
  
  declare ptr @mul(ptr, ptr)
  
  declare ptr @divv(ptr, ptr)
  
  declare ptr @plus(ptr, ptr)
  
  declare ptr @minus(ptr, ptr)
  
  declare ptr @eq(ptr, ptr)
  
  declare ptr @neq(ptr, ptr)
  
  declare ptr @less(ptr, ptr)
  
  declare ptr @leq(ptr, ptr)
  
  declare ptr @gre(ptr, ptr)
  
  declare ptr @greq(ptr, ptr)
  
  declare ptr @or(ptr, ptr)
  
  declare ptr @and(ptr, ptr)
  
  declare void @print_int(ptr)
  
  declare ptr @new_closure(ptr, i32)
  
  declare ptr @app_closure(ptr, i32, ...)
  
  declare ptr @call_closure(ptr)
  
  declare i1 @get_bool(ptr)
  
  define ptr @bin_op1(ptr %0, ptr %1) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @plus, i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %1)
    ret ptr %app_closure_res
  }
  
  define ptr @bin_op0(ptr %0, ptr %1, ptr %2) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @plus, i32 2)
    %empty_closure1 = call ptr @new_closure(ptr @bin_op1, i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 2, ptr %0, ptr %1)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %app_closure_res, ptr %2)
    ret ptr %app_closure_res2
  }
  
  define ptr @bin_op(ptr %0, ptr %1, ptr %2, ptr %3) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @plus, i32 2)
    %empty_closure1 = call ptr @new_closure(ptr @bin_op0, i32 3)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 3, ptr %0, ptr %1, ptr %2)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %app_closure_res, ptr %3)
    ret ptr %app_closure_res2
  }
  
  define ptr @sum(ptr %0, ptr %1, ptr %2, ptr %3) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @bin_op, i32 4)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 4, ptr %0, ptr %1, ptr %2, ptr %3)
    ret ptr %app_closure_res
  }
  
  define ptr @app() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @sum, i32 4)
    %int_var = call ptr @create_int_val(i32 1)
    %int_var1 = call ptr @create_int_val(i32 2)
    %int_var2 = call ptr @create_int_val(i32 3)
    %int_var3 = call ptr @create_int_val(i32 4)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 4, ptr %int_var, ptr %int_var1, ptr %int_var2, ptr %int_var3)
    ret ptr %app_closure_res
  }
  
  define i32 @main() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %empty_closure1 = call ptr @new_closure(ptr @app, i32 0)
    %call_closure_res = call ptr @call_closure(ptr %empty_closure1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %call_closure_res)
    ret i32 0
  }
  
  $ ./llvm_demo.exe < manytests/typed/001fac.ml 
  ; ModuleID = 'MEML'
  source_filename = "MEML"
  
  declare ptr @create_int_val(i32)
  
  declare ptr @create_bool_val(i1)
  
  declare ptr @mul(ptr, ptr)
  
  declare ptr @divv(ptr, ptr)
  
  declare ptr @plus(ptr, ptr)
  
  declare ptr @minus(ptr, ptr)
  
  declare ptr @eq(ptr, ptr)
  
  declare ptr @neq(ptr, ptr)
  
  declare ptr @less(ptr, ptr)
  
  declare ptr @leq(ptr, ptr)
  
  declare ptr @gre(ptr, ptr)
  
  declare ptr @greq(ptr, ptr)
  
  declare ptr @or(ptr, ptr)
  
  declare ptr @and(ptr, ptr)
  
  declare void @print_int(ptr)
  
  declare ptr @new_closure(ptr, i32)
  
  declare ptr @app_closure(ptr, i32, ...)
  
  declare ptr @call_closure(ptr)
  
  declare i1 @get_bool(ptr)
  
  define ptr @bin_op(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @leq, i32 2)
    %int_var = call ptr @create_int_val(i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %int_var)
    ret ptr %app_closure_res
  }
  
  define ptr @bin_op0(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @minus, i32 2)
    %int_var = call ptr @create_int_val(i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %int_var)
    ret ptr %app_closure_res
  }
  
  define ptr @app(ptr %0, ptr %1) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @bin_op0, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %1)
    %app_closure_res1 = call ptr (ptr, i32, ...) @app_closure(ptr %0, i32 1, ptr %app_closure_res)
    ret ptr %app_closure_res1
  }
  
  define ptr @bin_op1(ptr %0, ptr %1) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @mul, i32 2)
    %empty_closure1 = call ptr @new_closure(ptr @app, i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 2, ptr %0, ptr %1)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %1, ptr %app_closure_res)
    ret ptr %app_closure_res2
  }
  
  define ptr @fac(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @bin_op, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %0)
    %cond_i1 = call i1 @get_bool(ptr %app_closure_res)
    br i1 %cond_i1, label %then, label %else
  
  then:                                             ; preds = %entry
    %int_var = call ptr @create_int_val(i32 1)
    br label %merge
  
  else:                                             ; preds = %entry
    %empty_closure1 = call ptr @new_closure(ptr @bin_op1, i32 2)
    %empty_closure2 = call ptr @new_closure(ptr @fac, i32 1)
    %app_closure_res3 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 2, ptr %empty_closure2, ptr %0)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %if_res = phi ptr [ %int_var, %then ], [ %app_closure_res3, %else ]
    ret ptr %if_res
  }
  
  define ptr @app0() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @fac, i32 1)
    %int_var = call ptr @create_int_val(i32 4)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %int_var)
    ret ptr %app_closure_res
  }
  
  define ptr @unit() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %empty_closure1 = call ptr @new_closure(ptr @app0, i32 0)
    %call_closure_res = call ptr @call_closure(ptr %empty_closure1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %call_closure_res)
    ret ptr %app_closure_res
  }
  
  define i32 @main() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @unit, i32 0)
    %call_closure_res = call ptr @call_closure(ptr %empty_closure)
    %int_var = call ptr @create_int_val(i32 0)
    ret i32 0
  }
  $ ./llvm_demo.exe < manytests/typed/002fac.ml 
  ; ModuleID = 'MEML'
  source_filename = "MEML"
  
  declare ptr @create_int_val(i32)
  
  declare ptr @create_bool_val(i1)
  
  declare ptr @mul(ptr, ptr)
  
  declare ptr @divv(ptr, ptr)
  
  declare ptr @plus(ptr, ptr)
  
  declare ptr @minus(ptr, ptr)
  
  declare ptr @eq(ptr, ptr)
  
  declare ptr @neq(ptr, ptr)
  
  declare ptr @less(ptr, ptr)
  
  declare ptr @leq(ptr, ptr)
  
  declare ptr @gre(ptr, ptr)
  
  declare ptr @greq(ptr, ptr)
  
  declare ptr @or(ptr, ptr)
  
  declare ptr @and(ptr, ptr)
  
  declare void @print_int(ptr)
  
  declare ptr @new_closure(ptr, i32)
  
  declare ptr @app_closure(ptr, i32, ...)
  
  declare ptr @call_closure(ptr)
  
  declare i1 @get_bool(ptr)
  
  define ptr @bin_op(ptr %0, ptr %1) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @mul, i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %1, ptr %0)
    ret ptr %app_closure_res
  }
  
  define ptr @lambada(ptr %0, ptr %1, ptr %2) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @bin_op, i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %1, ptr %2)
    %app_closure_res1 = call ptr (ptr, i32, ...) @app_closure(ptr %0, i32 1, ptr %app_closure_res)
    ret ptr %app_closure_res1
  }
  
  define ptr @bin_op0(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @eq, i32 2)
    %int_var = call ptr @create_int_val(i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %int_var)
    ret ptr %app_closure_res
  }
  
  define ptr @app(ptr %0, ptr %1) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @lambada, i32 3)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %1)
    ret ptr %app_closure_res
  }
  
  define ptr @bin_op1(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @minus, i32 2)
    %int_var = call ptr @create_int_val(i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %int_var)
    ret ptr %app_closure_res
  }
  
  define ptr @fac_cps(ptr %0, ptr %1) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @bin_op0, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %0)
    %cond_i1 = call i1 @get_bool(ptr %app_closure_res)
    br i1 %cond_i1, label %then, label %else
  
  then:                                             ; preds = %entry
    %int_var = call ptr @create_int_val(i32 1)
    %app_closure_res1 = call ptr (ptr, i32, ...) @app_closure(ptr %1, i32 1, ptr %int_var)
    br label %merge
  
  else:                                             ; preds = %entry
    %empty_closure2 = call ptr @new_closure(ptr @fac_cps, i32 2)
    %empty_closure3 = call ptr @new_closure(ptr @bin_op1, i32 1)
    %app_closure_res4 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure3, i32 1, ptr %0)
    %empty_closure5 = call ptr @new_closure(ptr @app, i32 2)
    %app_closure_res6 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure5, i32 2, ptr %1, ptr %0)
    %app_closure_res7 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure2, i32 2, ptr %app_closure_res4, ptr %app_closure_res6)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %if_res = phi ptr [ %app_closure_res1, %then ], [ %app_closure_res7, %else ]
    ret ptr %if_res
  }
  
  define ptr @lambada0(ptr %0) {
  entry:
    ret ptr %0
  }
  
  define ptr @app0() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @fac_cps, i32 2)
    %int_var = call ptr @create_int_val(i32 4)
    %empty_closure1 = call ptr @new_closure(ptr @lambada0, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %int_var, ptr %empty_closure1)
    ret ptr %app_closure_res
  }
  
  define ptr @unit() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %empty_closure1 = call ptr @new_closure(ptr @app0, i32 0)
    %call_closure_res = call ptr @call_closure(ptr %empty_closure1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %call_closure_res)
    ret ptr %app_closure_res
  }
  
  define i32 @main() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @unit, i32 0)
    %call_closure_res = call ptr @call_closure(ptr %empty_closure)
    %int_var = call ptr @create_int_val(i32 0)
    ret i32 0
  }
  $ ./llvm_demo.exe < manytests/typed/003fib.ml 
  ; ModuleID = 'MEML'
  source_filename = "MEML"
  
  declare ptr @create_int_val(i32)
  
  declare ptr @create_bool_val(i1)
  
  declare ptr @mul(ptr, ptr)
  
  declare ptr @divv(ptr, ptr)
  
  declare ptr @plus(ptr, ptr)
  
  declare ptr @minus(ptr, ptr)
  
  declare ptr @eq(ptr, ptr)
  
  declare ptr @neq(ptr, ptr)
  
  declare ptr @less(ptr, ptr)
  
  declare ptr @leq(ptr, ptr)
  
  declare ptr @gre(ptr, ptr)
  
  declare ptr @greq(ptr, ptr)
  
  declare ptr @or(ptr, ptr)
  
  declare ptr @and(ptr, ptr)
  
  declare void @print_int(ptr)
  
  declare ptr @new_closure(ptr, i32)
  
  declare ptr @app_closure(ptr, i32, ...)
  
  declare ptr @call_closure(ptr)
  
  declare i1 @get_bool(ptr)
  
  define ptr @bin_op(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @minus, i32 2)
    %int_var = call ptr @create_int_val(i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %int_var)
    ret ptr %app_closure_res
  }
  
  define ptr @n1(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @bin_op, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %0)
    ret ptr %app_closure_res
  }
  
  define ptr @bin_op0(ptr %0, ptr %1) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @plus, i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %1)
    ret ptr %app_closure_res
  }
  
  define ptr @ab(ptr %0, ptr %1) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @bin_op0, i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %1)
    ret ptr %app_closure_res
  }
  
  define ptr @bin_op1(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @eq, i32 2)
    %int_var = call ptr @create_int_val(i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %int_var)
    ret ptr %app_closure_res
  }
  
  define ptr @app(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @n1, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %0)
    ret ptr %app_closure_res
  }
  
  define ptr @app0(ptr %0, ptr %1) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @ab, i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %1)
    ret ptr %app_closure_res
  }
  
  define ptr @fib_acc(ptr %0, ptr %1, ptr %2) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @bin_op1, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %2)
    %cond_i1 = call i1 @get_bool(ptr %app_closure_res)
    br i1 %cond_i1, label %then, label %else
  
  then:                                             ; preds = %entry
    br label %merge
  
  else:                                             ; preds = %entry
    %empty_closure1 = call ptr @new_closure(ptr @fib_acc, i32 3)
    %empty_closure2 = call ptr @new_closure(ptr @app0, i32 2)
    %app_closure_res3 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure2, i32 2, ptr %0, ptr %1)
    %empty_closure4 = call ptr @new_closure(ptr @app, i32 1)
    %app_closure_res5 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure4, i32 1, ptr %2)
    %app_closure_res6 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 3, ptr %1, ptr %app_closure_res3, ptr %app_closure_res5)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %if_res = phi ptr [ %1, %then ], [ %app_closure_res6, %else ]
    ret ptr %if_res
  }
  
  define ptr @bin_op2(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @less, i32 2)
    %int_var = call ptr @create_int_val(i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %int_var)
    ret ptr %app_closure_res
  }
  
  define ptr @bin_op3(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @minus, i32 2)
    %int_var = call ptr @create_int_val(i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %int_var)
    ret ptr %app_closure_res
  }
  
  define ptr @app1(ptr %0, ptr %1) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @bin_op3, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %1)
    %app_closure_res1 = call ptr (ptr, i32, ...) @app_closure(ptr %0, i32 1, ptr %app_closure_res)
    ret ptr %app_closure_res1
  }
  
  define ptr @bin_op5(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @minus, i32 2)
    %int_var = call ptr @create_int_val(i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %int_var)
    ret ptr %app_closure_res
  }
  
  define ptr @bin_op4(ptr %0, ptr %1) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @plus, i32 2)
    %empty_closure1 = call ptr @new_closure(ptr @bin_op5, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 1, ptr %1)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %0, i32 1, ptr %app_closure_res)
    %empty_closure3 = call ptr @new_closure(ptr @app1, i32 2)
    %app_closure_res4 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure3, i32 2, ptr %0, ptr %1)
    %app_closure_res5 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %app_closure_res2, ptr %app_closure_res4)
    ret ptr %app_closure_res5
  }
  
  define ptr @fib(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @bin_op2, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %0)
    %cond_i1 = call i1 @get_bool(ptr %app_closure_res)
    br i1 %cond_i1, label %then, label %else
  
  then:                                             ; preds = %entry
    br label %merge
  
  else:                                             ; preds = %entry
    %empty_closure1 = call ptr @new_closure(ptr @bin_op4, i32 2)
    %empty_closure2 = call ptr @new_closure(ptr @fib, i32 1)
    %app_closure_res3 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 2, ptr %empty_closure2, ptr %0)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %if_res = phi ptr [ %0, %then ], [ %app_closure_res3, %else ]
    ret ptr %if_res
  }
  
  define ptr @app2() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @fib_acc, i32 3)
    %int_var = call ptr @create_int_val(i32 0)
    %int_var1 = call ptr @create_int_val(i32 1)
    %int_var2 = call ptr @create_int_val(i32 4)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 3, ptr %int_var, ptr %int_var1, ptr %int_var2)
    ret ptr %app_closure_res
  }
  
  define ptr @unit() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %empty_closure1 = call ptr @new_closure(ptr @app2, i32 0)
    %call_closure_res = call ptr @call_closure(ptr %empty_closure1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %call_closure_res)
    ret ptr %app_closure_res
  }
  
  define ptr @app3() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @fib, i32 1)
    %int_var = call ptr @create_int_val(i32 4)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %int_var)
    ret ptr %app_closure_res
  }
  
  define ptr @unit0() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %empty_closure1 = call ptr @new_closure(ptr @app3, i32 0)
    %call_closure_res = call ptr @call_closure(ptr %empty_closure1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %call_closure_res)
    ret ptr %app_closure_res
  }
  
  define i32 @main() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @unit, i32 0)
    %call_closure_res = call ptr @call_closure(ptr %empty_closure)
    %empty_closure1 = call ptr @new_closure(ptr @unit0, i32 0)
    %call_closure_res2 = call ptr @call_closure(ptr %empty_closure1)
    %int_var = call ptr @create_int_val(i32 0)
    ret i32 0
  }
  $ ./llvm_demo.exe < manytests/typed/004manyargs.ml 
  ; ModuleID = 'MEML'
  source_filename = "MEML"
  
  declare ptr @create_int_val(i32)
  
  declare ptr @create_bool_val(i1)
  
  declare ptr @mul(ptr, ptr)
  
  declare ptr @divv(ptr, ptr)
  
  declare ptr @plus(ptr, ptr)
  
  declare ptr @minus(ptr, ptr)
  
  declare ptr @eq(ptr, ptr)
  
  declare ptr @neq(ptr, ptr)
  
  declare ptr @less(ptr, ptr)
  
  declare ptr @leq(ptr, ptr)
  
  declare ptr @gre(ptr, ptr)
  
  declare ptr @greq(ptr, ptr)
  
  declare ptr @or(ptr, ptr)
  
  declare ptr @and(ptr, ptr)
  
  declare void @print_int(ptr)
  
  declare ptr @new_closure(ptr, i32)
  
  declare ptr @app_closure(ptr, i32, ...)
  
  declare ptr @call_closure(ptr)
  
  declare i1 @get_bool(ptr)
  
  define ptr @bin_op() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @eq, i32 2)
    %int_var = call ptr @create_int_val(i32 1)
    %int_var1 = call ptr @create_int_val(i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %int_var, ptr %int_var1)
    ret ptr %app_closure_res
  }
  
  define ptr @wrap(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @bin_op, i32 0)
    %call_closure_res = call ptr @call_closure(ptr %empty_closure)
    %cond_i1 = call i1 @get_bool(ptr %call_closure_res)
    br i1 %cond_i1, label %then, label %else
  
  then:                                             ; preds = %entry
    br label %merge
  
  else:                                             ; preds = %entry
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %if_res = phi ptr [ %0, %then ], [ %0, %else ]
    ret ptr %if_res
  }
  
  define ptr @a0(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %0)
    ret ptr %app_closure_res
  }
  
  define ptr @b0(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %0)
    ret ptr %app_closure_res
  }
  
  define ptr @c0(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %0)
    ret ptr %app_closure_res
  }
  
  define ptr @test3(ptr %0, ptr %1, ptr %2) {
  entry:
    %int_var = call ptr @create_int_val(i32 0)
    ret ptr %int_var
  }
  
  define ptr @bin_op8(ptr %0, ptr %1) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @plus, i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %1)
    ret ptr %app_closure_res
  }
  
  define ptr @bin_op7(ptr %0, ptr %1, ptr %2) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @plus, i32 2)
    %empty_closure1 = call ptr @new_closure(ptr @bin_op8, i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 2, ptr %0, ptr %1)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %app_closure_res, ptr %2)
    ret ptr %app_closure_res2
  }
  
  define ptr @bin_op6(ptr %0, ptr %1, ptr %2, ptr %3) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @plus, i32 2)
    %empty_closure1 = call ptr @new_closure(ptr @bin_op7, i32 3)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 3, ptr %0, ptr %1, ptr %2)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %app_closure_res, ptr %3)
    ret ptr %app_closure_res2
  }
  
  define ptr @bin_op5(ptr %0, ptr %1, ptr %2, ptr %3, ptr %4) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @plus, i32 2)
    %empty_closure1 = call ptr @new_closure(ptr @bin_op6, i32 4)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 4, ptr %0, ptr %1, ptr %2, ptr %3)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %app_closure_res, ptr %4)
    ret ptr %app_closure_res2
  }
  
  define ptr @bin_op4(ptr %0, ptr %1, ptr %2, ptr %3, ptr %4, ptr %5) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @plus, i32 2)
    %empty_closure1 = call ptr @new_closure(ptr @bin_op5, i32 5)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 5, ptr %0, ptr %1, ptr %2, ptr %3, ptr %4)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %app_closure_res, ptr %5)
    ret ptr %app_closure_res2
  }
  
  define ptr @bin_op3(ptr %0, ptr %1, ptr %2, ptr %3, ptr %4, ptr %5, ptr %6) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @plus, i32 2)
    %empty_closure1 = call ptr @new_closure(ptr @bin_op4, i32 6)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 6, ptr %0, ptr %1, ptr %2, ptr %3, ptr %4, ptr %5)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %app_closure_res, ptr %6)
    ret ptr %app_closure_res2
  }
  
  define ptr @bin_op2(ptr %0, ptr %1, ptr %2, ptr %3, ptr %4, ptr %5, ptr %6, ptr %7) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @plus, i32 2)
    %empty_closure1 = call ptr @new_closure(ptr @bin_op3, i32 7)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 7, ptr %0, ptr %1, ptr %2, ptr %3, ptr %4, ptr %5, ptr %6)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %app_closure_res, ptr %7)
    ret ptr %app_closure_res2
  }
  
  define ptr @bin_op1(ptr %0, ptr %1, ptr %2, ptr %3, ptr %4, ptr %5, ptr %6, ptr %7, ptr %8) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @plus, i32 2)
    %empty_closure1 = call ptr @new_closure(ptr @bin_op2, i32 8)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 8, ptr %0, ptr %1, ptr %2, ptr %3, ptr %4, ptr %5, ptr %6, ptr %7)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %app_closure_res, ptr %8)
    ret ptr %app_closure_res2
  }
  
  define ptr @bin_op0(ptr %0, ptr %1, ptr %2, ptr %3, ptr %4, ptr %5, ptr %6, ptr %7, ptr %8, ptr %9) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @plus, i32 2)
    %empty_closure1 = call ptr @new_closure(ptr @bin_op1, i32 9)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 9, ptr %0, ptr %1, ptr %2, ptr %3, ptr %4, ptr %5, ptr %6, ptr %7, ptr %8)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %app_closure_res, ptr %9)
    ret ptr %app_closure_res2
  }
  
  define ptr @test10(ptr %0, ptr %1, ptr %2, ptr %3, ptr %4, ptr %5, ptr %6, ptr %7, ptr %8, ptr %9) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @bin_op0, i32 10)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 10, ptr %0, ptr %1, ptr %2, ptr %3, ptr %4, ptr %5, ptr %6, ptr %7, ptr %8, ptr %9)
    ret ptr %app_closure_res
  }
  
  define ptr @rez() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @wrap, i32 1)
    %empty_closure1 = call ptr @new_closure(ptr @test10, i32 10)
    %int_var = call ptr @create_int_val(i32 1)
    %int_var2 = call ptr @create_int_val(i32 10)
    %int_var3 = call ptr @create_int_val(i32 100)
    %int_var4 = call ptr @create_int_val(i32 1000)
    %int_var5 = call ptr @create_int_val(i32 10000)
    %int_var6 = call ptr @create_int_val(i32 100000)
    %int_var7 = call ptr @create_int_val(i32 1000000)
    %int_var8 = call ptr @create_int_val(i32 10000000)
    %int_var9 = call ptr @create_int_val(i32 100000000)
    %int_var10 = call ptr @create_int_val(i32 1000000000)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 11, ptr %empty_closure1, ptr %int_var, ptr %int_var2, ptr %int_var3, ptr %int_var4, ptr %int_var5, ptr %int_var6, ptr %int_var7, ptr %int_var8, ptr %int_var9, ptr %int_var10)
    ret ptr %app_closure_res
  }
  
  define ptr @unit() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %empty_closure1 = call ptr @new_closure(ptr @rez, i32 0)
    %call_closure_res = call ptr @call_closure(ptr %empty_closure1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %call_closure_res)
    ret ptr %app_closure_res
  }
  
  define ptr @temp2() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @wrap, i32 1)
    %empty_closure1 = call ptr @new_closure(ptr @test3, i32 3)
    %int_var = call ptr @create_int_val(i32 1)
    %int_var2 = call ptr @create_int_val(i32 10)
    %int_var3 = call ptr @create_int_val(i32 100)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 4, ptr %empty_closure1, ptr %int_var, ptr %int_var2, ptr %int_var3)
    ret ptr %app_closure_res
  }
  
  define i32 @main() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @unit, i32 0)
    %call_closure_res = call ptr @call_closure(ptr %empty_closure)
    %int_var = call ptr @create_int_val(i32 0)
    ret i32 0
  }
  $ ./llvm_demo.exe < manytests/typed/005fix.ml 
  ; ModuleID = 'MEML'
  source_filename = "MEML"
  
  declare ptr @create_int_val(i32)
  
  declare ptr @create_bool_val(i1)
  
  declare ptr @mul(ptr, ptr)
  
  declare ptr @divv(ptr, ptr)
  
  declare ptr @plus(ptr, ptr)
  
  declare ptr @minus(ptr, ptr)
  
  declare ptr @eq(ptr, ptr)
  
  declare ptr @neq(ptr, ptr)
  
  declare ptr @less(ptr, ptr)
  
  declare ptr @leq(ptr, ptr)
  
  declare ptr @gre(ptr, ptr)
  
  declare ptr @greq(ptr, ptr)
  
  declare ptr @or(ptr, ptr)
  
  declare ptr @and(ptr, ptr)
  
  declare void @print_int(ptr)
  
  declare ptr @new_closure(ptr, i32)
  
  declare ptr @app_closure(ptr, i32, ...)
  
  declare ptr @call_closure(ptr)
  
  declare i1 @get_bool(ptr)
  
  define ptr @app(ptr %0, ptr %1) {
  entry:
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %1, i32 1, ptr %0)
    ret ptr %app_closure_res
  }
  
  define ptr @fix(ptr %0, ptr %1) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @app, i32 2)
    %empty_closure1 = call ptr @new_closure(ptr @fix, i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %empty_closure1)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %0, i32 2, ptr %app_closure_res, ptr %1)
    ret ptr %app_closure_res2
  }
  
  define ptr @bin_op(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @leq, i32 2)
    %int_var = call ptr @create_int_val(i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %int_var)
    ret ptr %app_closure_res
  }
  
  define ptr @bin_op0(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @minus, i32 2)
    %int_var = call ptr @create_int_val(i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %int_var)
    ret ptr %app_closure_res
  }
  
  define ptr @app0(ptr %0, ptr %1) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @bin_op0, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %0)
    %app_closure_res1 = call ptr (ptr, i32, ...) @app_closure(ptr %1, i32 1, ptr %app_closure_res)
    ret ptr %app_closure_res1
  }
  
  define ptr @bin_op1(ptr %0, ptr %1) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @mul, i32 2)
    %empty_closure1 = call ptr @new_closure(ptr @app0, i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 2, ptr %0, ptr %1)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %app_closure_res)
    ret ptr %app_closure_res2
  }
  
  define ptr @fac(ptr %0, ptr %1) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @bin_op, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %1)
    %cond_i1 = call i1 @get_bool(ptr %app_closure_res)
    br i1 %cond_i1, label %then, label %else
  
  then:                                             ; preds = %entry
    %int_var = call ptr @create_int_val(i32 1)
    br label %merge
  
  else:                                             ; preds = %entry
    %empty_closure1 = call ptr @new_closure(ptr @bin_op1, i32 2)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 2, ptr %1, ptr %0)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %if_res = phi ptr [ %int_var, %then ], [ %app_closure_res2, %else ]
    ret ptr %if_res
  }
  
  define ptr @app1() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @fix, i32 2)
    %empty_closure1 = call ptr @new_closure(ptr @fac, i32 2)
    %int_var = call ptr @create_int_val(i32 6)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %empty_closure1, ptr %int_var)
    ret ptr %app_closure_res
  }
  
  define ptr @unit() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %empty_closure1 = call ptr @new_closure(ptr @app1, i32 0)
    %call_closure_res = call ptr @call_closure(ptr %empty_closure1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %call_closure_res)
    ret ptr %app_closure_res
  }
  
  define i32 @main() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @unit, i32 0)
    %call_closure_res = call ptr @call_closure(ptr %empty_closure)
    %int_var = call ptr @create_int_val(i32 0)
    ret i32 0
  }

  $ ./llvm_demo.exe < manytests/typed/006partial2.ml 
  ; ModuleID = 'MEML'
  source_filename = "MEML"
  
  declare ptr @create_int_val(i32)
  
  declare ptr @create_bool_val(i1)
  
  declare ptr @mul(ptr, ptr)
  
  declare ptr @divv(ptr, ptr)
  
  declare ptr @plus(ptr, ptr)
  
  declare ptr @minus(ptr, ptr)
  
  declare ptr @eq(ptr, ptr)
  
  declare ptr @neq(ptr, ptr)
  
  declare ptr @less(ptr, ptr)
  
  declare ptr @leq(ptr, ptr)
  
  declare ptr @gre(ptr, ptr)
  
  declare ptr @greq(ptr, ptr)
  
  declare ptr @or(ptr, ptr)
  
  declare ptr @and(ptr, ptr)
  
  declare void @print_int(ptr)
  
  declare ptr @new_closure(ptr, i32)
  
  declare ptr @app_closure(ptr, i32, ...)
  
  declare ptr @call_closure(ptr)
  
  declare i1 @get_bool(ptr)
  
  define ptr @unit(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %0)
    ret ptr %app_closure_res
  }
  
  define ptr @unit0(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %0)
    ret ptr %app_closure_res
  }
  
  define ptr @unit1(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %0)
    ret ptr %app_closure_res
  }
  
  define ptr @bin_op0(ptr %0, ptr %1) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @mul, i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %1)
    ret ptr %app_closure_res
  }
  
  define ptr @bin_op(ptr %0, ptr %1, ptr %2) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @plus, i32 2)
    %empty_closure1 = call ptr @new_closure(ptr @bin_op0, i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 2, ptr %1, ptr %2)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %app_closure_res)
    ret ptr %app_closure_res2
  }
  
  define ptr @foo(ptr %0, ptr %1, ptr %2) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @unit, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %0)
    %empty_closure1 = call ptr @new_closure(ptr @unit0, i32 1)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 1, ptr %1)
    %empty_closure3 = call ptr @new_closure(ptr @unit1, i32 1)
    %app_closure_res4 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure3, i32 1, ptr %2)
    %empty_closure5 = call ptr @new_closure(ptr @bin_op, i32 3)
    %app_closure_res6 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure5, i32 3, ptr %0, ptr %1, ptr %2)
    ret ptr %app_closure_res6
  }
  
  define ptr @foo0() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @foo, i32 3)
    %int_var = call ptr @create_int_val(i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %int_var)
    ret ptr %app_closure_res
  }
  
  define ptr @foo1() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @foo0, i32 0)
    %call_closure_res = call ptr @call_closure(ptr %empty_closure)
    %int_var = call ptr @create_int_val(i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %call_closure_res, i32 1, ptr %int_var)
    ret ptr %app_closure_res
  }
  
  define ptr @foo2() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @foo1, i32 0)
    %call_closure_res = call ptr @call_closure(ptr %empty_closure)
    %int_var = call ptr @create_int_val(i32 3)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %call_closure_res, i32 1, ptr %int_var)
    ret ptr %app_closure_res
  }
  
  define ptr @unit2() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %empty_closure1 = call ptr @new_closure(ptr @foo2, i32 0)
    %call_closure_res = call ptr @call_closure(ptr %empty_closure1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %call_closure_res)
    ret ptr %app_closure_res
  }
  
  define i32 @main() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @unit2, i32 0)
    %call_closure_res = call ptr @call_closure(ptr %empty_closure)
    %int_var = call ptr @create_int_val(i32 0)
    ret i32 0
  }
  $ ./llvm_demo.exe < manytests/typed/006partial3.ml 
  ; ModuleID = 'MEML'
  source_filename = "MEML"
  
  declare ptr @create_int_val(i32)
  
  declare ptr @create_bool_val(i1)
  
  declare ptr @mul(ptr, ptr)
  
  declare ptr @divv(ptr, ptr)
  
  declare ptr @plus(ptr, ptr)
  
  declare ptr @minus(ptr, ptr)
  
  declare ptr @eq(ptr, ptr)
  
  declare ptr @neq(ptr, ptr)
  
  declare ptr @less(ptr, ptr)
  
  declare ptr @leq(ptr, ptr)
  
  declare ptr @gre(ptr, ptr)
  
  declare ptr @greq(ptr, ptr)
  
  declare ptr @or(ptr, ptr)
  
  declare ptr @and(ptr, ptr)
  
  declare void @print_int(ptr)
  
  declare ptr @new_closure(ptr, i32)
  
  declare ptr @app_closure(ptr, i32, ...)
  
  declare ptr @call_closure(ptr)
  
  declare i1 @get_bool(ptr)
  
  define ptr @lambada0(ptr %0, ptr %1) {
  entry:
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %0, i32 1, ptr %1)
    ret ptr %app_closure_res
  }
  
  define ptr @unit(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %0)
    ret ptr %app_closure_res
  }
  
  define ptr @lambada(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @unit, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %0)
    %empty_closure1 = call ptr @new_closure(ptr @lambada0, i32 2)
    %empty_closure2 = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res3 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 1, ptr %empty_closure2)
    ret ptr %app_closure_res3
  }
  
  define ptr @unit0(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %0)
    ret ptr %app_closure_res
  }
  
  define ptr @foo(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @unit0, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %0)
    %empty_closure1 = call ptr @new_closure(ptr @lambada, i32 1)
    ret ptr %empty_closure1
  }
  
  define ptr @unit1() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @foo, i32 1)
    %int_var = call ptr @create_int_val(i32 4)
    %int_var1 = call ptr @create_int_val(i32 8)
    %int_var2 = call ptr @create_int_val(i32 9)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 3, ptr %int_var, ptr %int_var1, ptr %int_var2)
    ret ptr %app_closure_res
  }
  
  define i32 @main() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @unit1, i32 0)
    %call_closure_res = call ptr @call_closure(ptr %empty_closure)
    %int_var = call ptr @create_int_val(i32 0)
    ret i32 0
  }
  $ ./llvm_demo.exe < manytests/typed/007order.ml 
  ; ModuleID = 'MEML'
  source_filename = "MEML"
  
  declare ptr @create_int_val(i32)
  
  declare ptr @create_bool_val(i1)
  
  declare ptr @mul(ptr, ptr)
  
  declare ptr @divv(ptr, ptr)
  
  declare ptr @plus(ptr, ptr)
  
  declare ptr @minus(ptr, ptr)
  
  declare ptr @eq(ptr, ptr)
  
  declare ptr @neq(ptr, ptr)
  
  declare ptr @less(ptr, ptr)
  
  declare ptr @leq(ptr, ptr)
  
  declare ptr @gre(ptr, ptr)
  
  declare ptr @greq(ptr, ptr)
  
  declare ptr @or(ptr, ptr)
  
  declare ptr @and(ptr, ptr)
  
  declare void @print_int(ptr)
  
  declare ptr @new_closure(ptr, i32)
  
  declare ptr @app_closure(ptr, i32, ...)
  
  declare ptr @call_closure(ptr)
  
  declare i1 @get_bool(ptr)
  
  define ptr @bin_op(ptr %0, ptr %1) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @plus, i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %1)
    ret ptr %app_closure_res
  }
  
  define ptr @unit(ptr %0, ptr %1) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %empty_closure1 = call ptr @new_closure(ptr @bin_op, i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 2, ptr %0, ptr %1)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %app_closure_res)
    ret ptr %app_closure_res2
  }
  
  define ptr @unit0(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %0)
    ret ptr %app_closure_res
  }
  
  define ptr @bin_op2(ptr %0, ptr %1) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @mul, i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %1)
    ret ptr %app_closure_res
  }
  
  define ptr @bin_op1(ptr %0, ptr %1, ptr %2) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @divv, i32 2)
    %empty_closure1 = call ptr @new_closure(ptr @bin_op2, i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 2, ptr %1, ptr %2)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %app_closure_res, ptr %0)
    ret ptr %app_closure_res2
  }
  
  define ptr @bin_op0(ptr %0, ptr %1, ptr %2, ptr %3) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @plus, i32 2)
    %empty_closure1 = call ptr @new_closure(ptr @bin_op1, i32 3)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 3, ptr %0, ptr %1, ptr %2)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %app_closure_res, ptr %3)
    ret ptr %app_closure_res2
  }
  
  define ptr @_start(ptr %0, ptr %1, ptr %2, ptr %3, ptr %4, ptr %5, ptr %6, ptr %7, ptr %8) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @unit, i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %2, ptr %4)
    %empty_closure1 = call ptr @new_closure(ptr @unit0, i32 1)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 1, ptr %8)
    %empty_closure3 = call ptr @new_closure(ptr @bin_op0, i32 4)
    %app_closure_res4 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure3, i32 4, ptr %5, ptr %2, ptr %4, ptr %7)
    ret ptr %app_closure_res4
  }
  
  define ptr @app0() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %int_var = call ptr @create_int_val(i32 -1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %int_var)
    ret ptr %app_closure_res
  }
  
  define ptr @app1() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %int_var = call ptr @create_int_val(i32 4)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %int_var)
    ret ptr %app_closure_res
  }
  
  define ptr @app2() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %int_var = call ptr @create_int_val(i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %int_var)
    ret ptr %app_closure_res
  }
  
  define ptr @app3() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %int_var = call ptr @create_int_val(i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %int_var)
    ret ptr %app_closure_res
  }
  
  define ptr @app() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @_start, i32 9)
    %empty_closure1 = call ptr @new_closure(ptr @app3, i32 0)
    %call_closure_res = call ptr @call_closure(ptr %empty_closure1)
    %empty_closure2 = call ptr @new_closure(ptr @app2, i32 0)
    %call_closure_res3 = call ptr @call_closure(ptr %empty_closure2)
    %int_var = call ptr @create_int_val(i32 3)
    %empty_closure4 = call ptr @new_closure(ptr @app1, i32 0)
    %call_closure_res5 = call ptr @call_closure(ptr %empty_closure4)
    %int_var6 = call ptr @create_int_val(i32 100)
    %int_var7 = call ptr @create_int_val(i32 1000)
    %empty_closure8 = call ptr @new_closure(ptr @app0, i32 0)
    %call_closure_res9 = call ptr @call_closure(ptr %empty_closure8)
    %int_var10 = call ptr @create_int_val(i32 10000)
    %int_var11 = call ptr @create_int_val(i32 -555555)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 9, ptr %call_closure_res, ptr %call_closure_res3, ptr %int_var, ptr %call_closure_res5, ptr %int_var6, ptr %int_var7, ptr %call_closure_res9, ptr %int_var10, ptr %int_var11)
    ret ptr %app_closure_res
  }
  
  define i32 @main() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %empty_closure1 = call ptr @new_closure(ptr @app, i32 0)
    %call_closure_res = call ptr @call_closure(ptr %empty_closure1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %call_closure_res)
    ret i32 0
  }
  $ ./llvm_demo.exe < manytests/typed/008ascription.ml 
  ; ModuleID = 'MEML'
  source_filename = "MEML"
  
  declare ptr @create_int_val(i32)
  
  declare ptr @create_bool_val(i1)
  
  declare ptr @mul(ptr, ptr)
  
  declare ptr @divv(ptr, ptr)
  
  declare ptr @plus(ptr, ptr)
  
  declare ptr @minus(ptr, ptr)
  
  declare ptr @eq(ptr, ptr)
  
  declare ptr @neq(ptr, ptr)
  
  declare ptr @less(ptr, ptr)
  
  declare ptr @leq(ptr, ptr)
  
  declare ptr @gre(ptr, ptr)
  
  declare ptr @greq(ptr, ptr)
  
  declare ptr @or(ptr, ptr)
  
  declare ptr @and(ptr, ptr)
  
  declare void @print_int(ptr)
  
  declare ptr @new_closure(ptr, i32)
  
  declare ptr @app_closure(ptr, i32, ...)
  
  declare ptr @call_closure(ptr)
  
  declare i1 @get_bool(ptr)
  
  define ptr @app(ptr %0, ptr %1) {
  entry:
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %0, i32 1, ptr %1)
    ret ptr %app_closure_res
  }
  
  define ptr @addi(ptr %0, ptr %1, ptr %2) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @app, i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %1, ptr %2)
    %app_closure_res1 = call ptr (ptr, i32, ...) @app_closure(ptr %0, i32 2, ptr %2, ptr %app_closure_res)
    ret ptr %app_closure_res1
  }
  
  define ptr @bin_op(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @plus, i32 2)
    %int_var = call ptr @create_int_val(i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %int_var)
    ret ptr %app_closure_res
  }
  
  define ptr @bin_op0(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @mul, i32 2)
    %int_var = call ptr @create_int_val(i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %int_var)
    ret ptr %app_closure_res
  }
  
  define ptr @lambada(ptr %0, ptr %1) {
  entry:
    %cond_i1 = call i1 @get_bool(ptr %1)
    br i1 %cond_i1, label %then, label %else
  
  then:                                             ; preds = %entry
    %empty_closure = call ptr @new_closure(ptr @bin_op, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %0)
    br label %merge
  
  else:                                             ; preds = %entry
    %empty_closure1 = call ptr @new_closure(ptr @bin_op0, i32 1)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 1, ptr %0)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %if_res = phi ptr [ %app_closure_res, %then ], [ %app_closure_res2, %else ]
    ret ptr %if_res
  }
  
  define ptr @bin_op2(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @divv, i32 2)
    %int_var = call ptr @create_int_val(i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %int_var)
    ret ptr %app_closure_res
  }
  
  define ptr @bin_op1(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @eq, i32 2)
    %empty_closure1 = call ptr @new_closure(ptr @bin_op2, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 1, ptr %0)
    %int_var = call ptr @create_int_val(i32 0)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %app_closure_res, ptr %int_var)
    ret ptr %app_closure_res2
  }
  
  define ptr @lambada0(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @bin_op1, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %0)
    ret ptr %app_closure_res
  }
  
  define ptr @app0() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @addi, i32 3)
    %empty_closure1 = call ptr @new_closure(ptr @lambada, i32 2)
    %empty_closure2 = call ptr @new_closure(ptr @lambada0, i32 1)
    %int_var = call ptr @create_int_val(i32 4)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 3, ptr %empty_closure1, ptr %empty_closure2, ptr %int_var)
    ret ptr %app_closure_res
  }
  
  define ptr @unit() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %empty_closure1 = call ptr @new_closure(ptr @app0, i32 0)
    %call_closure_res = call ptr @call_closure(ptr %empty_closure1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %call_closure_res)
    ret ptr %app_closure_res
  }
  
  define i32 @main() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @unit, i32 0)
    %call_closure_res = call ptr @call_closure(ptr %empty_closure)
    %int_var = call ptr @create_int_val(i32 0)
    ret i32 0
  }

