
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
  
  define ptr @sum(ptr %0, ptr %1, ptr %2, ptr %3) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @plus, i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %1)
    %empty_closure1 = call ptr @new_closure(ptr @plus, i32 2)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 2, ptr %app_closure_res, ptr %2)
    %empty_closure3 = call ptr @new_closure(ptr @plus, i32 2)
    %app_closure_res4 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure3, i32 2, ptr %app_closure_res2, ptr %3)
    ret ptr %app_closure_res4
  }
  
  define i32 @main() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @sum, i32 4)
    %int_var = call ptr @create_int_val(i32 1)
    %int_var1 = call ptr @create_int_val(i32 2)
    %int_var2 = call ptr @create_int_val(i32 3)
    %int_var3 = call ptr @create_int_val(i32 4)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 4, ptr %int_var, ptr %int_var1, ptr %int_var2, ptr %int_var3)
    %empty_closure4 = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res5 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure4, i32 1, ptr %app_closure_res)
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
  
  define ptr @fac(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @leq, i32 2)
    %int_var = call ptr @create_int_val(i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %int_var)
    %cond_i1 = call i1 @get_bool(ptr %app_closure_res)
    br i1 %cond_i1, label %then, label %else
  
  then:                                             ; preds = %entry
    %int_var1 = call ptr @create_int_val(i32 1)
    br label %merge
  
  else:                                             ; preds = %entry
    %empty_closure2 = call ptr @new_closure(ptr @minus, i32 2)
    %int_var3 = call ptr @create_int_val(i32 1)
    %app_closure_res4 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure2, i32 2, ptr %0, ptr %int_var3)
    %empty_closure5 = call ptr @new_closure(ptr @mul, i32 2)
    %empty_closure6 = call ptr @new_closure(ptr @fac, i32 1)
    %app_closure_res7 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure6, i32 1, ptr %app_closure_res4)
    %app_closure_res8 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure5, i32 2, ptr %0, ptr %app_closure_res7)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %if_res = phi ptr [ %int_var1, %then ], [ %app_closure_res8, %else ]
    ret ptr %if_res
  }
  
  define i32 @main() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @fac, i32 1)
    %int_var = call ptr @create_int_val(i32 4)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %int_var)
    %empty_closure1 = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 1, ptr %app_closure_res)
    %int_var3 = call ptr @create_int_val(i32 0)
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
  
  define ptr @lambada(ptr %0, ptr %1, ptr %2) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @mul, i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %2, ptr %1)
    %app_closure_res1 = call ptr (ptr, i32, ...) @app_closure(ptr %0, i32 1, ptr %app_closure_res)
    ret ptr %app_closure_res1
  }
  
  define ptr @fac_cps(ptr %0, ptr %1) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @eq, i32 2)
    %int_var = call ptr @create_int_val(i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %int_var)
    %cond_i1 = call i1 @get_bool(ptr %app_closure_res)
    br i1 %cond_i1, label %then, label %else
  
  then:                                             ; preds = %entry
    %int_var1 = call ptr @create_int_val(i32 1)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %1, i32 1, ptr %int_var1)
    br label %merge
  
  else:                                             ; preds = %entry
    %empty_closure3 = call ptr @new_closure(ptr @minus, i32 2)
    %int_var4 = call ptr @create_int_val(i32 1)
    %app_closure_res5 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure3, i32 2, ptr %0, ptr %int_var4)
    %empty_closure6 = call ptr @new_closure(ptr @lambada, i32 3)
    %app_closure_res7 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure6, i32 2, ptr %1, ptr %0)
    %empty_closure8 = call ptr @new_closure(ptr @fac_cps, i32 2)
    %app_closure_res9 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure8, i32 2, ptr %app_closure_res5, ptr %app_closure_res7)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %if_res = phi ptr [ %app_closure_res2, %then ], [ %app_closure_res9, %else ]
    ret ptr %if_res
  }
  
  define ptr @lambada0(ptr %0) {
  entry:
    ret ptr %0
  }
  
  define i32 @main() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @fac_cps, i32 2)
    %int_var = call ptr @create_int_val(i32 4)
    %empty_closure1 = call ptr @new_closure(ptr @lambada0, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %int_var, ptr %empty_closure1)
    %empty_closure2 = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res3 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure2, i32 1, ptr %app_closure_res)
    %int_var4 = call ptr @create_int_val(i32 0)
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
  
  define ptr @fib_acc(ptr %0, ptr %1, ptr %2) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @eq, i32 2)
    %int_var = call ptr @create_int_val(i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %2, ptr %int_var)
    %cond_i1 = call i1 @get_bool(ptr %app_closure_res)
    br i1 %cond_i1, label %then, label %else
  
  then:                                             ; preds = %entry
    br label %merge
  
  else:                                             ; preds = %entry
    %empty_closure1 = call ptr @new_closure(ptr @minus, i32 2)
    %int_var2 = call ptr @create_int_val(i32 1)
    %app_closure_res3 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 2, ptr %2, ptr %int_var2)
    %empty_closure4 = call ptr @new_closure(ptr @plus, i32 2)
    %app_closure_res5 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure4, i32 2, ptr %0, ptr %1)
    %empty_closure6 = call ptr @new_closure(ptr @fib_acc, i32 3)
    %app_closure_res7 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure6, i32 3, ptr %1, ptr %app_closure_res5, ptr %app_closure_res3)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %if_res = phi ptr [ %1, %then ], [ %app_closure_res7, %else ]
    ret ptr %if_res
  }
  
  define ptr @fib(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @less, i32 2)
    %int_var = call ptr @create_int_val(i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %int_var)
    %cond_i1 = call i1 @get_bool(ptr %app_closure_res)
    br i1 %cond_i1, label %then, label %else
  
  then:                                             ; preds = %entry
    br label %merge
  
  else:                                             ; preds = %entry
    %empty_closure1 = call ptr @new_closure(ptr @minus, i32 2)
    %int_var2 = call ptr @create_int_val(i32 1)
    %app_closure_res3 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 2, ptr %0, ptr %int_var2)
    %empty_closure4 = call ptr @new_closure(ptr @minus, i32 2)
    %int_var5 = call ptr @create_int_val(i32 2)
    %app_closure_res6 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure4, i32 2, ptr %0, ptr %int_var5)
    %empty_closure7 = call ptr @new_closure(ptr @plus, i32 2)
    %empty_closure8 = call ptr @new_closure(ptr @fib, i32 1)
    %app_closure_res9 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure8, i32 1, ptr %app_closure_res3)
    %empty_closure10 = call ptr @new_closure(ptr @fib, i32 1)
    %app_closure_res11 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure10, i32 1, ptr %app_closure_res6)
    %app_closure_res12 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure7, i32 2, ptr %app_closure_res9, ptr %app_closure_res11)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %if_res = phi ptr [ %0, %then ], [ %app_closure_res12, %else ]
    ret ptr %if_res
  }
  
  define i32 @main() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @fib_acc, i32 3)
    %int_var = call ptr @create_int_val(i32 0)
    %int_var1 = call ptr @create_int_val(i32 1)
    %int_var2 = call ptr @create_int_val(i32 4)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 3, ptr %int_var, ptr %int_var1, ptr %int_var2)
    %empty_closure3 = call ptr @new_closure(ptr @fib, i32 1)
    %int_var4 = call ptr @create_int_val(i32 4)
    %app_closure_res5 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure3, i32 1, ptr %int_var4)
    %empty_closure6 = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res7 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure6, i32 1, ptr %app_closure_res)
    %empty_closure8 = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res9 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure8, i32 1, ptr %app_closure_res5)
    %int_var10 = call ptr @create_int_val(i32 0)
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
  
  define ptr @wrap(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @eq, i32 2)
    %int_var = call ptr @create_int_val(i32 1)
    %int_var1 = call ptr @create_int_val(i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %int_var, ptr %int_var1)
    %cond_i1 = call i1 @get_bool(ptr %app_closure_res)
    br i1 %cond_i1, label %then, label %else
  
  then:                                             ; preds = %entry
    br label %merge
  
  else:                                             ; preds = %entry
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %if_res = phi ptr [ %0, %then ], [ %0, %else ]
    ret ptr %if_res
  }
  
  define ptr @test3(ptr %0, ptr %1, ptr %2) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %0)
    %empty_closure1 = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 1, ptr %1)
    %empty_closure3 = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res4 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure3, i32 1, ptr %2)
    %int_var = call ptr @create_int_val(i32 0)
    ret ptr %int_var
  }
  
  define ptr @test10(ptr %0, ptr %1, ptr %2, ptr %3, ptr %4, ptr %5, ptr %6, ptr %7, ptr %8, ptr %9) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @plus, i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %1)
    %empty_closure1 = call ptr @new_closure(ptr @plus, i32 2)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 2, ptr %app_closure_res, ptr %2)
    %empty_closure3 = call ptr @new_closure(ptr @plus, i32 2)
    %app_closure_res4 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure3, i32 2, ptr %app_closure_res2, ptr %3)
    %empty_closure5 = call ptr @new_closure(ptr @plus, i32 2)
    %app_closure_res6 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure5, i32 2, ptr %app_closure_res4, ptr %4)
    %empty_closure7 = call ptr @new_closure(ptr @plus, i32 2)
    %app_closure_res8 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure7, i32 2, ptr %app_closure_res6, ptr %5)
    %empty_closure9 = call ptr @new_closure(ptr @plus, i32 2)
    %app_closure_res10 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure9, i32 2, ptr %app_closure_res8, ptr %6)
    %empty_closure11 = call ptr @new_closure(ptr @plus, i32 2)
    %app_closure_res12 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure11, i32 2, ptr %app_closure_res10, ptr %7)
    %empty_closure13 = call ptr @new_closure(ptr @plus, i32 2)
    %app_closure_res14 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure13, i32 2, ptr %app_closure_res12, ptr %8)
    %empty_closure15 = call ptr @new_closure(ptr @plus, i32 2)
    %app_closure_res16 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure15, i32 2, ptr %app_closure_res14, ptr %9)
    ret ptr %app_closure_res16
  }
  
  define i32 @main() {
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
    %empty_closure11 = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res12 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure11, i32 1, ptr %app_closure_res)
    %empty_closure13 = call ptr @new_closure(ptr @wrap, i32 1)
    %empty_closure14 = call ptr @new_closure(ptr @test3, i32 3)
    %int_var15 = call ptr @create_int_val(i32 1)
    %int_var16 = call ptr @create_int_val(i32 10)
    %int_var17 = call ptr @create_int_val(i32 100)
    %app_closure_res18 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure13, i32 4, ptr %empty_closure14, ptr %int_var15, ptr %int_var16, ptr %int_var17)
    %int_var19 = call ptr @create_int_val(i32 0)
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
  
  define ptr @fix(ptr %0, ptr %1) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @fix, i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %0)
    %app_closure_res1 = call ptr (ptr, i32, ...) @app_closure(ptr %0, i32 2, ptr %app_closure_res, ptr %1)
    ret ptr %app_closure_res1
  }
  
  define ptr @fac(ptr %0, ptr %1) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @leq, i32 2)
    %int_var = call ptr @create_int_val(i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %1, ptr %int_var)
    %cond_i1 = call i1 @get_bool(ptr %app_closure_res)
    br i1 %cond_i1, label %then, label %else
  
  then:                                             ; preds = %entry
    %int_var1 = call ptr @create_int_val(i32 1)
    br label %merge
  
  else:                                             ; preds = %entry
    %empty_closure2 = call ptr @new_closure(ptr @minus, i32 2)
    %int_var3 = call ptr @create_int_val(i32 1)
    %app_closure_res4 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure2, i32 2, ptr %1, ptr %int_var3)
    %empty_closure5 = call ptr @new_closure(ptr @mul, i32 2)
    %app_closure_res6 = call ptr (ptr, i32, ...) @app_closure(ptr %0, i32 1, ptr %app_closure_res4)
    %app_closure_res7 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure5, i32 2, ptr %1, ptr %app_closure_res6)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %if_res = phi ptr [ %int_var1, %then ], [ %app_closure_res7, %else ]
    ret ptr %if_res
  }
  
  define i32 @main() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @fix, i32 2)
    %empty_closure1 = call ptr @new_closure(ptr @fac, i32 2)
    %int_var = call ptr @create_int_val(i32 6)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %empty_closure1, ptr %int_var)
    %empty_closure2 = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res3 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure2, i32 1, ptr %app_closure_res)
    %int_var4 = call ptr @create_int_val(i32 0)
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
  
  define ptr @foo(ptr %0, ptr %1, ptr %2) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @mul, i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %1, ptr %2)
    %empty_closure1 = call ptr @new_closure(ptr @plus, i32 2)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 2, ptr %0, ptr %app_closure_res)
    %empty_closure3 = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res4 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure3, i32 1, ptr %0)
    %empty_closure5 = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res6 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure5, i32 1, ptr %1)
    %empty_closure7 = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res8 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure7, i32 1, ptr %2)
    ret ptr %app_closure_res2
  }
  
  define i32 @main() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @foo, i32 3)
    %int_var = call ptr @create_int_val(i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %int_var)
    %int_var1 = call ptr @create_int_val(i32 2)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %app_closure_res, i32 1, ptr %int_var1)
    %int_var3 = call ptr @create_int_val(i32 3)
    %app_closure_res4 = call ptr (ptr, i32, ...) @app_closure(ptr %app_closure_res2, i32 1, ptr %int_var3)
    %empty_closure5 = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res6 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure5, i32 1, ptr %app_closure_res4)
    %int_var7 = call ptr @create_int_val(i32 0)
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
  
  define ptr @lambada(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %0)
    %empty_closure1 = call ptr @new_closure(ptr @lambada0, i32 2)
    %empty_closure2 = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res3 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 1, ptr %empty_closure2)
    ret ptr %app_closure_res3
  }
  
  define ptr @foo(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %0)
    %empty_closure1 = call ptr @new_closure(ptr @lambada, i32 1)
    ret ptr %empty_closure1
  }
  
  define i32 @main() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @foo, i32 1)
    %int_var = call ptr @create_int_val(i32 4)
    %int_var1 = call ptr @create_int_val(i32 8)
    %int_var2 = call ptr @create_int_val(i32 9)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 3, ptr %int_var, ptr %int_var1, ptr %int_var2)
    %int_var3 = call ptr @create_int_val(i32 0)
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
  
  define ptr @_start(ptr %0, ptr %1, ptr %2, ptr %3, ptr %4, ptr %5, ptr %6, ptr %7, ptr %8) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @plus, i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %2, ptr %4)
    %empty_closure1 = call ptr @new_closure(ptr @mul, i32 2)
    %app_closure_res2 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 2, ptr %2, ptr %4)
    %empty_closure3 = call ptr @new_closure(ptr @divv, i32 2)
    %app_closure_res4 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure3, i32 2, ptr %app_closure_res2, ptr %5)
    %empty_closure5 = call ptr @new_closure(ptr @plus, i32 2)
    %app_closure_res6 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure5, i32 2, ptr %app_closure_res4, ptr %7)
    %empty_closure7 = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res8 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure7, i32 1, ptr %app_closure_res)
    %empty_closure9 = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res10 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure9, i32 1, ptr %8)
    ret ptr %app_closure_res6
  }
  
  define i32 @main() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @print_int, i32 1)
    %int_var = call ptr @create_int_val(i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 1, ptr %int_var)
    %empty_closure1 = call ptr @new_closure(ptr @print_int, i32 1)
    %int_var2 = call ptr @create_int_val(i32 2)
    %app_closure_res3 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 1, ptr %int_var2)
    %empty_closure4 = call ptr @new_closure(ptr @print_int, i32 1)
    %int_var5 = call ptr @create_int_val(i32 4)
    %app_closure_res6 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure4, i32 1, ptr %int_var5)
    %empty_closure7 = call ptr @new_closure(ptr @print_int, i32 1)
    %int_var8 = call ptr @create_int_val(i32 -1)
    %app_closure_res9 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure7, i32 1, ptr %int_var8)
    %empty_closure10 = call ptr @new_closure(ptr @_start, i32 9)
    %int_var11 = call ptr @create_int_val(i32 3)
    %int_var12 = call ptr @create_int_val(i32 100)
    %int_var13 = call ptr @create_int_val(i32 1000)
    %int_var14 = call ptr @create_int_val(i32 10000)
    %int_var15 = call ptr @create_int_val(i32 -555555)
    %app_closure_res16 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure10, i32 9, ptr %app_closure_res, ptr %app_closure_res3, ptr %int_var11, ptr %app_closure_res6, ptr %int_var12, ptr %int_var13, ptr %app_closure_res9, ptr %int_var14, ptr %int_var15)
    %empty_closure17 = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res18 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure17, i32 1, ptr %app_closure_res16)
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
  
  define ptr @addi(ptr %0, ptr %1, ptr %2) {
  entry:
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %1, i32 1, ptr %2)
    %app_closure_res1 = call ptr (ptr, i32, ...) @app_closure(ptr %0, i32 2, ptr %2, ptr %app_closure_res)
    ret ptr %app_closure_res1
  }
  
  define ptr @lambada(ptr %0, ptr %1) {
  entry:
    %cond_i1 = call i1 @get_bool(ptr %1)
    br i1 %cond_i1, label %then, label %else
  
  then:                                             ; preds = %entry
    %empty_closure = call ptr @new_closure(ptr @plus, i32 2)
    %int_var = call ptr @create_int_val(i32 1)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %int_var)
    br label %merge
  
  else:                                             ; preds = %entry
    %empty_closure1 = call ptr @new_closure(ptr @mul, i32 2)
    %int_var2 = call ptr @create_int_val(i32 2)
    %app_closure_res3 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 2, ptr %0, ptr %int_var2)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %if_res = phi ptr [ %app_closure_res, %then ], [ %app_closure_res3, %else ]
    ret ptr %if_res
  }
  
  define ptr @lambada0(ptr %0) {
  entry:
    %empty_closure = call ptr @new_closure(ptr @divv, i32 2)
    %int_var = call ptr @create_int_val(i32 2)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 2, ptr %0, ptr %int_var)
    %empty_closure1 = call ptr @new_closure(ptr @eq, i32 2)
    %int_var2 = call ptr @create_int_val(i32 0)
    %app_closure_res3 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure1, i32 2, ptr %app_closure_res, ptr %int_var2)
    ret ptr %app_closure_res3
  }
  
  define i32 @main() {
  entry:
    %empty_closure = call ptr @new_closure(ptr @addi, i32 3)
    %empty_closure1 = call ptr @new_closure(ptr @lambada, i32 2)
    %empty_closure2 = call ptr @new_closure(ptr @lambada0, i32 1)
    %int_var = call ptr @create_int_val(i32 4)
    %app_closure_res = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure, i32 3, ptr %empty_closure1, ptr %empty_closure2, ptr %int_var)
    %empty_closure3 = call ptr @new_closure(ptr @print_int, i32 1)
    %app_closure_res4 = call ptr (ptr, i32, ...) @app_closure(ptr %empty_closure3, i32 1, ptr %app_closure_res)
    %int_var5 = call ptr @create_int_val(i32 0)
    ret i32 0
  }

