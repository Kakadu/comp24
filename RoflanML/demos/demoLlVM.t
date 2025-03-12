  $ for prog in ../manytests/typed/*; do
  > echo "\n--------------- $prog"
  > dune exec ./demoLlVM.exe < $prog
  > echo "\n\n"
  > done
  
  --------------- ../manytests/typed/001fac.ml
  ; ModuleID = 'Roflan'
  source_filename = "Roflan"
  
  @main.1 = global ptr null
  
  declare ptr @RoflanML_eq(ptr, ptr)
  
  declare ptr @RoflanML_neq(ptr, ptr)
  
  declare ptr @RoflanML_gt(ptr, ptr)
  
  declare ptr @RoflanML_ge(ptr, ptr)
  
  declare ptr @RoflanML_lt(ptr, ptr)
  
  declare ptr @RoflanML_le(ptr, ptr)
  
  declare ptr @RoflanML_or(ptr, ptr)
  
  declare ptr @RoflanML_and(ptr, ptr)
  
  declare ptr @RoflanML_add(ptr, ptr)
  
  declare ptr @RoflanML_sub(ptr, ptr)
  
  declare ptr @RoflanML_mul(ptr, ptr)
  
  declare ptr @RoflanML_div(ptr, ptr)
  
  declare ptr @Create_int(i64)
  
  declare ptr @Create_bool(i1)
  
  declare ptr @Create_unit()
  
  declare ptr @Apply(ptr, ...)
  
  declare i64 @Get_int(ptr)
  
  declare i1 @Get_bool(ptr)
  
  declare ptr @print_int(ptr)
  
  declare ptr @print_bool(ptr)
  
  declare ptr @Create_closure(ptr, i64)
  
  define ptr @fac(ptr %n) {
  entry:
    %closure = call ptr @Create_closure(ptr @RoflanML_le, i64 2)
    %boxed_int = call ptr @Create_int(i64 1)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 2, ptr %n, ptr %boxed_int)
    %cond_bool = call i1 @Get_bool(ptr %apply_result)
    br i1 %cond_bool, label %then, label %else
  
  then:                                             ; preds = %entry
    %boxed_int1 = call ptr @Create_int(i64 1)
    br label %merge
  
  else:                                             ; preds = %entry
    %closure2 = call ptr @Create_closure(ptr @RoflanML_sub, i64 2)
    %boxed_int3 = call ptr @Create_int(i64 1)
    %apply_result4 = call ptr (ptr, ...) @Apply(ptr %closure2, i64 2, ptr %n, ptr %boxed_int3)
    %closure5 = call ptr @Create_closure(ptr @fac, i64 1)
    %apply_result6 = call ptr (ptr, ...) @Apply(ptr %closure5, i64 1, ptr %apply_result4)
    %closure7 = call ptr @Create_closure(ptr @RoflanML_mul, i64 2)
    %apply_result8 = call ptr (ptr, ...) @Apply(ptr %closure7, i64 2, ptr %n, ptr %apply_result6)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %branch_result = phi ptr [ %boxed_int1, %then ], [ %apply_result8, %else ]
    ret ptr %branch_result
  }
  
  define i32 @main() {
  entry:
    %closure = call ptr @Create_closure(ptr @fac, i64 1)
    %boxed_int = call ptr @Create_int(i64 4)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 1, ptr %boxed_int)
    %closure1 = call ptr @Create_closure(ptr @print_int, i64 1)
    %apply_result2 = call ptr (ptr, ...) @Apply(ptr %closure1, i64 1, ptr %apply_result)
    %boxed_int3 = call ptr @Create_int(i64 0)
    store ptr %boxed_int3, ptr @main.1, align 8
    ret i32 0
  }
  
  
  
  
  --------------- ../manytests/typed/002fac.ml
  ; ModuleID = 'Roflan'
  source_filename = "Roflan"
  
  @main.1 = global ptr null
  
  declare ptr @RoflanML_eq(ptr, ptr)
  
  declare ptr @RoflanML_neq(ptr, ptr)
  
  declare ptr @RoflanML_gt(ptr, ptr)
  
  declare ptr @RoflanML_ge(ptr, ptr)
  
  declare ptr @RoflanML_lt(ptr, ptr)
  
  declare ptr @RoflanML_le(ptr, ptr)
  
  declare ptr @RoflanML_or(ptr, ptr)
  
  declare ptr @RoflanML_and(ptr, ptr)
  
  declare ptr @RoflanML_add(ptr, ptr)
  
  declare ptr @RoflanML_sub(ptr, ptr)
  
  declare ptr @RoflanML_mul(ptr, ptr)
  
  declare ptr @RoflanML_div(ptr, ptr)
  
  declare ptr @Create_int(i64)
  
  declare ptr @Create_bool(i1)
  
  declare ptr @Create_unit()
  
  declare ptr @Apply(ptr, ...)
  
  declare i64 @Get_int(ptr)
  
  declare i1 @Get_bool(ptr)
  
  declare ptr @print_int(ptr)
  
  declare ptr @print_bool(ptr)
  
  declare ptr @Create_closure(ptr, i64)
  
  define ptr @ll_0(ptr %k, ptr %n, ptr %p) {
  entry:
    %closure = call ptr @Create_closure(ptr @RoflanML_mul, i64 2)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 2, ptr %p, ptr %n)
    %apply_result1 = call ptr (ptr, ...) @Apply(ptr %k, i64 1, ptr %apply_result)
    ret ptr %apply_result1
  }
  
  define ptr @fac_cps(ptr %n, ptr %k) {
  entry:
    %closure = call ptr @Create_closure(ptr @RoflanML_eq, i64 2)
    %boxed_int = call ptr @Create_int(i64 1)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 2, ptr %n, ptr %boxed_int)
    %cond_bool = call i1 @Get_bool(ptr %apply_result)
    br i1 %cond_bool, label %then, label %else
  
  then:                                             ; preds = %entry
    %boxed_int1 = call ptr @Create_int(i64 1)
    %apply_result2 = call ptr (ptr, ...) @Apply(ptr %k, i64 1, ptr %boxed_int1)
    br label %merge
  
  else:                                             ; preds = %entry
    %closure3 = call ptr @Create_closure(ptr @RoflanML_sub, i64 2)
    %boxed_int4 = call ptr @Create_int(i64 1)
    %apply_result5 = call ptr (ptr, ...) @Apply(ptr %closure3, i64 2, ptr %n, ptr %boxed_int4)
    %closure6 = call ptr @Create_closure(ptr @ll_0, i64 3)
    %apply_result7 = call ptr (ptr, ...) @Apply(ptr %closure6, i64 2, ptr %k, ptr %n)
    %closure8 = call ptr @Create_closure(ptr @fac_cps, i64 2)
    %apply_result9 = call ptr (ptr, ...) @Apply(ptr %closure8, i64 2, ptr %apply_result5, ptr %apply_result7)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %branch_result = phi ptr [ %apply_result2, %then ], [ %apply_result9, %else ]
    ret ptr %branch_result
  }
  
  define ptr @ll_1(ptr %alpha_0) {
  entry:
    ret ptr %alpha_0
  }
  
  define i32 @main() {
  entry:
    %closure = call ptr @Create_closure(ptr @fac_cps, i64 2)
    %boxed_int = call ptr @Create_int(i64 4)
    %closure1 = call ptr @Create_closure(ptr @ll_1, i64 1)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 2, ptr %boxed_int, ptr %closure1)
    %closure2 = call ptr @Create_closure(ptr @print_int, i64 1)
    %apply_result3 = call ptr (ptr, ...) @Apply(ptr %closure2, i64 1, ptr %apply_result)
    %boxed_int4 = call ptr @Create_int(i64 0)
    store ptr %boxed_int4, ptr @main.1, align 8
    ret i32 0
  }
  
  
  
  
  --------------- ../manytests/typed/003fib.ml
  ; ModuleID = 'Roflan'
  source_filename = "Roflan"
  
  @main.1 = global ptr null
  
  declare ptr @RoflanML_eq(ptr, ptr)
  
  declare ptr @RoflanML_neq(ptr, ptr)
  
  declare ptr @RoflanML_gt(ptr, ptr)
  
  declare ptr @RoflanML_ge(ptr, ptr)
  
  declare ptr @RoflanML_lt(ptr, ptr)
  
  declare ptr @RoflanML_le(ptr, ptr)
  
  declare ptr @RoflanML_or(ptr, ptr)
  
  declare ptr @RoflanML_and(ptr, ptr)
  
  declare ptr @RoflanML_add(ptr, ptr)
  
  declare ptr @RoflanML_sub(ptr, ptr)
  
  declare ptr @RoflanML_mul(ptr, ptr)
  
  declare ptr @RoflanML_div(ptr, ptr)
  
  declare ptr @Create_int(i64)
  
  declare ptr @Create_bool(i1)
  
  declare ptr @Create_unit()
  
  declare ptr @Apply(ptr, ...)
  
  declare i64 @Get_int(ptr)
  
  declare i1 @Get_bool(ptr)
  
  declare ptr @print_int(ptr)
  
  declare ptr @print_bool(ptr)
  
  declare ptr @Create_closure(ptr, i64)
  
  define ptr @fib_acc(ptr %a, ptr %b, ptr %n) {
  entry:
    %closure = call ptr @Create_closure(ptr @RoflanML_eq, i64 2)
    %boxed_int = call ptr @Create_int(i64 1)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 2, ptr %n, ptr %boxed_int)
    %cond_bool = call i1 @Get_bool(ptr %apply_result)
    br i1 %cond_bool, label %then, label %else
  
  then:                                             ; preds = %entry
    br label %merge
  
  else:                                             ; preds = %entry
    %closure1 = call ptr @Create_closure(ptr @RoflanML_sub, i64 2)
    %boxed_int2 = call ptr @Create_int(i64 1)
    %apply_result3 = call ptr (ptr, ...) @Apply(ptr %closure1, i64 2, ptr %n, ptr %boxed_int2)
    %closure4 = call ptr @Create_closure(ptr @RoflanML_add, i64 2)
    %apply_result5 = call ptr (ptr, ...) @Apply(ptr %closure4, i64 2, ptr %a, ptr %b)
    %closure6 = call ptr @Create_closure(ptr @fib_acc, i64 3)
    %apply_result7 = call ptr (ptr, ...) @Apply(ptr %closure6, i64 3, ptr %b, ptr %apply_result5, ptr %apply_result3)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %branch_result = phi ptr [ %b, %then ], [ %apply_result7, %else ]
    ret ptr %branch_result
  }
  
  define ptr @fib(ptr %n) {
  entry:
    %closure = call ptr @Create_closure(ptr @RoflanML_lt, i64 2)
    %boxed_int = call ptr @Create_int(i64 2)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 2, ptr %n, ptr %boxed_int)
    %cond_bool = call i1 @Get_bool(ptr %apply_result)
    br i1 %cond_bool, label %then, label %else
  
  then:                                             ; preds = %entry
    br label %merge
  
  else:                                             ; preds = %entry
    %closure1 = call ptr @Create_closure(ptr @RoflanML_sub, i64 2)
    %boxed_int2 = call ptr @Create_int(i64 1)
    %apply_result3 = call ptr (ptr, ...) @Apply(ptr %closure1, i64 2, ptr %n, ptr %boxed_int2)
    %closure4 = call ptr @Create_closure(ptr @fib, i64 1)
    %apply_result5 = call ptr (ptr, ...) @Apply(ptr %closure4, i64 1, ptr %apply_result3)
    %closure6 = call ptr @Create_closure(ptr @RoflanML_sub, i64 2)
    %boxed_int7 = call ptr @Create_int(i64 2)
    %apply_result8 = call ptr (ptr, ...) @Apply(ptr %closure6, i64 2, ptr %n, ptr %boxed_int7)
    %closure9 = call ptr @Create_closure(ptr @fib, i64 1)
    %apply_result10 = call ptr (ptr, ...) @Apply(ptr %closure9, i64 1, ptr %apply_result8)
    %closure11 = call ptr @Create_closure(ptr @RoflanML_add, i64 2)
    %apply_result12 = call ptr (ptr, ...) @Apply(ptr %closure11, i64 2, ptr %apply_result5, ptr %apply_result10)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %branch_result = phi ptr [ %n, %then ], [ %apply_result12, %else ]
    ret ptr %branch_result
  }
  
  define i32 @main() {
  entry:
    %closure = call ptr @Create_closure(ptr @fib_acc, i64 3)
    %boxed_int = call ptr @Create_int(i64 0)
    %boxed_int1 = call ptr @Create_int(i64 1)
    %boxed_int2 = call ptr @Create_int(i64 4)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 3, ptr %boxed_int, ptr %boxed_int1, ptr %boxed_int2)
    %closure3 = call ptr @Create_closure(ptr @print_int, i64 1)
    %apply_result4 = call ptr (ptr, ...) @Apply(ptr %closure3, i64 1, ptr %apply_result)
    %closure5 = call ptr @Create_closure(ptr @fib, i64 1)
    %boxed_int6 = call ptr @Create_int(i64 4)
    %apply_result7 = call ptr (ptr, ...) @Apply(ptr %closure5, i64 1, ptr %boxed_int6)
    %closure8 = call ptr @Create_closure(ptr @print_int, i64 1)
    %apply_result9 = call ptr (ptr, ...) @Apply(ptr %closure8, i64 1, ptr %apply_result7)
    %boxed_int10 = call ptr @Create_int(i64 0)
    store ptr %boxed_int10, ptr @main.1, align 8
    ret i32 0
  }
  
  
  
  
  --------------- ../manytests/typed/004manyargs.ml
  ; ModuleID = 'Roflan'
  source_filename = "Roflan"
  
  @main.1 = global ptr null
  
  declare ptr @RoflanML_eq(ptr, ptr)
  
  declare ptr @RoflanML_neq(ptr, ptr)
  
  declare ptr @RoflanML_gt(ptr, ptr)
  
  declare ptr @RoflanML_ge(ptr, ptr)
  
  declare ptr @RoflanML_lt(ptr, ptr)
  
  declare ptr @RoflanML_le(ptr, ptr)
  
  declare ptr @RoflanML_or(ptr, ptr)
  
  declare ptr @RoflanML_and(ptr, ptr)
  
  declare ptr @RoflanML_add(ptr, ptr)
  
  declare ptr @RoflanML_sub(ptr, ptr)
  
  declare ptr @RoflanML_mul(ptr, ptr)
  
  declare ptr @RoflanML_div(ptr, ptr)
  
  declare ptr @Create_int(i64)
  
  declare ptr @Create_bool(i1)
  
  declare ptr @Create_unit()
  
  declare ptr @Apply(ptr, ...)
  
  declare i64 @Get_int(ptr)
  
  declare i1 @Get_bool(ptr)
  
  declare ptr @print_int(ptr)
  
  declare ptr @print_bool(ptr)
  
  declare ptr @Create_closure(ptr, i64)
  
  define ptr @wrap(ptr %f) {
  entry:
    %closure = call ptr @Create_closure(ptr @RoflanML_eq, i64 2)
    %boxed_int = call ptr @Create_int(i64 1)
    %boxed_int1 = call ptr @Create_int(i64 1)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 2, ptr %boxed_int, ptr %boxed_int1)
    %cond_bool = call i1 @Get_bool(ptr %apply_result)
    br i1 %cond_bool, label %then, label %else
  
  then:                                             ; preds = %entry
    br label %merge
  
  else:                                             ; preds = %entry
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %branch_result = phi ptr [ %f, %then ], [ %f, %else ]
    ret ptr %branch_result
  }
  
  define ptr @test3(ptr %a, ptr %b, ptr %c) {
  entry:
    %closure = call ptr @Create_closure(ptr @print_int, i64 1)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 1, ptr %a)
    %closure1 = call ptr @Create_closure(ptr @print_int, i64 1)
    %apply_result2 = call ptr (ptr, ...) @Apply(ptr %closure1, i64 1, ptr %b)
    %closure3 = call ptr @Create_closure(ptr @print_int, i64 1)
    %apply_result4 = call ptr (ptr, ...) @Apply(ptr %closure3, i64 1, ptr %c)
    %boxed_int = call ptr @Create_int(i64 0)
    ret ptr %boxed_int
  }
  
  define ptr @test10(ptr %a, ptr %b, ptr %c, ptr %d, ptr %e, ptr %f, ptr %g, ptr %h, ptr %i, ptr %j) {
  entry:
    %closure = call ptr @Create_closure(ptr @RoflanML_add, i64 2)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 2, ptr %a, ptr %b)
    %closure1 = call ptr @Create_closure(ptr @RoflanML_add, i64 2)
    %apply_result2 = call ptr (ptr, ...) @Apply(ptr %closure1, i64 2, ptr %apply_result, ptr %c)
    %closure3 = call ptr @Create_closure(ptr @RoflanML_add, i64 2)
    %apply_result4 = call ptr (ptr, ...) @Apply(ptr %closure3, i64 2, ptr %apply_result2, ptr %d)
    %closure5 = call ptr @Create_closure(ptr @RoflanML_add, i64 2)
    %apply_result6 = call ptr (ptr, ...) @Apply(ptr %closure5, i64 2, ptr %apply_result4, ptr %e)
    %closure7 = call ptr @Create_closure(ptr @RoflanML_add, i64 2)
    %apply_result8 = call ptr (ptr, ...) @Apply(ptr %closure7, i64 2, ptr %apply_result6, ptr %f)
    %closure9 = call ptr @Create_closure(ptr @RoflanML_add, i64 2)
    %apply_result10 = call ptr (ptr, ...) @Apply(ptr %closure9, i64 2, ptr %apply_result8, ptr %g)
    %closure11 = call ptr @Create_closure(ptr @RoflanML_add, i64 2)
    %apply_result12 = call ptr (ptr, ...) @Apply(ptr %closure11, i64 2, ptr %apply_result10, ptr %h)
    %closure13 = call ptr @Create_closure(ptr @RoflanML_add, i64 2)
    %apply_result14 = call ptr (ptr, ...) @Apply(ptr %closure13, i64 2, ptr %apply_result12, ptr %i)
    %closure15 = call ptr @Create_closure(ptr @RoflanML_add, i64 2)
    %apply_result16 = call ptr (ptr, ...) @Apply(ptr %closure15, i64 2, ptr %apply_result14, ptr %j)
    ret ptr %apply_result16
  }
  
  define i32 @main() {
  entry:
    %closure = call ptr @Create_closure(ptr @wrap, i64 1)
    %closure1 = call ptr @Create_closure(ptr @test10, i64 10)
    %boxed_int = call ptr @Create_int(i64 1)
    %boxed_int2 = call ptr @Create_int(i64 10)
    %boxed_int3 = call ptr @Create_int(i64 100)
    %boxed_int4 = call ptr @Create_int(i64 1000)
    %boxed_int5 = call ptr @Create_int(i64 10000)
    %boxed_int6 = call ptr @Create_int(i64 100000)
    %boxed_int7 = call ptr @Create_int(i64 1000000)
    %boxed_int8 = call ptr @Create_int(i64 10000000)
    %boxed_int9 = call ptr @Create_int(i64 100000000)
    %boxed_int10 = call ptr @Create_int(i64 1000000000)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 11, ptr %closure1, ptr %boxed_int, ptr %boxed_int2, ptr %boxed_int3, ptr %boxed_int4, ptr %boxed_int5, ptr %boxed_int6, ptr %boxed_int7, ptr %boxed_int8, ptr %boxed_int9, ptr %boxed_int10)
    %closure11 = call ptr @Create_closure(ptr @print_int, i64 1)
    %apply_result12 = call ptr (ptr, ...) @Apply(ptr %closure11, i64 1, ptr %apply_result)
    %closure13 = call ptr @Create_closure(ptr @wrap, i64 1)
    %closure14 = call ptr @Create_closure(ptr @test3, i64 3)
    %boxed_int15 = call ptr @Create_int(i64 1)
    %boxed_int16 = call ptr @Create_int(i64 10)
    %boxed_int17 = call ptr @Create_int(i64 100)
    %apply_result18 = call ptr (ptr, ...) @Apply(ptr %closure13, i64 4, ptr %closure14, ptr %boxed_int15, ptr %boxed_int16, ptr %boxed_int17)
    %boxed_int19 = call ptr @Create_int(i64 0)
    store ptr %boxed_int19, ptr @main.1, align 8
    ret i32 0
  }
  
  
  
  
  --------------- ../manytests/typed/005fix.ml
  ; ModuleID = 'Roflan'
  source_filename = "Roflan"
  
  @main.1 = global ptr null
  
  declare ptr @RoflanML_eq(ptr, ptr)
  
  declare ptr @RoflanML_neq(ptr, ptr)
  
  declare ptr @RoflanML_gt(ptr, ptr)
  
  declare ptr @RoflanML_ge(ptr, ptr)
  
  declare ptr @RoflanML_lt(ptr, ptr)
  
  declare ptr @RoflanML_le(ptr, ptr)
  
  declare ptr @RoflanML_or(ptr, ptr)
  
  declare ptr @RoflanML_and(ptr, ptr)
  
  declare ptr @RoflanML_add(ptr, ptr)
  
  declare ptr @RoflanML_sub(ptr, ptr)
  
  declare ptr @RoflanML_mul(ptr, ptr)
  
  declare ptr @RoflanML_div(ptr, ptr)
  
  declare ptr @Create_int(i64)
  
  declare ptr @Create_bool(i1)
  
  declare ptr @Create_unit()
  
  declare ptr @Apply(ptr, ...)
  
  declare i64 @Get_int(ptr)
  
  declare i1 @Get_bool(ptr)
  
  declare ptr @print_int(ptr)
  
  declare ptr @print_bool(ptr)
  
  declare ptr @Create_closure(ptr, i64)
  
  define ptr @fix(ptr %f, ptr %x) {
  entry:
    %closure = call ptr @Create_closure(ptr @fix, i64 2)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 1, ptr %f)
    %apply_result1 = call ptr (ptr, ...) @Apply(ptr %f, i64 2, ptr %apply_result, ptr %x)
    ret ptr %apply_result1
  }
  
  define ptr @fac(ptr %self, ptr %n) {
  entry:
    %closure = call ptr @Create_closure(ptr @RoflanML_le, i64 2)
    %boxed_int = call ptr @Create_int(i64 1)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 2, ptr %n, ptr %boxed_int)
    %cond_bool = call i1 @Get_bool(ptr %apply_result)
    br i1 %cond_bool, label %then, label %else
  
  then:                                             ; preds = %entry
    %boxed_int1 = call ptr @Create_int(i64 1)
    br label %merge
  
  else:                                             ; preds = %entry
    %closure2 = call ptr @Create_closure(ptr @RoflanML_sub, i64 2)
    %boxed_int3 = call ptr @Create_int(i64 1)
    %apply_result4 = call ptr (ptr, ...) @Apply(ptr %closure2, i64 2, ptr %n, ptr %boxed_int3)
    %apply_result5 = call ptr (ptr, ...) @Apply(ptr %self, i64 1, ptr %apply_result4)
    %closure6 = call ptr @Create_closure(ptr @RoflanML_mul, i64 2)
    %apply_result7 = call ptr (ptr, ...) @Apply(ptr %closure6, i64 2, ptr %n, ptr %apply_result5)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %branch_result = phi ptr [ %boxed_int1, %then ], [ %apply_result7, %else ]
    ret ptr %branch_result
  }
  
  define i32 @main() {
  entry:
    %closure = call ptr @Create_closure(ptr @fix, i64 2)
    %closure1 = call ptr @Create_closure(ptr @fac, i64 2)
    %boxed_int = call ptr @Create_int(i64 6)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 2, ptr %closure1, ptr %boxed_int)
    %closure2 = call ptr @Create_closure(ptr @print_int, i64 1)
    %apply_result3 = call ptr (ptr, ...) @Apply(ptr %closure2, i64 1, ptr %apply_result)
    %boxed_int4 = call ptr @Create_int(i64 0)
    store ptr %boxed_int4, ptr @main.1, align 8
    ret i32 0
  }
  
  
  
  
  --------------- ../manytests/typed/006partial.ml
  ; ModuleID = 'Roflan'
  source_filename = "Roflan"
  
  @main.1 = global ptr null
  
  declare ptr @RoflanML_eq(ptr, ptr)
  
  declare ptr @RoflanML_neq(ptr, ptr)
  
  declare ptr @RoflanML_gt(ptr, ptr)
  
  declare ptr @RoflanML_ge(ptr, ptr)
  
  declare ptr @RoflanML_lt(ptr, ptr)
  
  declare ptr @RoflanML_le(ptr, ptr)
  
  declare ptr @RoflanML_or(ptr, ptr)
  
  declare ptr @RoflanML_and(ptr, ptr)
  
  declare ptr @RoflanML_add(ptr, ptr)
  
  declare ptr @RoflanML_sub(ptr, ptr)
  
  declare ptr @RoflanML_mul(ptr, ptr)
  
  declare ptr @RoflanML_div(ptr, ptr)
  
  declare ptr @Create_int(i64)
  
  declare ptr @Create_bool(i1)
  
  declare ptr @Create_unit()
  
  declare ptr @Apply(ptr, ...)
  
  declare i64 @Get_int(ptr)
  
  declare i1 @Get_bool(ptr)
  
  declare ptr @print_int(ptr)
  
  declare ptr @print_bool(ptr)
  
  declare ptr @Create_closure(ptr, i64)
  
  define ptr @ll_0(ptr %foo) {
  entry:
    %closure = call ptr @Create_closure(ptr @RoflanML_add, i64 2)
    %boxed_int = call ptr @Create_int(i64 2)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 2, ptr %foo, ptr %boxed_int)
    ret ptr %apply_result
  }
  
  define ptr @ll_1(ptr %foo) {
  entry:
    %closure = call ptr @Create_closure(ptr @RoflanML_mul, i64 2)
    %boxed_int = call ptr @Create_int(i64 10)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 2, ptr %foo, ptr %boxed_int)
    ret ptr %apply_result
  }
  
  define ptr @foo(ptr %b) {
  entry:
    %cond_bool = call i1 @Get_bool(ptr %b)
    br i1 %cond_bool, label %then, label %else
  
  then:                                             ; preds = %entry
    %closure = call ptr @Create_closure(ptr @ll_0, i64 1)
    br label %merge
  
  else:                                             ; preds = %entry
    %closure1 = call ptr @Create_closure(ptr @ll_1, i64 1)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %branch_result = phi ptr [ %closure, %then ], [ %closure1, %else ]
    ret ptr %branch_result
  }
  
  define ptr @alpha_0(ptr %x) {
  entry:
    %closure = call ptr @Create_closure(ptr @foo, i64 1)
    %boxed_bool = call ptr @Create_bool(i1 false)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 2, ptr %boxed_bool, ptr %x)
    %closure1 = call ptr @Create_closure(ptr @foo, i64 1)
    %boxed_bool2 = call ptr @Create_bool(i1 true)
    %apply_result3 = call ptr (ptr, ...) @Apply(ptr %closure1, i64 2, ptr %boxed_bool2, ptr %apply_result)
    %closure4 = call ptr @Create_closure(ptr @foo, i64 1)
    %boxed_bool5 = call ptr @Create_bool(i1 false)
    %apply_result6 = call ptr (ptr, ...) @Apply(ptr %closure4, i64 2, ptr %boxed_bool5, ptr %apply_result3)
    %closure7 = call ptr @Create_closure(ptr @foo, i64 1)
    %boxed_bool8 = call ptr @Create_bool(i1 true)
    %apply_result9 = call ptr (ptr, ...) @Apply(ptr %closure7, i64 2, ptr %boxed_bool8, ptr %apply_result6)
    ret ptr %apply_result9
  }
  
  define i32 @main() {
  entry:
    %closure = call ptr @Create_closure(ptr @alpha_0, i64 1)
    %boxed_int = call ptr @Create_int(i64 11)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 1, ptr %boxed_int)
    %closure1 = call ptr @Create_closure(ptr @print_int, i64 1)
    %apply_result2 = call ptr (ptr, ...) @Apply(ptr %closure1, i64 1, ptr %apply_result)
    %boxed_int3 = call ptr @Create_int(i64 0)
    store ptr %boxed_int3, ptr @main.1, align 8
    ret i32 0
  }
  
  
  
  
  --------------- ../manytests/typed/006partial2.ml
  ; ModuleID = 'Roflan'
  source_filename = "Roflan"
  
  @main.1 = global ptr null
  
  declare ptr @RoflanML_eq(ptr, ptr)
  
  declare ptr @RoflanML_neq(ptr, ptr)
  
  declare ptr @RoflanML_gt(ptr, ptr)
  
  declare ptr @RoflanML_ge(ptr, ptr)
  
  declare ptr @RoflanML_lt(ptr, ptr)
  
  declare ptr @RoflanML_le(ptr, ptr)
  
  declare ptr @RoflanML_or(ptr, ptr)
  
  declare ptr @RoflanML_and(ptr, ptr)
  
  declare ptr @RoflanML_add(ptr, ptr)
  
  declare ptr @RoflanML_sub(ptr, ptr)
  
  declare ptr @RoflanML_mul(ptr, ptr)
  
  declare ptr @RoflanML_div(ptr, ptr)
  
  declare ptr @Create_int(i64)
  
  declare ptr @Create_bool(i1)
  
  declare ptr @Create_unit()
  
  declare ptr @Apply(ptr, ...)
  
  declare i64 @Get_int(ptr)
  
  declare i1 @Get_bool(ptr)
  
  declare ptr @print_int(ptr)
  
  declare ptr @print_bool(ptr)
  
  declare ptr @Create_closure(ptr, i64)
  
  define ptr @foo(ptr %a, ptr %b, ptr %c) {
  entry:
    %closure = call ptr @Create_closure(ptr @print_int, i64 1)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 1, ptr %a)
    %closure1 = call ptr @Create_closure(ptr @print_int, i64 1)
    %apply_result2 = call ptr (ptr, ...) @Apply(ptr %closure1, i64 1, ptr %b)
    %closure3 = call ptr @Create_closure(ptr @print_int, i64 1)
    %apply_result4 = call ptr (ptr, ...) @Apply(ptr %closure3, i64 1, ptr %c)
    %closure5 = call ptr @Create_closure(ptr @RoflanML_mul, i64 2)
    %apply_result6 = call ptr (ptr, ...) @Apply(ptr %closure5, i64 2, ptr %b, ptr %c)
    %closure7 = call ptr @Create_closure(ptr @RoflanML_add, i64 2)
    %apply_result8 = call ptr (ptr, ...) @Apply(ptr %closure7, i64 2, ptr %a, ptr %apply_result6)
    ret ptr %apply_result8
  }
  
  define i32 @main() {
  entry:
    %closure = call ptr @Create_closure(ptr @foo, i64 3)
    %boxed_int = call ptr @Create_int(i64 1)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 1, ptr %boxed_int)
    %boxed_int1 = call ptr @Create_int(i64 2)
    %apply_result2 = call ptr (ptr, ...) @Apply(ptr %apply_result, i64 1, ptr %boxed_int1)
    %boxed_int3 = call ptr @Create_int(i64 3)
    %apply_result4 = call ptr (ptr, ...) @Apply(ptr %apply_result2, i64 1, ptr %boxed_int3)
    %closure5 = call ptr @Create_closure(ptr @print_int, i64 1)
    %apply_result6 = call ptr (ptr, ...) @Apply(ptr %closure5, i64 1, ptr %apply_result4)
    %boxed_int7 = call ptr @Create_int(i64 0)
    store ptr %boxed_int7, ptr @main.1, align 8
    ret i32 0
  }
  
  
  
  
  --------------- ../manytests/typed/006partial3.ml
  ; ModuleID = 'Roflan'
  source_filename = "Roflan"
  
  @main.1 = global ptr null
  
  declare ptr @RoflanML_eq(ptr, ptr)
  
  declare ptr @RoflanML_neq(ptr, ptr)
  
  declare ptr @RoflanML_gt(ptr, ptr)
  
  declare ptr @RoflanML_ge(ptr, ptr)
  
  declare ptr @RoflanML_lt(ptr, ptr)
  
  declare ptr @RoflanML_le(ptr, ptr)
  
  declare ptr @RoflanML_or(ptr, ptr)
  
  declare ptr @RoflanML_and(ptr, ptr)
  
  declare ptr @RoflanML_add(ptr, ptr)
  
  declare ptr @RoflanML_sub(ptr, ptr)
  
  declare ptr @RoflanML_mul(ptr, ptr)
  
  declare ptr @RoflanML_div(ptr, ptr)
  
  declare ptr @Create_int(i64)
  
  declare ptr @Create_bool(i1)
  
  declare ptr @Create_unit()
  
  declare ptr @Apply(ptr, ...)
  
  declare i64 @Get_int(ptr)
  
  declare i1 @Get_bool(ptr)
  
  declare ptr @print_int(ptr)
  
  declare ptr @print_bool(ptr)
  
  declare ptr @Create_closure(ptr, i64)
  
  define ptr @ll_1(ptr %c) {
  entry:
    %closure = call ptr @Create_closure(ptr @print_int, i64 1)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 1, ptr %c)
    ret ptr %apply_result
  }
  
  define ptr @ll_0(ptr %b) {
  entry:
    %closure = call ptr @Create_closure(ptr @print_int, i64 1)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 1, ptr %b)
    %closure1 = call ptr @Create_closure(ptr @ll_1, i64 1)
    ret ptr %closure1
  }
  
  define ptr @foo(ptr %a) {
  entry:
    %closure = call ptr @Create_closure(ptr @print_int, i64 1)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 1, ptr %a)
    %closure1 = call ptr @Create_closure(ptr @ll_0, i64 1)
    ret ptr %closure1
  }
  
  define i32 @main() {
  entry:
    %closure = call ptr @Create_closure(ptr @foo, i64 1)
    %boxed_int = call ptr @Create_int(i64 4)
    %boxed_int1 = call ptr @Create_int(i64 8)
    %boxed_int2 = call ptr @Create_int(i64 9)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 3, ptr %boxed_int, ptr %boxed_int1, ptr %boxed_int2)
    %boxed_int3 = call ptr @Create_int(i64 0)
    store ptr %boxed_int3, ptr @main.1, align 8
    ret i32 0
  }
  
  
  
  $ for prog in ../compiler/tests/*; do
  > echo "\n--------------- $prog"
  > dune exec ./demoLlVM.exe < $prog
  > echo "\n\n"
  > done
  
  --------------- ../compiler/tests/017op.ml
  ; ModuleID = 'Roflan'
  source_filename = "Roflan"
  
  @main.1 = global ptr null
  
  declare ptr @RoflanML_eq(ptr, ptr)
  
  declare ptr @RoflanML_neq(ptr, ptr)
  
  declare ptr @RoflanML_gt(ptr, ptr)
  
  declare ptr @RoflanML_ge(ptr, ptr)
  
  declare ptr @RoflanML_lt(ptr, ptr)
  
  declare ptr @RoflanML_le(ptr, ptr)
  
  declare ptr @RoflanML_or(ptr, ptr)
  
  declare ptr @RoflanML_and(ptr, ptr)
  
  declare ptr @RoflanML_add(ptr, ptr)
  
  declare ptr @RoflanML_sub(ptr, ptr)
  
  declare ptr @RoflanML_mul(ptr, ptr)
  
  declare ptr @RoflanML_div(ptr, ptr)
  
  declare ptr @Create_int(i64)
  
  declare ptr @Create_bool(i1)
  
  declare ptr @Create_unit()
  
  declare ptr @Apply(ptr, ...)
  
  declare i64 @Get_int(ptr)
  
  declare i1 @Get_bool(ptr)
  
  declare ptr @print_int(ptr)
  
  declare ptr @print_bool(ptr)
  
  declare ptr @Create_closure(ptr, i64)
  
  define ptr @alpha_0(ptr %x, ptr %y, ptr %z) {
  entry:
    %closure = call ptr @Create_closure(ptr @RoflanML_add, i64 2)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 2, ptr %x, ptr %y)
    %closure1 = call ptr @Create_closure(ptr @RoflanML_sub, i64 2)
    %apply_result2 = call ptr (ptr, ...) @Apply(ptr %closure1, i64 2, ptr %apply_result, ptr %z)
    ret ptr %apply_result2
  }
  
  define ptr @alpha_1(ptr %x, ptr %y) {
  entry:
    %closure = call ptr @Create_closure(ptr @RoflanML_mul, i64 2)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 2, ptr %x, ptr %y)
    ret ptr %apply_result
  }
  
  define ptr @alpha_2(ptr %x, ptr %y) {
  entry:
    %closure = call ptr @Create_closure(ptr @alpha_0, i64 3)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 2, ptr %x, ptr %y)
    ret ptr %apply_result
  }
  
  define i32 @main() {
  entry:
    %closure = call ptr @Create_closure(ptr @alpha_0, i64 3)
    %boxed_int = call ptr @Create_int(i64 1)
    %boxed_int1 = call ptr @Create_int(i64 2)
    %boxed_int2 = call ptr @Create_int(i64 3)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 3, ptr %boxed_int, ptr %boxed_int1, ptr %boxed_int2)
    %closure3 = call ptr @Create_closure(ptr @print_int, i64 1)
    %apply_result4 = call ptr (ptr, ...) @Apply(ptr %closure3, i64 1, ptr %apply_result)
    %closure5 = call ptr @Create_closure(ptr @alpha_1, i64 2)
    %boxed_int6 = call ptr @Create_int(i64 1)
    %boxed_int7 = call ptr @Create_int(i64 2)
    %apply_result8 = call ptr (ptr, ...) @Apply(ptr %closure5, i64 2, ptr %boxed_int6, ptr %boxed_int7)
    %closure9 = call ptr @Create_closure(ptr @print_int, i64 1)
    %apply_result10 = call ptr (ptr, ...) @Apply(ptr %closure9, i64 1, ptr %apply_result8)
    %closure11 = call ptr @Create_closure(ptr @alpha_2, i64 2)
    %boxed_int12 = call ptr @Create_int(i64 2)
    %boxed_int13 = call ptr @Create_int(i64 3)
    %boxed_int14 = call ptr @Create_int(i64 4)
    %apply_result15 = call ptr (ptr, ...) @Apply(ptr %closure11, i64 3, ptr %boxed_int12, ptr %boxed_int13, ptr %boxed_int14)
    %closure16 = call ptr @Create_closure(ptr @print_int, i64 1)
    %apply_result17 = call ptr (ptr, ...) @Apply(ptr %closure16, i64 1, ptr %apply_result15)
    %boxed_int18 = call ptr @Create_int(i64 0)
    store ptr %boxed_int18, ptr @main.1, align 8
    ret i32 0
  }
  
  
  
  
  --------------- ../compiler/tests/018print.ml
  ; ModuleID = 'Roflan'
  source_filename = "Roflan"
  
  declare ptr @RoflanML_eq(ptr, ptr)
  
  declare ptr @RoflanML_neq(ptr, ptr)
  
  declare ptr @RoflanML_gt(ptr, ptr)
  
  declare ptr @RoflanML_ge(ptr, ptr)
  
  declare ptr @RoflanML_lt(ptr, ptr)
  
  declare ptr @RoflanML_le(ptr, ptr)
  
  declare ptr @RoflanML_or(ptr, ptr)
  
  declare ptr @RoflanML_and(ptr, ptr)
  
  declare ptr @RoflanML_add(ptr, ptr)
  
  declare ptr @RoflanML_sub(ptr, ptr)
  
  declare ptr @RoflanML_mul(ptr, ptr)
  
  declare ptr @RoflanML_div(ptr, ptr)
  
  declare ptr @Create_int(i64)
  
  declare ptr @Create_bool(i1)
  
  declare ptr @Create_unit()
  
  declare ptr @Apply(ptr, ...)
  
  declare i64 @Get_int(ptr)
  
  declare i1 @Get_bool(ptr)
  
  declare ptr @print_int(ptr)
  
  declare ptr @print_bool(ptr)
  
  declare ptr @Create_closure(ptr, i64)
  
  define ptr @alpha_0(ptr %x) {
  entry:
    %closure = call ptr @Create_closure(ptr @print_bool, i64 1)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 1, ptr %x)
    ret ptr %apply_result
  }
  
  define i32 @main() {
  entry:
    %closure = call ptr @Create_closure(ptr @RoflanML_and, i64 2)
    %boxed_bool = call ptr @Create_bool(i1 true)
    %boxed_bool1 = call ptr @Create_bool(i1 false)
    %apply_result = call ptr (ptr, ...) @Apply(ptr %closure, i64 2, ptr %boxed_bool, ptr %boxed_bool1)
    %closure2 = call ptr @Create_closure(ptr @alpha_0, i64 1)
    %apply_result3 = call ptr (ptr, ...) @Apply(ptr %closure2, i64 1, ptr %apply_result)
    ret i32 0
  }
  
  
  
