  $ ./codegen_runner.exe < manytests/typed/001fac.ml
  ; ModuleID = 'aefjnv_ml'
  source_filename = "aefjnv_ml"
  
  @cc_ac0_main = global i64 0
  
  define i64 @main() {
  entry:
    %0 = call i64 @fac(i64 9)
    %1 = call i64 @rt_print_int(i64 %0)
    store i64 1, ptr @cc_ac0_main, align 4
    ret i64 0
  }
  
  declare i64 @rt_get_by_idx(i64, i64)
  
  declare i64 @rt_get_list_tail(i64, i64)
  
  declare i64 @rt_get_list_len(i64)
  
  declare i64 @rt_fail(i64)
  
  declare i64 @rt_alloc_closure(i64, i64)
  
  declare i64 @rt_apply_to_closure(i64, i64, ...)
  
  declare i64 @rt_append_to_list(i64, i64, ...)
  
  declare i64 @rt_alloc_tuple(i64, ...)
  
  declare i64 @rt_unminus(i64)
  
  declare i64 @rt_mul(i64, i64)
  
  declare i64 @rt_div(i64, i64)
  
  declare i64 @rt_add(i64, i64)
  
  declare i64 @rt_sub(i64, i64)
  
  declare i64 @rt_leq(i64, i64)
  
  declare i64 @rt_less(i64, i64)
  
  declare i64 @rt_meq(i64, i64)
  
  declare i64 @rt_more(i64, i64)
  
  declare i64 @rt_eq2(i64, i64)
  
  declare i64 @rt_eq(i64, i64)
  
  declare i64 @rt_neq(i64, i64)
  
  declare i64 @rt_and(i64, i64)
  
  declare i64 @rt_or(i64, i64)
  
  declare i64 @rt_print_int(i64)
  
  define i64 @fac(i64 %0) {
  entry:
    %1 = call i64 @rt_leq(i64 %0, i64 3)
    %2 = ashr i64 %1, 1
    %3 = trunc i64 %2 to i1
    br i1 %3, label %5, label %6
  
  continuation:                                     ; preds = %6, %5
    %4 = phi i64 [ 3, %5 ], [ %9, %6 ]
    ret i64 %4
  
  5:                                                ; preds = %entry
    br label %continuation
  
  6:                                                ; preds = %entry
    %7 = call i64 @rt_sub(i64 %0, i64 3)
    %8 = call i64 @fac(i64 %7)
    %9 = call i64 @rt_mul(i64 %0, i64 %8)
    br label %continuation
  }

  $ ./codegen_runner.exe < manytests/typed/002fac.ml
  ; ModuleID = 'aefjnv_ml'
  source_filename = "aefjnv_ml"
  
  @cc_ac1_main = global i64 0
  
  define i64 @main() {
  entry:
    %0 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @ll_1 to i64), i64 3)
    %1 = call i64 @fac_cps(i64 9, i64 %0)
    %2 = call i64 @rt_print_int(i64 %1)
    store i64 1, ptr @cc_ac1_main, align 4
    ret i64 0
  }
  
  declare i64 @rt_get_by_idx(i64, i64)
  
  declare i64 @rt_get_list_tail(i64, i64)
  
  declare i64 @rt_get_list_len(i64)
  
  declare i64 @rt_fail(i64)
  
  declare i64 @rt_alloc_closure(i64, i64)
  
  declare i64 @rt_apply_to_closure(i64, i64, ...)
  
  declare i64 @rt_append_to_list(i64, i64, ...)
  
  declare i64 @rt_alloc_tuple(i64, ...)
  
  declare i64 @rt_unminus(i64)
  
  declare i64 @rt_mul(i64, i64)
  
  declare i64 @rt_div(i64, i64)
  
  declare i64 @rt_add(i64, i64)
  
  declare i64 @rt_sub(i64, i64)
  
  declare i64 @rt_leq(i64, i64)
  
  declare i64 @rt_less(i64, i64)
  
  declare i64 @rt_meq(i64, i64)
  
  declare i64 @rt_more(i64, i64)
  
  declare i64 @rt_eq2(i64, i64)
  
  declare i64 @rt_eq(i64, i64)
  
  declare i64 @rt_neq(i64, i64)
  
  declare i64 @rt_and(i64, i64)
  
  declare i64 @rt_or(i64, i64)
  
  declare i64 @rt_print_int(i64)
  
  define i64 @ll_0(i64 %0, i64 %1, i64 %2) {
  entry:
    %3 = call i64 @rt_mul(i64 %2, i64 %0)
    %4 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %1, i64 3, i64 %3)
    ret i64 %4
  }
  
  define i64 @fac_cps(i64 %0, i64 %1) {
  entry:
    %2 = call i64 @rt_eq(i64 %0, i64 3)
    %3 = ashr i64 %2, 1
    %4 = trunc i64 %3 to i1
    br i1 %4, label %6, label %8
  
  continuation:                                     ; preds = %8, %6
    %5 = phi i64 [ %7, %6 ], [ %12, %8 ]
    ret i64 %5
  
  6:                                                ; preds = %entry
    %7 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %1, i64 3, i64 3)
    br label %continuation
  
  8:                                                ; preds = %entry
    %9 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @ll_0 to i64), i64 7)
    %10 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %9, i64 5, i64 %0, i64 %1)
    %11 = call i64 @rt_sub(i64 %0, i64 3)
    %12 = call i64 @fac_cps(i64 %11, i64 %10)
    br label %continuation
  }
  
  define i64 @ll_1(i64 %0) {
  entry:
    ret i64 %0
  }

  $ ./codegen_runner.exe < manytests/typed/003fib.ml
  ; ModuleID = 'aefjnv_ml'
  source_filename = "aefjnv_ml"
  
  @cc_ac1_main = global i64 0
  
  define i64 @main() {
  entry:
    %0 = call i64 @fib_acc(i64 1, i64 3, i64 9)
    %1 = call i64 @rt_print_int(i64 %0)
    %2 = call i64 @fib(i64 9)
    %3 = call i64 @rt_print_int(i64 %2)
    store i64 1, ptr @cc_ac1_main, align 4
    ret i64 0
  }
  
  declare i64 @rt_get_by_idx(i64, i64)
  
  declare i64 @rt_get_list_tail(i64, i64)
  
  declare i64 @rt_get_list_len(i64)
  
  declare i64 @rt_fail(i64)
  
  declare i64 @rt_alloc_closure(i64, i64)
  
  declare i64 @rt_apply_to_closure(i64, i64, ...)
  
  declare i64 @rt_append_to_list(i64, i64, ...)
  
  declare i64 @rt_alloc_tuple(i64, ...)
  
  declare i64 @rt_unminus(i64)
  
  declare i64 @rt_mul(i64, i64)
  
  declare i64 @rt_div(i64, i64)
  
  declare i64 @rt_add(i64, i64)
  
  declare i64 @rt_sub(i64, i64)
  
  declare i64 @rt_leq(i64, i64)
  
  declare i64 @rt_less(i64, i64)
  
  declare i64 @rt_meq(i64, i64)
  
  declare i64 @rt_more(i64, i64)
  
  declare i64 @rt_eq2(i64, i64)
  
  declare i64 @rt_eq(i64, i64)
  
  declare i64 @rt_neq(i64, i64)
  
  declare i64 @rt_and(i64, i64)
  
  declare i64 @rt_or(i64, i64)
  
  declare i64 @rt_print_int(i64)
  
  define i64 @fib_acc(i64 %0, i64 %1, i64 %2) {
  entry:
    %3 = call i64 @rt_eq(i64 %2, i64 3)
    %4 = ashr i64 %3, 1
    %5 = trunc i64 %4 to i1
    br i1 %5, label %7, label %8
  
  continuation:                                     ; preds = %8, %7
    %6 = phi i64 [ %1, %7 ], [ %11, %8 ]
    ret i64 %6
  
  7:                                                ; preds = %entry
    br label %continuation
  
  8:                                                ; preds = %entry
    %9 = call i64 @rt_sub(i64 %2, i64 3)
    %10 = call i64 @rt_add(i64 %0, i64 %1)
    %11 = call i64 @fib_acc(i64 %1, i64 %10, i64 %9)
    br label %continuation
  }
  
  define i64 @fib(i64 %0) {
  entry:
    %1 = call i64 @rt_less(i64 %0, i64 5)
    %2 = ashr i64 %1, 1
    %3 = trunc i64 %2 to i1
    br i1 %3, label %5, label %6
  
  continuation:                                     ; preds = %6, %5
    %4 = phi i64 [ %0, %5 ], [ %11, %6 ]
    ret i64 %4
  
  5:                                                ; preds = %entry
    br label %continuation
  
  6:                                                ; preds = %entry
    %7 = call i64 @rt_sub(i64 %0, i64 5)
    %8 = call i64 @fib(i64 %7)
    %9 = call i64 @rt_sub(i64 %0, i64 3)
    %10 = call i64 @fib(i64 %9)
    %11 = call i64 @rt_add(i64 %10, i64 %8)
    br label %continuation
  }

  $ ./codegen_runner.exe < manytests/typed/004manyargs.ml
  ; ModuleID = 'aefjnv_ml'
  source_filename = "aefjnv_ml"
  
  @cc_ac7_main = global i64 0
  
  define i64 @main() {
  entry:
    %0 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @test10 to i64), i64 21)
    %1 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @wrap to i64), i64 3)
    %2 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %1, i64 23, i64 %0, i64 3, i64 21, i64 201, i64 2001, i64 20001, i64 200001, i64 2000001, i64 20000001, i64 200000001, i64 2000000001)
    %3 = call i64 @rt_print_int(i64 %2)
    %4 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @test3 to i64), i64 7)
    %5 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @wrap to i64), i64 3)
    %6 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %5, i64 9, i64 %4, i64 3, i64 21, i64 201)
    store i64 1, ptr @cc_ac7_main, align 4
    ret i64 0
  }
  
  declare i64 @rt_get_by_idx(i64, i64)
  
  declare i64 @rt_get_list_tail(i64, i64)
  
  declare i64 @rt_get_list_len(i64)
  
  declare i64 @rt_fail(i64)
  
  declare i64 @rt_alloc_closure(i64, i64)
  
  declare i64 @rt_apply_to_closure(i64, i64, ...)
  
  declare i64 @rt_append_to_list(i64, i64, ...)
  
  declare i64 @rt_alloc_tuple(i64, ...)
  
  declare i64 @rt_unminus(i64)
  
  declare i64 @rt_mul(i64, i64)
  
  declare i64 @rt_div(i64, i64)
  
  declare i64 @rt_add(i64, i64)
  
  declare i64 @rt_sub(i64, i64)
  
  declare i64 @rt_leq(i64, i64)
  
  declare i64 @rt_less(i64, i64)
  
  declare i64 @rt_meq(i64, i64)
  
  declare i64 @rt_more(i64, i64)
  
  declare i64 @rt_eq2(i64, i64)
  
  declare i64 @rt_eq(i64, i64)
  
  declare i64 @rt_neq(i64, i64)
  
  declare i64 @rt_and(i64, i64)
  
  declare i64 @rt_or(i64, i64)
  
  declare i64 @rt_print_int(i64)
  
  define i64 @wrap(i64 %0) {
  entry:
    %1 = call i64 @rt_eq(i64 3, i64 3)
    %2 = ashr i64 %1, 1
    %3 = trunc i64 %2 to i1
    br i1 %3, label %5, label %6
  
  continuation:                                     ; preds = %6, %5
    %4 = phi i64 [ %0, %5 ], [ %0, %6 ]
    ret i64 %4
  
  5:                                                ; preds = %entry
    br label %continuation
  
  6:                                                ; preds = %entry
    br label %continuation
  }
  
  define i64 @test3(i64 %0, i64 %1, i64 %2) {
  entry:
    %3 = call i64 @rt_print_int(i64 %0)
    %4 = call i64 @rt_print_int(i64 %1)
    %5 = call i64 @rt_print_int(i64 %2)
    ret i64 1
  }
  
  define i64 @test10(i64 %0, i64 %1, i64 %2, i64 %3, i64 %4, i64 %5, i64 %6, i64 %7, i64 %8, i64 %9) {
  entry:
    %10 = call i64 @rt_add(i64 %0, i64 %1)
    %11 = call i64 @rt_add(i64 %10, i64 %2)
    %12 = call i64 @rt_add(i64 %11, i64 %3)
    %13 = call i64 @rt_add(i64 %12, i64 %4)
    %14 = call i64 @rt_add(i64 %13, i64 %5)
    %15 = call i64 @rt_add(i64 %14, i64 %6)
    %16 = call i64 @rt_add(i64 %15, i64 %7)
    %17 = call i64 @rt_add(i64 %16, i64 %8)
    %18 = call i64 @rt_add(i64 %17, i64 %9)
    ret i64 %18
  }

  $ ./codegen_runner.exe < manytests/typed/005fix.ml
  ; ModuleID = 'aefjnv_ml'
  source_filename = "aefjnv_ml"
  
  @cc_ac0_main = global i64 0
  
  define i64 @main() {
  entry:
    %0 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @fac to i64), i64 5)
    %1 = call i64 @fix(i64 %0, i64 13)
    %2 = call i64 @rt_print_int(i64 %1)
    store i64 1, ptr @cc_ac0_main, align 4
    ret i64 0
  }
  
  declare i64 @rt_get_by_idx(i64, i64)
  
  declare i64 @rt_get_list_tail(i64, i64)
  
  declare i64 @rt_get_list_len(i64)
  
  declare i64 @rt_fail(i64)
  
  declare i64 @rt_alloc_closure(i64, i64)
  
  declare i64 @rt_apply_to_closure(i64, i64, ...)
  
  declare i64 @rt_append_to_list(i64, i64, ...)
  
  declare i64 @rt_alloc_tuple(i64, ...)
  
  declare i64 @rt_unminus(i64)
  
  declare i64 @rt_mul(i64, i64)
  
  declare i64 @rt_div(i64, i64)
  
  declare i64 @rt_add(i64, i64)
  
  declare i64 @rt_sub(i64, i64)
  
  declare i64 @rt_leq(i64, i64)
  
  declare i64 @rt_less(i64, i64)
  
  declare i64 @rt_meq(i64, i64)
  
  declare i64 @rt_more(i64, i64)
  
  declare i64 @rt_eq2(i64, i64)
  
  declare i64 @rt_eq(i64, i64)
  
  declare i64 @rt_neq(i64, i64)
  
  declare i64 @rt_and(i64, i64)
  
  declare i64 @rt_or(i64, i64)
  
  declare i64 @rt_print_int(i64)
  
  define i64 @fix(i64 %0, i64 %1) {
  entry:
    %2 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @fix to i64), i64 5)
    %3 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %2, i64 3, i64 %0)
    %4 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %0, i64 5, i64 %3, i64 %1)
    ret i64 %4
  }
  
  define i64 @fac(i64 %0, i64 %1) {
  entry:
    %2 = call i64 @rt_leq(i64 %1, i64 3)
    %3 = ashr i64 %2, 1
    %4 = trunc i64 %3 to i1
    br i1 %4, label %6, label %7
  
  continuation:                                     ; preds = %7, %6
    %5 = phi i64 [ 3, %6 ], [ %10, %7 ]
    ret i64 %5
  
  6:                                                ; preds = %entry
    br label %continuation
  
  7:                                                ; preds = %entry
    %8 = call i64 @rt_sub(i64 %1, i64 3)
    %9 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %0, i64 3, i64 %8)
    %10 = call i64 @rt_mul(i64 %1, i64 %9)
    br label %continuation
  }

  $ ./codegen_runner.exe < manytests/typed/006partial2.ml
  ; ModuleID = 'aefjnv_ml'
  source_filename = "aefjnv_ml"
  
  @cc_ac3_main = global i64 0
  
  define i64 @main() {
  entry:
    %0 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @foo to i64), i64 7)
    %1 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %0, i64 3, i64 3)
    %2 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %1, i64 3, i64 5)
    %3 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %2, i64 3, i64 7)
    %4 = call i64 @rt_print_int(i64 %3)
    store i64 1, ptr @cc_ac3_main, align 4
    ret i64 0
  }
  
  declare i64 @rt_get_by_idx(i64, i64)
  
  declare i64 @rt_get_list_tail(i64, i64)
  
  declare i64 @rt_get_list_len(i64)
  
  declare i64 @rt_fail(i64)
  
  declare i64 @rt_alloc_closure(i64, i64)
  
  declare i64 @rt_apply_to_closure(i64, i64, ...)
  
  declare i64 @rt_append_to_list(i64, i64, ...)
  
  declare i64 @rt_alloc_tuple(i64, ...)
  
  declare i64 @rt_unminus(i64)
  
  declare i64 @rt_mul(i64, i64)
  
  declare i64 @rt_div(i64, i64)
  
  declare i64 @rt_add(i64, i64)
  
  declare i64 @rt_sub(i64, i64)
  
  declare i64 @rt_leq(i64, i64)
  
  declare i64 @rt_less(i64, i64)
  
  declare i64 @rt_meq(i64, i64)
  
  declare i64 @rt_more(i64, i64)
  
  declare i64 @rt_eq2(i64, i64)
  
  declare i64 @rt_eq(i64, i64)
  
  declare i64 @rt_neq(i64, i64)
  
  declare i64 @rt_and(i64, i64)
  
  declare i64 @rt_or(i64, i64)
  
  declare i64 @rt_print_int(i64)
  
  define i64 @foo(i64 %0, i64 %1, i64 %2) {
  entry:
    %3 = call i64 @rt_print_int(i64 %0)
    %4 = call i64 @rt_print_int(i64 %1)
    %5 = call i64 @rt_print_int(i64 %2)
    %6 = call i64 @rt_mul(i64 %1, i64 %2)
    %7 = call i64 @rt_add(i64 %0, i64 %6)
    ret i64 %7
  }

  $ ./codegen_runner.exe < manytests/typed/006partial3.ml
  ; ModuleID = 'aefjnv_ml'
  source_filename = "aefjnv_ml"
  
  @cc_ac0_main = global i64 0
  
  define i64 @main() {
  entry:
    %0 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @foo to i64), i64 3)
    %1 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %0, i64 7, i64 9, i64 17, i64 19)
    store i64 1, ptr @cc_ac0_main, align 4
    ret i64 0
  }
  
  declare i64 @rt_get_by_idx(i64, i64)
  
  declare i64 @rt_get_list_tail(i64, i64)
  
  declare i64 @rt_get_list_len(i64)
  
  declare i64 @rt_fail(i64)
  
  declare i64 @rt_alloc_closure(i64, i64)
  
  declare i64 @rt_apply_to_closure(i64, i64, ...)
  
  declare i64 @rt_append_to_list(i64, i64, ...)
  
  declare i64 @rt_alloc_tuple(i64, ...)
  
  declare i64 @rt_unminus(i64)
  
  declare i64 @rt_mul(i64, i64)
  
  declare i64 @rt_div(i64, i64)
  
  declare i64 @rt_add(i64, i64)
  
  declare i64 @rt_sub(i64, i64)
  
  declare i64 @rt_leq(i64, i64)
  
  declare i64 @rt_less(i64, i64)
  
  declare i64 @rt_meq(i64, i64)
  
  declare i64 @rt_more(i64, i64)
  
  declare i64 @rt_eq2(i64, i64)
  
  declare i64 @rt_eq(i64, i64)
  
  declare i64 @rt_neq(i64, i64)
  
  declare i64 @rt_and(i64, i64)
  
  declare i64 @rt_or(i64, i64)
  
  declare i64 @rt_print_int(i64)
  
  define i64 @ll_1(i64 %0) {
  entry:
    %1 = call i64 @rt_print_int(i64 %0)
    ret i64 %1
  }
  
  define i64 @ll_0(i64 %0) {
  entry:
    %1 = call i64 @rt_print_int(i64 %0)
    %2 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @ll_1 to i64), i64 3)
    ret i64 %2
  }
  
  define i64 @foo(i64 %0) {
  entry:
    %1 = call i64 @rt_print_int(i64 %0)
    %2 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @ll_0 to i64), i64 3)
    ret i64 %2
  }

  $ ./codegen_runner.exe < manytests/typed/007order.ml
  ; ModuleID = 'aefjnv_ml'
  source_filename = "aefjnv_ml"
  
  @cc_ac1_main = global i64 0
  
  define i64 @main() {
  entry:
    %0 = call i64 @rt_unminus(i64 1111111)
    %1 = call i64 @rt_unminus(i64 3)
    %2 = call i64 @rt_print_int(i64 %1)
    %3 = call i64 @rt_print_int(i64 9)
    %4 = call i64 @rt_print_int(i64 5)
    %5 = call i64 @rt_print_int(i64 3)
    %6 = call i64 @cc_ac0__start(i64 %5, i64 %4, i64 7, i64 %3, i64 201, i64 2001, i64 %2, i64 20001, i64 %0)
    %7 = call i64 @rt_print_int(i64 %6)
    store i64 %7, ptr @cc_ac1_main, align 4
    ret i64 0
  }
  
  declare i64 @rt_get_by_idx(i64, i64)
  
  declare i64 @rt_get_list_tail(i64, i64)
  
  declare i64 @rt_get_list_len(i64)
  
  declare i64 @rt_fail(i64)
  
  declare i64 @rt_alloc_closure(i64, i64)
  
  declare i64 @rt_apply_to_closure(i64, i64, ...)
  
  declare i64 @rt_append_to_list(i64, i64, ...)
  
  declare i64 @rt_alloc_tuple(i64, ...)
  
  declare i64 @rt_unminus(i64)
  
  declare i64 @rt_mul(i64, i64)
  
  declare i64 @rt_div(i64, i64)
  
  declare i64 @rt_add(i64, i64)
  
  declare i64 @rt_sub(i64, i64)
  
  declare i64 @rt_leq(i64, i64)
  
  declare i64 @rt_less(i64, i64)
  
  declare i64 @rt_meq(i64, i64)
  
  declare i64 @rt_more(i64, i64)
  
  declare i64 @rt_eq2(i64, i64)
  
  declare i64 @rt_eq(i64, i64)
  
  declare i64 @rt_neq(i64, i64)
  
  declare i64 @rt_and(i64, i64)
  
  declare i64 @rt_or(i64, i64)
  
  declare i64 @rt_print_int(i64)
  
  define i64 @cc_ac0__start(i64 %0, i64 %1, i64 %2, i64 %3, i64 %4, i64 %5, i64 %6, i64 %7, i64 %8) {
  entry:
    %9 = call i64 @rt_add(i64 %2, i64 %4)
    %10 = call i64 @rt_print_int(i64 %9)
    %11 = call i64 @rt_print_int(i64 %8)
    %12 = call i64 @rt_mul(i64 %2, i64 %4)
    %13 = call i64 @rt_div(i64 %12, i64 %5)
    %14 = call i64 @rt_add(i64 %13, i64 %7)
    ret i64 %14
  }

  $ ./codegen_runner.exe < manytests/typed/008ascription.ml
  ; ModuleID = 'aefjnv_ml'
  source_filename = "aefjnv_ml"
  
  @cc_ac2_main = global i64 0
  
  define i64 @main() {
  entry:
    %0 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @ll_0 to i64), i64 5)
    %1 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @ll_1 to i64), i64 3)
    %2 = call i64 @addi(i64 %0, i64 %1, i64 9)
    %3 = call i64 @rt_print_int(i64 %2)
    store i64 1, ptr @cc_ac2_main, align 4
    ret i64 0
  }
  
  declare i64 @rt_get_by_idx(i64, i64)
  
  declare i64 @rt_get_list_tail(i64, i64)
  
  declare i64 @rt_get_list_len(i64)
  
  declare i64 @rt_fail(i64)
  
  declare i64 @rt_alloc_closure(i64, i64)
  
  declare i64 @rt_apply_to_closure(i64, i64, ...)
  
  declare i64 @rt_append_to_list(i64, i64, ...)
  
  declare i64 @rt_alloc_tuple(i64, ...)
  
  declare i64 @rt_unminus(i64)
  
  declare i64 @rt_mul(i64, i64)
  
  declare i64 @rt_div(i64, i64)
  
  declare i64 @rt_add(i64, i64)
  
  declare i64 @rt_sub(i64, i64)
  
  declare i64 @rt_leq(i64, i64)
  
  declare i64 @rt_less(i64, i64)
  
  declare i64 @rt_meq(i64, i64)
  
  declare i64 @rt_more(i64, i64)
  
  declare i64 @rt_eq2(i64, i64)
  
  declare i64 @rt_eq(i64, i64)
  
  declare i64 @rt_neq(i64, i64)
  
  declare i64 @rt_and(i64, i64)
  
  declare i64 @rt_or(i64, i64)
  
  declare i64 @rt_print_int(i64)
  
  define i64 @addi(i64 %0, i64 %1, i64 %2) {
  entry:
    %3 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %1, i64 3, i64 %2)
    %4 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %0, i64 5, i64 %2, i64 %3)
    ret i64 %4
  }
  
  define i64 @ll_0(i64 %0, i64 %1) {
  entry:
    %2 = ashr i64 %1, 1
    %3 = trunc i64 %2 to i1
    br i1 %3, label %5, label %7
  
  continuation:                                     ; preds = %7, %5
    %4 = phi i64 [ %6, %5 ], [ %8, %7 ]
    ret i64 %4
  
  5:                                                ; preds = %entry
    %6 = call i64 @rt_add(i64 %0, i64 3)
    br label %continuation
  
  7:                                                ; preds = %entry
    %8 = call i64 @rt_mul(i64 %0, i64 5)
    br label %continuation
  }
  
  define i64 @ll_1(i64 %0) {
  entry:
    %1 = call i64 @rt_div(i64 %0, i64 5)
    %2 = call i64 @rt_eq(i64 %1, i64 1)
    ret i64 %2
  }

  $ ./codegen_runner.exe < manytests/typed/009let_poly.ml
  ; ModuleID = 'aefjnv_ml'
  source_filename = "aefjnv_ml"
  
  @temp = global i64 0
  
  define i64 @main() {
  entry:
    %0 = call i64 @f(i64 3)
    %1 = call i64 @f(i64 3)
    %2 = call i64 (i64, ...) @rt_alloc_tuple(i64 5, i64 %0, i64 %1)
    store i64 %2, ptr @temp, align 4
    ret i64 0
  }
  
  declare i64 @rt_get_by_idx(i64, i64)
  
  declare i64 @rt_get_list_tail(i64, i64)
  
  declare i64 @rt_get_list_len(i64)
  
  declare i64 @rt_fail(i64)
  
  declare i64 @rt_alloc_closure(i64, i64)
  
  declare i64 @rt_apply_to_closure(i64, i64, ...)
  
  declare i64 @rt_append_to_list(i64, i64, ...)
  
  declare i64 @rt_alloc_tuple(i64, ...)
  
  declare i64 @rt_unminus(i64)
  
  declare i64 @rt_mul(i64, i64)
  
  declare i64 @rt_div(i64, i64)
  
  declare i64 @rt_add(i64, i64)
  
  declare i64 @rt_sub(i64, i64)
  
  declare i64 @rt_leq(i64, i64)
  
  declare i64 @rt_less(i64, i64)
  
  declare i64 @rt_meq(i64, i64)
  
  declare i64 @rt_more(i64, i64)
  
  declare i64 @rt_eq2(i64, i64)
  
  declare i64 @rt_eq(i64, i64)
  
  declare i64 @rt_neq(i64, i64)
  
  declare i64 @rt_and(i64, i64)
  
  declare i64 @rt_or(i64, i64)
  
  declare i64 @rt_print_int(i64)
  
  define i64 @f(i64 %0) {
  entry:
    ret i64 %0
  }

  $ ./codegen_runner.exe < manytests/typed/011mapcps.ml
  ; ModuleID = 'aefjnv_ml'
  source_filename = "aefjnv_ml"
  
  @cc_ac6_main = global i64 0
  
  define i64 @main() {
  entry:
    %0 = call i64 (i64, i64, ...) @rt_append_to_list(i64 7, i64 0, i64 7, i64 5, i64 3)
    %1 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @ll_1 to i64), i64 3)
    %2 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @ll_2 to i64), i64 3)
    %3 = call i64 @map(i64 %1, i64 %0, i64 %2)
    %4 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @rt_print_int to i64), i64 3)
    %5 = call i64 @iter(i64 %4, i64 %3)
    store i64 %5, ptr @cc_ac6_main, align 4
    ret i64 0
  }
  
  declare i64 @rt_get_by_idx(i64, i64)
  
  declare i64 @rt_get_list_tail(i64, i64)
  
  declare i64 @rt_get_list_len(i64)
  
  declare i64 @rt_fail(i64)
  
  declare i64 @rt_alloc_closure(i64, i64)
  
  declare i64 @rt_apply_to_closure(i64, i64, ...)
  
  declare i64 @rt_append_to_list(i64, i64, ...)
  
  declare i64 @rt_alloc_tuple(i64, ...)
  
  declare i64 @rt_unminus(i64)
  
  declare i64 @rt_mul(i64, i64)
  
  declare i64 @rt_div(i64, i64)
  
  declare i64 @rt_add(i64, i64)
  
  declare i64 @rt_sub(i64, i64)
  
  declare i64 @rt_leq(i64, i64)
  
  declare i64 @rt_less(i64, i64)
  
  declare i64 @rt_meq(i64, i64)
  
  declare i64 @rt_more(i64, i64)
  
  declare i64 @rt_eq2(i64, i64)
  
  declare i64 @rt_eq(i64, i64)
  
  declare i64 @rt_neq(i64, i64)
  
  declare i64 @rt_and(i64, i64)
  
  declare i64 @rt_or(i64, i64)
  
  declare i64 @rt_print_int(i64)
  
  define i64 @ll_0(i64 %0, i64 %1, i64 %2, i64 %3) {
  entry:
    %4 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %2, i64 3, i64 %1)
    %5 = call i64 (i64, i64, ...) @rt_append_to_list(i64 3, i64 %3, i64 %4)
    %6 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %0, i64 3, i64 %5)
    ret i64 %6
  }
  
  define i64 @map(i64 %0, i64 %1, i64 %2) {
  entry:
    %3 = call i64 @rt_eq(i64 0, i64 %1)
    %4 = ashr i64 %3, 1
    %5 = trunc i64 %4 to i1
    br i1 %5, label %7, label %9
  
  continuation:                                     ; preds = %continuation1, %7
    %6 = phi i64 [ %8, %7 ], [ %14, %continuation1 ]
    ret i64 %6
  
  7:                                                ; preds = %entry
    %8 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %2, i64 3, i64 0)
    br label %continuation
  
  9:                                                ; preds = %entry
    %10 = call i64 @rt_get_list_len(i64 %1)
    %11 = call i64 @rt_meq(i64 %10, i64 5)
    %12 = ashr i64 %11, 1
    %13 = trunc i64 %12 to i1
    br i1 %13, label %15, label %21
  
  continuation1:                                    ; preds = %21, %15
    %14 = phi i64 [ %20, %15 ], [ %22, %21 ]
    br label %continuation
  
  15:                                               ; preds = %9
    %16 = call i64 @rt_get_list_tail(i64 %1, i64 3)
    %17 = call i64 @rt_get_by_idx(i64 %1, i64 1)
    %18 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @ll_0 to i64), i64 9)
    %19 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %18, i64 7, i64 %2, i64 %17, i64 %0)
    %20 = call i64 @map(i64 %0, i64 %16, i64 %19)
    br label %continuation1
  
  21:                                               ; preds = %9
    %22 = call i64 @rt_fail(i64 0)
    br label %continuation1
  }
  
  define i64 @iter(i64 %0, i64 %1) {
  entry:
    %2 = call i64 @rt_eq(i64 0, i64 %1)
    %3 = ashr i64 %2, 1
    %4 = trunc i64 %3 to i1
    br i1 %4, label %6, label %7
  
  continuation:                                     ; preds = %continuation1, %6
    %5 = phi i64 [ 0, %6 ], [ %12, %continuation1 ]
    ret i64 %5
  
  6:                                                ; preds = %entry
    br label %continuation
  
  7:                                                ; preds = %entry
    %8 = call i64 @rt_get_list_len(i64 %1)
    %9 = call i64 @rt_meq(i64 %8, i64 5)
    %10 = ashr i64 %9, 1
    %11 = trunc i64 %10 to i1
    br i1 %11, label %13, label %18
  
  continuation1:                                    ; preds = %18, %13
    %12 = phi i64 [ %17, %13 ], [ %19, %18 ]
    br label %continuation
  
  13:                                               ; preds = %7
    %14 = call i64 @rt_get_list_tail(i64 %1, i64 3)
    %15 = call i64 @rt_get_by_idx(i64 %1, i64 1)
    %16 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %0, i64 3, i64 %15)
    %17 = call i64 @iter(i64 %0, i64 %14)
    br label %continuation1
  
  18:                                               ; preds = %7
    %19 = call i64 @rt_fail(i64 0)
    br label %continuation1
  }
  
  define i64 @ll_1(i64 %0) {
  entry:
    %1 = call i64 @rt_add(i64 %0, i64 3)
    ret i64 %1
  }
  
  define i64 @ll_2(i64 %0) {
  entry:
    ret i64 %0
  }

  $ ./codegen_runner.exe < manytests/typed/013foldfoldr.ml
  ; ModuleID = 'aefjnv_ml'
  source_filename = "aefjnv_ml"
  
  @cc_ac3_main = global i64 0
  
  define i64 @main() {
  entry:
    %0 = call i64 (i64, i64, ...) @rt_append_to_list(i64 7, i64 0, i64 7, i64 5, i64 3)
    %1 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @ll_1 to i64), i64 5)
    %2 = call i64 @foldl(i64 %1, i64 3, i64 %0)
    %3 = call i64 @rt_print_int(i64 %2)
    store i64 %3, ptr @cc_ac3_main, align 4
    ret i64 0
  }
  
  declare i64 @rt_get_by_idx(i64, i64)
  
  declare i64 @rt_get_list_tail(i64, i64)
  
  declare i64 @rt_get_list_len(i64)
  
  declare i64 @rt_fail(i64)
  
  declare i64 @rt_alloc_closure(i64, i64)
  
  declare i64 @rt_apply_to_closure(i64, i64, ...)
  
  declare i64 @rt_append_to_list(i64, i64, ...)
  
  declare i64 @rt_alloc_tuple(i64, ...)
  
  declare i64 @rt_unminus(i64)
  
  declare i64 @rt_mul(i64, i64)
  
  declare i64 @rt_div(i64, i64)
  
  declare i64 @rt_add(i64, i64)
  
  declare i64 @rt_sub(i64, i64)
  
  declare i64 @rt_leq(i64, i64)
  
  declare i64 @rt_less(i64, i64)
  
  declare i64 @rt_meq(i64, i64)
  
  declare i64 @rt_more(i64, i64)
  
  declare i64 @rt_eq2(i64, i64)
  
  declare i64 @rt_eq(i64, i64)
  
  declare i64 @rt_neq(i64, i64)
  
  declare i64 @rt_and(i64, i64)
  
  declare i64 @rt_or(i64, i64)
  
  declare i64 @rt_print_int(i64)
  
  define i64 @id(i64 %0) {
  entry:
    ret i64 %0
  }
  
  define i64 @fold_right(i64 %0, i64 %1, i64 %2) {
  entry:
    %3 = call i64 @rt_eq(i64 0, i64 %2)
    %4 = ashr i64 %3, 1
    %5 = trunc i64 %4 to i1
    br i1 %5, label %7, label %8
  
  continuation:                                     ; preds = %continuation1, %7
    %6 = phi i64 [ %1, %7 ], [ %13, %continuation1 ]
    ret i64 %6
  
  7:                                                ; preds = %entry
    br label %continuation
  
  8:                                                ; preds = %entry
    %9 = call i64 @rt_get_list_len(i64 %2)
    %10 = call i64 @rt_meq(i64 %9, i64 5)
    %11 = ashr i64 %10, 1
    %12 = trunc i64 %11 to i1
    br i1 %12, label %14, label %19
  
  continuation1:                                    ; preds = %19, %14
    %13 = phi i64 [ %18, %14 ], [ %20, %19 ]
    br label %continuation
  
  14:                                               ; preds = %8
    %15 = call i64 @rt_get_list_tail(i64 %2, i64 3)
    %16 = call i64 @rt_get_by_idx(i64 %2, i64 1)
    %17 = call i64 @fold_right(i64 %0, i64 %1, i64 %15)
    %18 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %0, i64 5, i64 %16, i64 %17)
    br label %continuation1
  
  19:                                               ; preds = %8
    %20 = call i64 @rt_fail(i64 0)
    br label %continuation1
  }
  
  define i64 @ll_0(i64 %0, i64 %1, i64 %2, i64 %3) {
  entry:
    %4 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %0, i64 5, i64 %3, i64 %1)
    %5 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %2, i64 3, i64 %4)
    ret i64 %5
  }
  
  define i64 @foldl(i64 %0, i64 %1, i64 %2) {
  entry:
    %3 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @ll_0 to i64), i64 9)
    %4 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %3, i64 3, i64 %0)
    %5 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @id to i64), i64 3)
    %6 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @fold_right to i64), i64 7)
    %7 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %6, i64 9, i64 %4, i64 %5, i64 %2, i64 %1)
    ret i64 %7
  }
  
  define i64 @ll_1(i64 %0, i64 %1) {
  entry:
    %2 = call i64 @rt_mul(i64 %0, i64 %1)
    ret i64 %2
  }

  $ ./codegen_runner.exe < manytests/typed/015tuples.ml
  ; ModuleID = 'aefjnv_ml'
  source_filename = "aefjnv_ml"
  
  @tie = global i64 0
  @cc_ac10_main = global i64 0
  
  define i64 @main() {
  entry:
    %0 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @feven to i64), i64 5)
    %1 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @fodd to i64), i64 5)
    %2 = call i64 (i64, ...) @rt_alloc_tuple(i64 5, i64 %0, i64 %1)
    %3 = call i64 @fixpoly(i64 %2)
    store i64 %3, ptr @tie, align 4
    %4 = call i64 @modd(i64 3)
    %5 = call i64 @rt_print_int(i64 %4)
    %6 = call i64 @cc_ac_meven(i64 5)
    %7 = call i64 @rt_print_int(i64 %6)
    %tie = load i64, ptr @tie, align 4
    %8 = call i64 @rt_get_by_idx(i64 %tie, i64 1)
    %9 = call i64 @rt_get_by_idx(i64 %tie, i64 3)
    %10 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %9, i64 3, i64 7)
    %11 = call i64 @rt_print_int(i64 %10)
    %12 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %8, i64 3, i64 9)
    %13 = call i64 @rt_print_int(i64 %12)
    store i64 1, ptr @cc_ac10_main, align 4
    ret i64 0
  }
  
  declare i64 @rt_get_by_idx(i64, i64)
  
  declare i64 @rt_get_list_tail(i64, i64)
  
  declare i64 @rt_get_list_len(i64)
  
  declare i64 @rt_fail(i64)
  
  declare i64 @rt_alloc_closure(i64, i64)
  
  declare i64 @rt_apply_to_closure(i64, i64, ...)
  
  declare i64 @rt_append_to_list(i64, i64, ...)
  
  declare i64 @rt_alloc_tuple(i64, ...)
  
  declare i64 @rt_unminus(i64)
  
  declare i64 @rt_mul(i64, i64)
  
  declare i64 @rt_div(i64, i64)
  
  declare i64 @rt_add(i64, i64)
  
  declare i64 @rt_sub(i64, i64)
  
  declare i64 @rt_leq(i64, i64)
  
  declare i64 @rt_less(i64, i64)
  
  declare i64 @rt_meq(i64, i64)
  
  declare i64 @rt_more(i64, i64)
  
  declare i64 @rt_eq2(i64, i64)
  
  declare i64 @rt_eq(i64, i64)
  
  declare i64 @rt_neq(i64, i64)
  
  declare i64 @rt_and(i64, i64)
  
  declare i64 @rt_or(i64, i64)
  
  declare i64 @rt_print_int(i64)
  
  define i64 @fix(i64 %0, i64 %1) {
  entry:
    %2 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @fix to i64), i64 5)
    %3 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %2, i64 3, i64 %0)
    %4 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %0, i64 5, i64 %3, i64 %1)
    ret i64 %4
  }
  
  define i64 @map(i64 %0, i64 %1) {
  entry:
    %2 = call i64 @rt_get_by_idx(i64 %1, i64 1)
    %3 = call i64 @rt_get_by_idx(i64 %1, i64 3)
    %4 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %0, i64 3, i64 %2)
    %5 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %0, i64 3, i64 %3)
    %6 = call i64 (i64, ...) @rt_alloc_tuple(i64 5, i64 %4, i64 %5)
    ret i64 %6
  }
  
  define i64 @ll_1(i64 %0, i64 %1, i64 %2, i64 %3) {
  entry:
    %4 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %0, i64 3, i64 %1)
    %5 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %2, i64 5, i64 %4, i64 %3)
    ret i64 %5
  }
  
  define i64 @ll_0(i64 %0, i64 %1) {
  entry:
    %2 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @ll_1 to i64), i64 9)
    %3 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %2, i64 5, i64 %0, i64 %1)
    %4 = call i64 @map(i64 %3, i64 %1)
    ret i64 %4
  }
  
  define i64 @fixpoly(i64 %0) {
  entry:
    %1 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @ll_0 to i64), i64 5)
    %2 = call i64 @fix(i64 %1, i64 %0)
    ret i64 %2
  }
  
  define i64 @feven(i64 %0, i64 %1) {
  entry:
    %2 = call i64 @rt_get_by_idx(i64 %0, i64 1)
    %3 = call i64 @rt_get_by_idx(i64 %0, i64 3)
    %4 = call i64 @rt_eq(i64 %1, i64 1)
    %5 = ashr i64 %4, 1
    %6 = trunc i64 %5 to i1
    br i1 %6, label %8, label %9
  
  continuation:                                     ; preds = %9, %8
    %7 = phi i64 [ 3, %8 ], [ %11, %9 ]
    ret i64 %7
  
  8:                                                ; preds = %entry
    br label %continuation
  
  9:                                                ; preds = %entry
    %10 = call i64 @rt_sub(i64 %1, i64 3)
    %11 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %3, i64 3, i64 %10)
    br label %continuation
  }
  
  define i64 @fodd(i64 %0, i64 %1) {
  entry:
    %2 = call i64 @rt_get_by_idx(i64 %0, i64 1)
    %3 = call i64 @rt_get_by_idx(i64 %0, i64 3)
    %4 = call i64 @rt_eq(i64 %1, i64 1)
    %5 = ashr i64 %4, 1
    %6 = trunc i64 %5 to i1
    br i1 %6, label %8, label %9
  
  continuation:                                     ; preds = %9, %8
    %7 = phi i64 [ 1, %8 ], [ %11, %9 ]
    ret i64 %7
  
  8:                                                ; preds = %entry
    br label %continuation
  
  9:                                                ; preds = %entry
    %10 = call i64 @rt_sub(i64 %1, i64 3)
    %11 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %2, i64 3, i64 %10)
    br label %continuation
  }
  
  define i64 @cc_ac_meven(i64 %0) {
  entry:
    %1 = call i64 @rt_eq(i64 %0, i64 1)
    %2 = ashr i64 %1, 1
    %3 = trunc i64 %2 to i1
    br i1 %3, label %5, label %6
  
  continuation:                                     ; preds = %6, %5
    %4 = phi i64 [ 3, %5 ], [ %8, %6 ]
    ret i64 %4
  
  5:                                                ; preds = %entry
    br label %continuation
  
  6:                                                ; preds = %entry
    %7 = call i64 @rt_sub(i64 %0, i64 3)
    %8 = call i64 @modd(i64 %7)
    br label %continuation
  }
  
  define i64 @modd(i64 %0) {
  entry:
    %1 = call i64 @rt_eq(i64 %0, i64 1)
    %2 = ashr i64 %1, 1
    %3 = trunc i64 %2 to i1
    br i1 %3, label %5, label %6
  
  continuation:                                     ; preds = %6, %5
    %4 = phi i64 [ 3, %5 ], [ %8, %6 ]
    ret i64 %4
  
  5:                                                ; preds = %entry
    br label %continuation
  
  6:                                                ; preds = %entry
    %7 = call i64 @rt_sub(i64 %0, i64 3)
    %8 = call i64 @cc_ac_meven(i64 %7)
    br label %continuation
  }

  $ ./codegen_runner.exe < manytests/typed/016lists.ml
  ; ModuleID = 'aefjnv_ml'
  source_filename = "aefjnv_ml"
  
  @length_tail = global i64 0
  @concat = global i64 0
  @cc_ac26_main = global i64 0
  
  define i64 @main() {
  entry:
    %0 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @helper to i64), i64 5)
    %1 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %0, i64 3, i64 1)
    store i64 %1, ptr @length_tail, align 4
    %2 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @cc_ac13_helper to i64), i64 3)
    store i64 %2, ptr @concat, align 4
    %3 = call i64 (i64, i64, ...) @rt_append_to_list(i64 7, i64 0, i64 7, i64 5, i64 3)
    %4 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @rt_print_int to i64), i64 3)
    %5 = call i64 @iter(i64 %4, i64 %3)
    %6 = call i64 (i64, i64, ...) @rt_append_to_list(i64 9, i64 0, i64 9, i64 7, i64 5, i64 3)
    %7 = call i64 (i64, i64, ...) @rt_append_to_list(i64 5, i64 0, i64 5, i64 3)
    %8 = call i64 @cartesian(i64 %7, i64 %6)
    %9 = call i64 @length(i64 %8)
    %10 = call i64 @rt_print_int(i64 %9)
    store i64 1, ptr @cc_ac26_main, align 4
    ret i64 0
  }
  
  declare i64 @rt_get_by_idx(i64, i64)
  
  declare i64 @rt_get_list_tail(i64, i64)
  
  declare i64 @rt_get_list_len(i64)
  
  declare i64 @rt_fail(i64)
  
  declare i64 @rt_alloc_closure(i64, i64)
  
  declare i64 @rt_apply_to_closure(i64, i64, ...)
  
  declare i64 @rt_append_to_list(i64, i64, ...)
  
  declare i64 @rt_alloc_tuple(i64, ...)
  
  declare i64 @rt_unminus(i64)
  
  declare i64 @rt_mul(i64, i64)
  
  declare i64 @rt_div(i64, i64)
  
  declare i64 @rt_add(i64, i64)
  
  declare i64 @rt_sub(i64, i64)
  
  declare i64 @rt_leq(i64, i64)
  
  declare i64 @rt_less(i64, i64)
  
  declare i64 @rt_meq(i64, i64)
  
  declare i64 @rt_more(i64, i64)
  
  declare i64 @rt_eq2(i64, i64)
  
  declare i64 @rt_eq(i64, i64)
  
  declare i64 @rt_neq(i64, i64)
  
  declare i64 @rt_and(i64, i64)
  
  declare i64 @rt_or(i64, i64)
  
  declare i64 @rt_print_int(i64)
  
  define i64 @length(i64 %0) {
  entry:
    %1 = call i64 @rt_eq(i64 0, i64 %0)
    %2 = ashr i64 %1, 1
    %3 = trunc i64 %2 to i1
    br i1 %3, label %5, label %6
  
  continuation:                                     ; preds = %continuation1, %5
    %4 = phi i64 [ 1, %5 ], [ %11, %continuation1 ]
    ret i64 %4
  
  5:                                                ; preds = %entry
    br label %continuation
  
  6:                                                ; preds = %entry
    %7 = call i64 @rt_get_list_len(i64 %0)
    %8 = call i64 @rt_meq(i64 %7, i64 5)
    %9 = ashr i64 %8, 1
    %10 = trunc i64 %9 to i1
    br i1 %10, label %12, label %17
  
  continuation1:                                    ; preds = %17, %12
    %11 = phi i64 [ %16, %12 ], [ %18, %17 ]
    br label %continuation
  
  12:                                               ; preds = %6
    %13 = call i64 @rt_get_list_tail(i64 %0, i64 3)
    %14 = call i64 @rt_get_by_idx(i64 %0, i64 1)
    %15 = call i64 @length(i64 %13)
    %16 = call i64 @rt_add(i64 3, i64 %15)
    br label %continuation1
  
  17:                                               ; preds = %6
    %18 = call i64 @rt_fail(i64 0)
    br label %continuation1
  }
  
  define i64 @helper(i64 %0, i64 %1) {
  entry:
    %2 = call i64 @rt_eq(i64 0, i64 %1)
    %3 = ashr i64 %2, 1
    %4 = trunc i64 %3 to i1
    br i1 %4, label %6, label %7
  
  continuation:                                     ; preds = %continuation1, %6
    %5 = phi i64 [ %0, %6 ], [ %12, %continuation1 ]
    ret i64 %5
  
  6:                                                ; preds = %entry
    br label %continuation
  
  7:                                                ; preds = %entry
    %8 = call i64 @rt_get_list_len(i64 %1)
    %9 = call i64 @rt_meq(i64 %8, i64 5)
    %10 = ashr i64 %9, 1
    %11 = trunc i64 %10 to i1
    br i1 %11, label %13, label %18
  
  continuation1:                                    ; preds = %18, %13
    %12 = phi i64 [ %17, %13 ], [ %19, %18 ]
    br label %continuation
  
  13:                                               ; preds = %7
    %14 = call i64 @rt_get_list_tail(i64 %1, i64 3)
    %15 = call i64 @rt_get_by_idx(i64 %1, i64 1)
    %16 = call i64 @rt_add(i64 %0, i64 3)
    %17 = call i64 @helper(i64 %16, i64 %14)
    br label %continuation1
  
  18:                                               ; preds = %7
    %19 = call i64 @rt_fail(i64 0)
    br label %continuation1
  }
  
  define i64 @map(i64 %0, i64 %1) {
  entry:
    %2 = call i64 @rt_eq(i64 0, i64 %1)
    %3 = ashr i64 %2, 1
    %4 = trunc i64 %3 to i1
    br i1 %4, label %6, label %7
  
  continuation:                                     ; preds = %continuation1, %6
    %5 = phi i64 [ 0, %6 ], [ %12, %continuation1 ]
    ret i64 %5
  
  6:                                                ; preds = %entry
    br label %continuation
  
  7:                                                ; preds = %entry
    %8 = call i64 @rt_get_list_len(i64 %1)
    %9 = call i64 @rt_eq(i64 3, i64 %8)
    %10 = ashr i64 %9, 1
    %11 = trunc i64 %10 to i1
    br i1 %11, label %13, label %17
  
  continuation1:                                    ; preds = %continuation2, %13
    %12 = phi i64 [ %16, %13 ], [ %22, %continuation2 ]
    br label %continuation
  
  13:                                               ; preds = %7
    %14 = call i64 @rt_get_by_idx(i64 %1, i64 1)
    %15 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %0, i64 3, i64 %14)
    %16 = call i64 (i64, i64, ...) @rt_append_to_list(i64 3, i64 0, i64 %15)
    br label %continuation1
  
  17:                                               ; preds = %7
    %18 = call i64 @rt_get_list_len(i64 %1)
    %19 = call i64 @rt_eq(i64 5, i64 %18)
    %20 = ashr i64 %19, 1
    %21 = trunc i64 %20 to i1
    br i1 %21, label %23, label %29
  
  continuation2:                                    ; preds = %continuation3, %23
    %22 = phi i64 [ %28, %23 ], [ %34, %continuation3 ]
    br label %continuation1
  
  23:                                               ; preds = %17
    %24 = call i64 @rt_get_by_idx(i64 %1, i64 1)
    %25 = call i64 @rt_get_by_idx(i64 %1, i64 3)
    %26 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %0, i64 3, i64 %25)
    %27 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %0, i64 3, i64 %24)
    %28 = call i64 (i64, i64, ...) @rt_append_to_list(i64 5, i64 0, i64 %26, i64 %27)
    br label %continuation2
  
  29:                                               ; preds = %17
    %30 = call i64 @rt_get_list_len(i64 %1)
    %31 = call i64 @rt_eq(i64 7, i64 %30)
    %32 = ashr i64 %31, 1
    %33 = trunc i64 %32 to i1
    br i1 %33, label %35, label %43
  
  continuation3:                                    ; preds = %continuation4, %35
    %34 = phi i64 [ %42, %35 ], [ %48, %continuation4 ]
    br label %continuation2
  
  35:                                               ; preds = %29
    %36 = call i64 @rt_get_by_idx(i64 %1, i64 1)
    %37 = call i64 @rt_get_by_idx(i64 %1, i64 3)
    %38 = call i64 @rt_get_by_idx(i64 %1, i64 5)
    %39 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %0, i64 3, i64 %38)
    %40 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %0, i64 3, i64 %37)
    %41 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %0, i64 3, i64 %36)
    %42 = call i64 (i64, i64, ...) @rt_append_to_list(i64 7, i64 0, i64 %39, i64 %40, i64 %41)
    br label %continuation3
  
  43:                                               ; preds = %29
    %44 = call i64 @rt_get_list_len(i64 %1)
    %45 = call i64 @rt_meq(i64 %44, i64 11)
    %46 = ashr i64 %45, 1
    %47 = trunc i64 %46 to i1
    br i1 %47, label %49, label %61
  
  continuation4:                                    ; preds = %61, %49
    %48 = phi i64 [ %60, %49 ], [ %62, %61 ]
    br label %continuation3
  
  49:                                               ; preds = %43
    %50 = call i64 @rt_get_list_tail(i64 %1, i64 9)
    %51 = call i64 @rt_get_by_idx(i64 %1, i64 1)
    %52 = call i64 @rt_get_by_idx(i64 %1, i64 3)
    %53 = call i64 @rt_get_by_idx(i64 %1, i64 5)
    %54 = call i64 @rt_get_by_idx(i64 %1, i64 7)
    %55 = call i64 @map(i64 %0, i64 %50)
    %56 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %0, i64 3, i64 %54)
    %57 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %0, i64 3, i64 %53)
    %58 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %0, i64 3, i64 %52)
    %59 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %0, i64 3, i64 %51)
    %60 = call i64 (i64, i64, ...) @rt_append_to_list(i64 9, i64 %55, i64 %56, i64 %57, i64 %58, i64 %59)
    br label %continuation4
  
  61:                                               ; preds = %43
    %62 = call i64 @rt_fail(i64 0)
    br label %continuation4
  }
  
  define i64 @append(i64 %0, i64 %1) {
  entry:
    %2 = call i64 @rt_eq(i64 0, i64 %0)
    %3 = ashr i64 %2, 1
    %4 = trunc i64 %3 to i1
    br i1 %4, label %6, label %7
  
  continuation:                                     ; preds = %continuation1, %6
    %5 = phi i64 [ %1, %6 ], [ %12, %continuation1 ]
    ret i64 %5
  
  6:                                                ; preds = %entry
    br label %continuation
  
  7:                                                ; preds = %entry
    %8 = call i64 @rt_get_list_len(i64 %0)
    %9 = call i64 @rt_meq(i64 %8, i64 5)
    %10 = ashr i64 %9, 1
    %11 = trunc i64 %10 to i1
    br i1 %11, label %13, label %18
  
  continuation1:                                    ; preds = %18, %13
    %12 = phi i64 [ %17, %13 ], [ %19, %18 ]
    br label %continuation
  
  13:                                               ; preds = %7
    %14 = call i64 @rt_get_list_tail(i64 %0, i64 3)
    %15 = call i64 @rt_get_by_idx(i64 %0, i64 1)
    %16 = call i64 @append(i64 %14, i64 %1)
    %17 = call i64 (i64, i64, ...) @rt_append_to_list(i64 3, i64 %16, i64 %15)
    br label %continuation1
  
  18:                                               ; preds = %7
    %19 = call i64 @rt_fail(i64 0)
    br label %continuation1
  }
  
  define i64 @cc_ac13_helper(i64 %0) {
  entry:
    %1 = call i64 @rt_eq(i64 0, i64 %0)
    %2 = ashr i64 %1, 1
    %3 = trunc i64 %2 to i1
    br i1 %3, label %5, label %6
  
  continuation:                                     ; preds = %continuation1, %5
    %4 = phi i64 [ 0, %5 ], [ %11, %continuation1 ]
    ret i64 %4
  
  5:                                                ; preds = %entry
    br label %continuation
  
  6:                                                ; preds = %entry
    %7 = call i64 @rt_get_list_len(i64 %0)
    %8 = call i64 @rt_meq(i64 %7, i64 5)
    %9 = ashr i64 %8, 1
    %10 = trunc i64 %9 to i1
    br i1 %10, label %12, label %17
  
  continuation1:                                    ; preds = %17, %12
    %11 = phi i64 [ %16, %12 ], [ %18, %17 ]
    br label %continuation
  
  12:                                               ; preds = %6
    %13 = call i64 @rt_get_list_tail(i64 %0, i64 3)
    %14 = call i64 @rt_get_by_idx(i64 %0, i64 1)
    %15 = call i64 @cc_ac13_helper(i64 %13)
    %16 = call i64 @append(i64 %14, i64 %15)
    br label %continuation1
  
  17:                                               ; preds = %6
    %18 = call i64 @rt_fail(i64 0)
    br label %continuation1
  }
  
  define i64 @iter(i64 %0, i64 %1) {
  entry:
    %2 = call i64 @rt_eq(i64 0, i64 %1)
    %3 = ashr i64 %2, 1
    %4 = trunc i64 %3 to i1
    br i1 %4, label %6, label %7
  
  continuation:                                     ; preds = %continuation1, %6
    %5 = phi i64 [ 0, %6 ], [ %12, %continuation1 ]
    ret i64 %5
  
  6:                                                ; preds = %entry
    br label %continuation
  
  7:                                                ; preds = %entry
    %8 = call i64 @rt_get_list_len(i64 %1)
    %9 = call i64 @rt_meq(i64 %8, i64 5)
    %10 = ashr i64 %9, 1
    %11 = trunc i64 %10 to i1
    br i1 %11, label %13, label %18
  
  continuation1:                                    ; preds = %18, %13
    %12 = phi i64 [ %17, %13 ], [ %19, %18 ]
    br label %continuation
  
  13:                                               ; preds = %7
    %14 = call i64 @rt_get_list_tail(i64 %1, i64 3)
    %15 = call i64 @rt_get_by_idx(i64 %1, i64 1)
    %16 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %0, i64 3, i64 %15)
    %17 = call i64 @iter(i64 %0, i64 %14)
    br label %continuation1
  
  18:                                               ; preds = %7
    %19 = call i64 @rt_fail(i64 0)
    br label %continuation1
  }
  
  define i64 @ll_0(i64 %0, i64 %1) {
  entry:
    %2 = call i64 (i64, ...) @rt_alloc_tuple(i64 5, i64 %0, i64 %1)
    ret i64 %2
  }
  
  define i64 @cartesian(i64 %0, i64 %1) {
  entry:
    %2 = call i64 @rt_eq(i64 0, i64 %0)
    %3 = ashr i64 %2, 1
    %4 = trunc i64 %3 to i1
    br i1 %4, label %6, label %7
  
  continuation:                                     ; preds = %continuation1, %6
    %5 = phi i64 [ 0, %6 ], [ %12, %continuation1 ]
    ret i64 %5
  
  6:                                                ; preds = %entry
    br label %continuation
  
  7:                                                ; preds = %entry
    %8 = call i64 @rt_get_list_len(i64 %0)
    %9 = call i64 @rt_meq(i64 %8, i64 5)
    %10 = ashr i64 %9, 1
    %11 = trunc i64 %10 to i1
    br i1 %11, label %13, label %21
  
  continuation1:                                    ; preds = %21, %13
    %12 = phi i64 [ %20, %13 ], [ %22, %21 ]
    br label %continuation
  
  13:                                               ; preds = %7
    %14 = call i64 @rt_get_list_tail(i64 %0, i64 3)
    %15 = call i64 @rt_get_by_idx(i64 %0, i64 1)
    %16 = call i64 @cartesian(i64 %14, i64 %1)
    %17 = call i64 @rt_alloc_closure(i64 ptrtoint (ptr @ll_0 to i64), i64 5)
    %18 = call i64 (i64, i64, ...) @rt_apply_to_closure(i64 %17, i64 3, i64 %15)
    %19 = call i64 @map(i64 %18, i64 %1)
    %20 = call i64 @append(i64 %19, i64 %16)
    br label %continuation1
  
  21:                                               ; preds = %7
    %22 = call i64 @rt_fail(i64 0)
    br label %continuation1
  }
