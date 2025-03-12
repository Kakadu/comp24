  $ ./LlvmCodegenRunner.exe <<EOF
  > let a = print_int 1 
  > EOF

  $ cat output.ll
  ; ModuleID = 'HamsterML'
  source_filename = "HamsterML"
  
  @plus_mlint_glob_llvm = global i64 0
  @minus_mlint_glob_llvm = global i64 0
  @mult_mlint_glob_llvm = global i64 0
  @div_mlint_glob_llvm = global i64 0
  @l_ml_glob_llvm = global i64 0
  @le_ml_glob_llvm = global i64 0
  @g_ml_glob_llvm = global i64 0
  @ge_ml_glob_llvm = global i64 0
  @eq_ml_glob_llvm = global i64 0
  @peq_ml_glob_llvm = global i64 0
  @neq_ml_glob_llvm = global i64 0
  @pneq_ml_glob_llvm = global i64 0
  @print_int_glob_llvm = global i64 0
  @land_ml_glob_llvm = global i64 0
  @lor_ml_glob_llvm = global i64 0
  @ll_var_0_glob_llvm = global i64 0
  
  define i64 @init_llvm() {
  entry:
    %0 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @plus_mlint to i64), i64 2)
    store i64 %0, ptr @plus_mlint_glob_llvm, align 4
    %1 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @minus_mlint to i64), i64 2)
    store i64 %1, ptr @minus_mlint_glob_llvm, align 4
    %2 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @mult_mlint to i64), i64 2)
    store i64 %2, ptr @mult_mlint_glob_llvm, align 4
    %3 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @div_mlint to i64), i64 2)
    store i64 %3, ptr @div_mlint_glob_llvm, align 4
    %4 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @l_ml to i64), i64 2)
    store i64 %4, ptr @l_ml_glob_llvm, align 4
    %5 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @le_ml to i64), i64 2)
    store i64 %5, ptr @le_ml_glob_llvm, align 4
    %6 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @g_ml to i64), i64 2)
    store i64 %6, ptr @g_ml_glob_llvm, align 4
    %7 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @ge_ml to i64), i64 2)
    store i64 %7, ptr @ge_ml_glob_llvm, align 4
    %8 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @eq_ml to i64), i64 2)
    store i64 %8, ptr @eq_ml_glob_llvm, align 4
    %9 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @peq_ml to i64), i64 2)
    store i64 %9, ptr @peq_ml_glob_llvm, align 4
    %10 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @neq_ml to i64), i64 2)
    store i64 %10, ptr @neq_ml_glob_llvm, align 4
    %11 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @pneq_ml to i64), i64 2)
    store i64 %11, ptr @pneq_ml_glob_llvm, align 4
    %12 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    store i64 %12, ptr @print_int_glob_llvm, align 4
    %13 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @land_ml to i64), i64 2)
    store i64 %13, ptr @land_ml_glob_llvm, align 4
    %14 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @lor_ml to i64), i64 2)
    store i64 %14, ptr @lor_ml_glob_llvm, align 4
    ret i64 0
  }
  
  declare i64 @plus_mlint(i64, i64)
  
  declare i64 @minus_mlint(i64, i64)
  
  declare i64 @mult_mlint(i64, i64)
  
  declare i64 @div_mlint(i64, i64)
  
  declare i64 @l_ml(i64, i64)
  
  declare i64 @le_ml(i64, i64)
  
  declare i64 @g_ml(i64, i64)
  
  declare i64 @ge_ml(i64, i64)
  
  declare i64 @eq_ml(i64, i64)
  
  declare i64 @peq_ml(i64, i64)
  
  declare i64 @neq_ml(i64, i64)
  
  declare i64 @pneq_ml(i64, i64)
  
  declare i64 @print_int(i64)
  
  declare i64 @land_ml(i64, i64)
  
  declare i64 @lor_ml(i64, i64)
  
  declare i64 @mlrt_get_box_field(i64, i64)
  
  declare i64 @mlrt_check_tag(i64, i64)
  
  declare i64 @mltr_match_error(i64)
  
  declare i64 @mlrt_create_tuple(i64, ...)
  
  declare i64 @mlrt_create_empty_closure(i64)
  
  declare i64 @mlrt_apply_args_to_closure(i64, i64, ...)
  
  define i64 @main() {
  entry:
    %0 = call i64 @init_llvm()
    %ll_var_0_glob_llvm = call i64 @ll_var_0()
    store i64 %ll_var_0_glob_llvm, ptr @ll_var_0_glob_llvm, align 4
    ret i64 0
  }
  
  define i64 @ll_var_0() {
  entry:
    %0 = call i64 @print_int(i64 3)
    ret i64 %0
  }
 
  $ clang-16 output.ll -Wno-override-module -o output_executable ../../lib/runtime.so -lffi
  $ ./output_executable
  1

  $ ./LlvmCodegenRunner.exe < manytests/typed/001fac.ml

  $ cat output.ll
  ; ModuleID = 'HamsterML'
  source_filename = "HamsterML"
  
  @plus_mlint_glob_llvm = global i64 0
  @minus_mlint_glob_llvm = global i64 0
  @mult_mlint_glob_llvm = global i64 0
  @div_mlint_glob_llvm = global i64 0
  @l_ml_glob_llvm = global i64 0
  @le_ml_glob_llvm = global i64 0
  @g_ml_glob_llvm = global i64 0
  @ge_ml_glob_llvm = global i64 0
  @eq_ml_glob_llvm = global i64 0
  @peq_ml_glob_llvm = global i64 0
  @neq_ml_glob_llvm = global i64 0
  @pneq_ml_glob_llvm = global i64 0
  @print_int_glob_llvm = global i64 0
  @land_ml_glob_llvm = global i64 0
  @lor_ml_glob_llvm = global i64 0
  @ll_var_0_glob_llvm = global i64 0
  @ll_var_1_glob_llvm = global i64 0
  
  define i64 @init_llvm() {
  entry:
    %0 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @plus_mlint to i64), i64 2)
    store i64 %0, ptr @plus_mlint_glob_llvm, align 4
    %1 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @minus_mlint to i64), i64 2)
    store i64 %1, ptr @minus_mlint_glob_llvm, align 4
    %2 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @mult_mlint to i64), i64 2)
    store i64 %2, ptr @mult_mlint_glob_llvm, align 4
    %3 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @div_mlint to i64), i64 2)
    store i64 %3, ptr @div_mlint_glob_llvm, align 4
    %4 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @l_ml to i64), i64 2)
    store i64 %4, ptr @l_ml_glob_llvm, align 4
    %5 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @le_ml to i64), i64 2)
    store i64 %5, ptr @le_ml_glob_llvm, align 4
    %6 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @g_ml to i64), i64 2)
    store i64 %6, ptr @g_ml_glob_llvm, align 4
    %7 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @ge_ml to i64), i64 2)
    store i64 %7, ptr @ge_ml_glob_llvm, align 4
    %8 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @eq_ml to i64), i64 2)
    store i64 %8, ptr @eq_ml_glob_llvm, align 4
    %9 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @peq_ml to i64), i64 2)
    store i64 %9, ptr @peq_ml_glob_llvm, align 4
    %10 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @neq_ml to i64), i64 2)
    store i64 %10, ptr @neq_ml_glob_llvm, align 4
    %11 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @pneq_ml to i64), i64 2)
    store i64 %11, ptr @pneq_ml_glob_llvm, align 4
    %12 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    store i64 %12, ptr @print_int_glob_llvm, align 4
    %13 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @land_ml to i64), i64 2)
    store i64 %13, ptr @land_ml_glob_llvm, align 4
    %14 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @lor_ml to i64), i64 2)
    store i64 %14, ptr @lor_ml_glob_llvm, align 4
    ret i64 0
  }
  
  declare i64 @plus_mlint(i64, i64)
  
  declare i64 @minus_mlint(i64, i64)
  
  declare i64 @mult_mlint(i64, i64)
  
  declare i64 @div_mlint(i64, i64)
  
  declare i64 @l_ml(i64, i64)
  
  declare i64 @le_ml(i64, i64)
  
  declare i64 @g_ml(i64, i64)
  
  declare i64 @ge_ml(i64, i64)
  
  declare i64 @eq_ml(i64, i64)
  
  declare i64 @peq_ml(i64, i64)
  
  declare i64 @neq_ml(i64, i64)
  
  declare i64 @pneq_ml(i64, i64)
  
  declare i64 @print_int(i64)
  
  declare i64 @land_ml(i64, i64)
  
  declare i64 @lor_ml(i64, i64)
  
  declare i64 @mlrt_get_box_field(i64, i64)
  
  declare i64 @mlrt_check_tag(i64, i64)
  
  declare i64 @mltr_match_error(i64)
  
  declare i64 @mlrt_create_tuple(i64, ...)
  
  declare i64 @mlrt_create_empty_closure(i64)
  
  declare i64 @mlrt_apply_args_to_closure(i64, i64, ...)
  
  define i64 @main() {
  entry:
    %0 = call i64 @init_llvm()
    %1 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @ll_var_0 to i64), i64 1)
    store i64 %1, ptr @ll_var_0_glob_llvm, align 4
    %ll_var_1_glob_llvm = call i64 @ll_var_1()
    store i64 %ll_var_1_glob_llvm, ptr @ll_var_1_glob_llvm, align 4
    ret i64 0
  }
  
  define i64 @ll_var_0(i64 %0) {
  entry:
    %"*" = load i64, ptr @mult_mlint_glob_llvm, align 4
    %- = load i64, ptr @minus_mlint_glob_llvm, align 4
    %"<=" = load i64, ptr @le_ml_glob_llvm, align 4
    %1 = call i64 (i64, i64, ...) @mlrt_apply_args_to_closure(i64 %"<=", i64 2, i64 %0, i64 3)
    %2 = ashr i64 %1, 1
    %3 = trunc i64 %2 to i1
    br i1 %3, label %5, label %6
  
  continue:                                         ; preds = %6, %5
    %4 = phi i64 [ 3, %5 ], [ %9, %6 ]
    ret i64 %4
  
  5:                                                ; preds = %entry
    br label %continue
  
  6:                                                ; preds = %entry
    %7 = call i64 (i64, i64, ...) @mlrt_apply_args_to_closure(i64 %-, i64 2, i64 %0, i64 3)
    %8 = call i64 @ll_var_0(i64 %7)
    %9 = call i64 (i64, i64, ...) @mlrt_apply_args_to_closure(i64 %"*", i64 2, i64 %0, i64 %8)
    br label %continue
  }
  
  define i64 @ll_var_1() {
  entry:
    %0 = call i64 @ll_var_0(i64 9)
    %1 = call i64 @print_int(i64 %0)
    ret i64 1
  }

  $ clang-16 output.ll -Wno-override-module -o output_executable ../../lib/runtime.so -lffi
  $ ./output_executable
  24

  $ ./LlvmCodegenRunner.exe < manytests/typed/002fac.ml
  $ clang-16 output.ll -Wno-override-module -o output_executable ../../lib/runtime.so -lffi
  $ ./output_executable
  24

  $ ./LlvmCodegenRunner.exe < manytests/typed/003fib.ml
  $ clang-16 output.ll -Wno-override-module -o output_executable ../../lib/runtime.so -lffi
  $ ./output_executable
  3
  3

  $ ./LlvmCodegenRunner.exe < manytests/typed/004manyargs.ml
  $ clang-16 output.ll -Wno-override-module -o output_executable ../../lib/runtime.so -lffi
  $ ./output_executable
  1111111111
  1
  10
  100

  $ ./LlvmCodegenRunner.exe < manytests/typed/005fix.ml
  $ clang-16 output.ll -Wno-override-module -o output_executable ../../lib/runtime.so -lffi
  $ ./output_executable
  720

  $ ./LlvmCodegenRunner.exe < manytests/typed/006partial.ml
  $ clang-16 output.ll -Wno-override-module -o output_executable ../../lib/runtime.so -lffi
  $ ./output_executable
  1122

  $ ./LlvmCodegenRunner.exe < manytests/typed/006partial2.ml
  $ clang-16 output.ll -Wno-override-module -o output_executable ../../lib/runtime.so -lffi
  $ ./output_executable
  1
  2
  3
  7

  $ ./LlvmCodegenRunner.exe < manytests/typed/006partial3.ml
  $ clang-16 output.ll -Wno-override-module -o output_executable ../../lib/runtime.so -lffi
  $ ./output_executable
  4
  8
  9

  $ ./LlvmCodegenRunner.exe < manytests/typed/007order.ml
  $ clang-16 output.ll -Wno-override-module -o output_executable ../../lib/runtime.so -lffi
  $ ./output_executable
  -1
  4
  2
  1
  103
  -555555
  10000


  $ ./LlvmCodegenRunner.exe < manytests/typed/008ascription.ml
  $ clang-16 output.ll -Wno-override-module -o output_executable ../../lib/runtime.so -lffi
  $ ./output_executable
  8
