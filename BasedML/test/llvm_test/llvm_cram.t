  $ ./llvm_demo.exe << EOF
  > let rec fact_cps n cont =
  > if (n = 0) then
  >  cont 1
  > else
  >  fact_cps (n - 1) (fun acc -> cont (n * acc))
  > let rec map lst f cont = 
  > let res = match lst with
  > | h::tl -> map tl f (fun x -> cont ((f h) :: x))
  > | [] -> cont []
  > in res
  > let map f lst = map lst f (fun x -> x)
  > let print_fac n = fact_cps n print_int 
  > let unit_list = map print_fac [1; 2; 3; 4; 5]
  > EOF


  $ cat out.ll
  ; ModuleID = 'Based_ml'
  source_filename = "Based_ml"
  target triple = "x86_64-pc-linux-gnu"
  
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
  @mlrt_create_cons_glob_llvm = global i64 0
  @ll_0_glob_llvm = global i64 0
  @fact_cps_0_glob_llvm = global i64 0
  @ll_1_glob_llvm = global i64 0
  @map_0_glob_llvm = global i64 0
  @ll_2_glob_llvm = global i64 0
  @map_1_glob_llvm = global i64 0
  @print_fac_0_glob_llvm = global i64 0
  @unit_list_0_glob_llvm = global i64 0
  
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
    %15 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @mlrt_create_cons to i64), i64 2)
    store i64 %15, ptr @mlrt_create_cons_glob_llvm, align 4
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
  
  declare i64 @mlrt_create_cons(i64, i64)
  
  declare i64 @mlrt_get_box_field(i64, i64)
  
  declare i64 @mlrt_check_tag(i64, i64)
  
  declare i64 @mltr_match_error(i64)
  
  declare i64 @mlrt_create_tuple(i64, ...)
  
  declare i64 @mlrt_create_empty_closure(i64)
  
  declare i64 @mlrt_apply_args_to_closure(i64, i64, ...)
  
  define i64 @main() {
  entry:
    %0 = call i64 @init_llvm()
    %1 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @ll_0 to i64), i64 3)
    store i64 %1, ptr @ll_0_glob_llvm, align 4
    %2 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @fact_cps_0 to i64), i64 2)
    store i64 %2, ptr @fact_cps_0_glob_llvm, align 4
    %3 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @ll_1 to i64), i64 4)
    store i64 %3, ptr @ll_1_glob_llvm, align 4
    %4 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @map_0 to i64), i64 3)
    store i64 %4, ptr @map_0_glob_llvm, align 4
    %5 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @ll_2 to i64), i64 1)
    store i64 %5, ptr @ll_2_glob_llvm, align 4
    %6 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @map_1 to i64), i64 2)
    store i64 %6, ptr @map_1_glob_llvm, align 4
    %7 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @print_fac_0 to i64), i64 1)
    store i64 %7, ptr @print_fac_0_glob_llvm, align 4
    %unit_list_0_glob_llvm = call i64 @unit_list_0()
    store i64 %unit_list_0_glob_llvm, ptr @unit_list_0_glob_llvm, align 4
    ret i64 0
  }
  
  define i64 @ll_0(i64 %0, i64 %1, i64 %2) {
  entry:
    %3 = call i64 @mult_mlint(i64 %1, i64 %2)
    %4 = call i64 (i64, i64, ...) @mlrt_apply_args_to_closure(i64 %0, i64 1, i64 %3)
    ret i64 %4
  }
  
  define i64 @fact_cps_0(i64 %0, i64 %1) {
  entry:
    %ll_0 = load i64, ptr @ll_0_glob_llvm, align 4
    %2 = call i64 @eq_ml(i64 %0, i64 1)
    %3 = ashr i64 %2, 1
    %4 = trunc i64 %3 to i1
    br i1 %4, label %6, label %8
  
  continue:                                         ; preds = %8, %6
    %5 = phi i64 [ %7, %6 ], [ %11, %8 ]
    ret i64 %5
  
  6:                                                ; preds = %entry
    %7 = call i64 (i64, i64, ...) @mlrt_apply_args_to_closure(i64 %1, i64 1, i64 3)
    br label %continue
  
  8:                                                ; preds = %entry
    %9 = call i64 (i64, i64, ...) @mlrt_apply_args_to_closure(i64 %ll_0, i64 2, i64 %1, i64 %0)
    %10 = call i64 @minus_mlint(i64 %0, i64 3)
    %11 = call i64 @fact_cps_0(i64 %10, i64 %9)
    br label %continue
  }
  
  define i64 @ll_1(i64 %0, i64 %1, i64 %2, i64 %3) {
  entry:
    %4 = call i64 (i64, i64, ...) @mlrt_apply_args_to_closure(i64 %1, i64 1, i64 %2)
    %5 = call i64 @mlrt_create_cons(i64 %4, i64 %3)
    %6 = call i64 (i64, i64, ...) @mlrt_apply_args_to_closure(i64 %0, i64 1, i64 %5)
    ret i64 %6
  }
  
  define i64 @map_0(i64 %0, i64 %1, i64 %2) {
  entry:
    %ll_1 = load i64, ptr @ll_1_glob_llvm, align 4
    %3 = call i64 @mlrt_check_tag(i64 %0, i64 1)
    %4 = ashr i64 %3, 1
    %5 = trunc i64 %4 to i1
    br i1 %5, label %7, label %12
  
  continue:                                         ; preds = %continue1, %7
    %6 = phi i64 [ %11, %7 ], [ %16, %continue1 ]
    ret i64 %6
  
  7:                                                ; preds = %entry
    %8 = call i64 @mlrt_get_box_field(i64 %0, i64 1)
    %9 = call i64 @mlrt_get_box_field(i64 %0, i64 3)
    %10 = call i64 (i64, i64, ...) @mlrt_apply_args_to_closure(i64 %ll_1, i64 3, i64 %2, i64 %1, i64 %8)
    %11 = call i64 @map_0(i64 %9, i64 %1, i64 %10)
    br label %continue
  
  12:                                               ; preds = %entry
    %13 = call i64 @eq_ml(i64 %0, i64 1)
    %14 = ashr i64 %13, 1
    %15 = trunc i64 %14 to i1
    br i1 %15, label %17, label %19
  
  continue1:                                        ; preds = %19, %17
    %16 = phi i64 [ %18, %17 ], [ %20, %19 ]
    br label %continue
  
  17:                                               ; preds = %12
    %18 = call i64 (i64, i64, ...) @mlrt_apply_args_to_closure(i64 %2, i64 1, i64 1)
    br label %continue1
  
  19:                                               ; preds = %12
    %20 = call i64 @mltr_match_error(i64 1)
    br label %continue1
  }
  
  define i64 @ll_2(i64 %0) {
  entry:
    ret i64 %0
  }
  
  define i64 @map_1(i64 %0, i64 %1) {
  entry:
    %ll_2 = load i64, ptr @ll_2_glob_llvm, align 4
    %2 = call i64 @map_0(i64 %1, i64 %0, i64 %ll_2)
    ret i64 %2
  }
  
  define i64 @print_fac_0(i64 %0) {
  entry:
    %print_int = load i64, ptr @print_int_glob_llvm, align 4
    %1 = call i64 @fact_cps_0(i64 %0, i64 %print_int)
    ret i64 %1
  }
  
  define i64 @unit_list_0() {
  entry:
    %print_fac_0 = load i64, ptr @print_fac_0_glob_llvm, align 4
    %0 = call i64 @mlrt_create_cons(i64 11, i64 1)
    %1 = call i64 @mlrt_create_cons(i64 9, i64 %0)
    %2 = call i64 @mlrt_create_cons(i64 7, i64 %1)
    %3 = call i64 @mlrt_create_cons(i64 5, i64 %2)
    %4 = call i64 @mlrt_create_cons(i64 3, i64 %3)
    %5 = call i64 @map_1(i64 %print_fac_0, i64 %4)
    ret i64 %5
  }

  $ clang-16 out.ll -L../../runtime/ -lmlstd -lmlrt  -o  out.elf
  $ LD_LIBRARY_PATH=$LD_LIBRARY_PATH:../../runtime/ ./out.elf
  120
  24
  6
  2
  1
