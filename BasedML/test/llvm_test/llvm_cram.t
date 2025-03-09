Some init for riscv
  $ ln -s ../../runtime/riscv/libffi.so ../../runtime/riscv/libffi.so.8

Other 
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
  target triple = "riscv64-unknown-linux-gnu"
  
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
  @mlrt_compact_glob_llvm = global i64 0
  @mlrt_print_gc_info_glob_llvm = global i64 0
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
    %0 = call i64 (i64, ...) @mlrt_handle_global_vars(i64 26, i64 ptrtoint (ptr @unit_list_0_glob_llvm to i64), i64 ptrtoint (ptr @print_fac_0_glob_llvm to i64), i64 ptrtoint (ptr @map_1_glob_llvm to i64), i64 ptrtoint (ptr @ll_2_glob_llvm to i64), i64 ptrtoint (ptr @map_0_glob_llvm to i64), i64 ptrtoint (ptr @ll_1_glob_llvm to i64), i64 ptrtoint (ptr @fact_cps_0_glob_llvm to i64), i64 ptrtoint (ptr @ll_0_glob_llvm to i64), i64 ptrtoint (ptr @mlrt_print_gc_info_glob_llvm to i64), i64 ptrtoint (ptr @mlrt_compact_glob_llvm to i64), i64 ptrtoint (ptr @mlrt_create_cons_glob_llvm to i64), i64 ptrtoint (ptr @lor_ml_glob_llvm to i64), i64 ptrtoint (ptr @land_ml_glob_llvm to i64), i64 ptrtoint (ptr @print_int_glob_llvm to i64), i64 ptrtoint (ptr @pneq_ml_glob_llvm to i64), i64 ptrtoint (ptr @neq_ml_glob_llvm to i64), i64 ptrtoint (ptr @peq_ml_glob_llvm to i64), i64 ptrtoint (ptr @eq_ml_glob_llvm to i64), i64 ptrtoint (ptr @ge_ml_glob_llvm to i64), i64 ptrtoint (ptr @g_ml_glob_llvm to i64), i64 ptrtoint (ptr @le_ml_glob_llvm to i64), i64 ptrtoint (ptr @l_ml_glob_llvm to i64), i64 ptrtoint (ptr @div_mlint_glob_llvm to i64), i64 ptrtoint (ptr @mult_mlint_glob_llvm to i64), i64 ptrtoint (ptr @minus_mlint_glob_llvm to i64), i64 ptrtoint (ptr @plus_mlint_glob_llvm to i64))
    %1 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @plus_mlint to i64), i64 2)
    store i64 %1, ptr @plus_mlint_glob_llvm, align 4
    %2 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @minus_mlint to i64), i64 2)
    store i64 %2, ptr @minus_mlint_glob_llvm, align 4
    %3 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @mult_mlint to i64), i64 2)
    store i64 %3, ptr @mult_mlint_glob_llvm, align 4
    %4 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @div_mlint to i64), i64 2)
    store i64 %4, ptr @div_mlint_glob_llvm, align 4
    %5 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @l_ml to i64), i64 2)
    store i64 %5, ptr @l_ml_glob_llvm, align 4
    %6 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @le_ml to i64), i64 2)
    store i64 %6, ptr @le_ml_glob_llvm, align 4
    %7 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @g_ml to i64), i64 2)
    store i64 %7, ptr @g_ml_glob_llvm, align 4
    %8 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @ge_ml to i64), i64 2)
    store i64 %8, ptr @ge_ml_glob_llvm, align 4
    %9 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @eq_ml to i64), i64 2)
    store i64 %9, ptr @eq_ml_glob_llvm, align 4
    %10 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @peq_ml to i64), i64 2)
    store i64 %10, ptr @peq_ml_glob_llvm, align 4
    %11 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @neq_ml to i64), i64 2)
    store i64 %11, ptr @neq_ml_glob_llvm, align 4
    %12 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @pneq_ml to i64), i64 2)
    store i64 %12, ptr @pneq_ml_glob_llvm, align 4
    %13 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    store i64 %13, ptr @print_int_glob_llvm, align 4
    %14 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @land_ml to i64), i64 2)
    store i64 %14, ptr @land_ml_glob_llvm, align 4
    %15 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @lor_ml to i64), i64 2)
    store i64 %15, ptr @lor_ml_glob_llvm, align 4
    %16 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @mlrt_create_cons to i64), i64 2)
    store i64 %16, ptr @mlrt_create_cons_glob_llvm, align 4
    %17 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @mlrt_compact to i64), i64 1)
    store i64 %17, ptr @mlrt_compact_glob_llvm, align 4
    %18 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @mlrt_print_gc_info to i64), i64 1)
    store i64 %18, ptr @mlrt_print_gc_info_glob_llvm, align 4
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
  
  declare i64 @mlrt_handle_global_vars(i64, ...)
  
  declare i64 @mlrt_compact(i64)
  
  declare i64 @mlrt_print_gc_info(i64)
  
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
    %6 = phi i64 [ %11, %7 ], [ %19, %continue1 ]
    ret i64 %6
  
  7:                                                ; preds = %entry
    %8 = call i64 @mlrt_get_box_field(i64 %0, i64 1)
    %9 = call i64 @mlrt_get_box_field(i64 %0, i64 3)
    %10 = call i64 (i64, i64, ...) @mlrt_apply_args_to_closure(i64 %ll_1, i64 3, i64 %2, i64 %1, i64 %8)
    %11 = call i64 @map_0(i64 %9, i64 %1, i64 %10)
    br label %continue
  
  12:                                               ; preds = %entry
    %13 = icmp eq i64 %0, 1
    %14 = zext i1 %13 to i64
    %15 = shl i64 %14, 1
    %16 = add i64 %15, 1
    %17 = ashr i64 %16, 1
    %18 = trunc i64 %17 to i1
    br i1 %18, label %20, label %22
  
  continue1:                                        ; preds = %22, %20
    %19 = phi i64 [ %21, %20 ], [ %23, %22 ]
    br label %continue
  
  20:                                               ; preds = %12
    %21 = call i64 (i64, i64, ...) @mlrt_apply_args_to_closure(i64 %2, i64 1, i64 1)
    br label %continue1
  
  22:                                               ; preds = %12
    %23 = call i64 @mltr_match_error(i64 1)
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

  $ clang-16 -w out.ll -L../../runtime/ -lmlstd -lmlrt  -o  out.elf
  $ LD_LIBRARY_PATH=$LD_LIBRARY_PATH:../../runtime/ ./out.elf
  120
  24
  6
  2
  1

  $ ./riscv_run.sh  out.ll
  120
  24
  6
  2
  1

Operation/Function overide
  $ ./llvm_demo.exe << EOF
  > let _ = print_int (3 + 5)
  > let ( + ) a b = 10*a+b
  > let _ = print_int (3 + 5)
  > let print_int (a, b) = let () = print_int a in print_int b
  > let ( + ) (a, b) (c, d) = (a+c, b+d)
  > let _ = print_int ((1, 2) + (3, 4))
  $ ./riscv_run.sh  out.ll
  8
  35
  13
  41

Test struct and physic equal
  $ ./llvm_demo.exe << EOF
  > let print_bool b = print_int (if b then 1 else 0)
  > let lst1 = [1; 2; 3]
  > let lst2 = [1; 2; 3]
  > let lst3 = [1; 2; 2]
  > let _ = print_bool (lst1 == lst2)
  > let _ = print_bool (lst1 = lst2)
  > let _ = print_bool (2 == 1)
  > let _ = print_bool (2 = 1)
  > let _ = print_bool (1 == 1)
  > let _ = print_bool (1 = 1)
  > let _ = print_bool (lst3 <> lst2)

  $ ./riscv_run.sh  out.ll
  0
  1
  0
  0
  1
  1
  1

Runtime lor, land 
  $ ./llvm_demo.exe << EOF
  > let print_bool b = print_int (if b then 1 else 0)
  > let and_id = ( && ) true
  > let _ = print_bool (and_id false)
  > let _ = print_bool (and_id true)
  $ ./riscv_run.sh  out.ll
  0
  1

Manytests
  $ ./llvm_demo.exe < ../manytests/typed/001fac.ml
  $ ./riscv_run.sh  out.ll
  24
  $ ocaml -w -a ../manytests/typed/001fac.ml 
  24
  $ ./llvm_demo.exe < ../manytests/typed/002fac.ml
  $ ./riscv_run.sh  out.ll
  24
  $ ocaml -w -a ../manytests/typed/002fac.ml 
  24
  $ ./llvm_demo.exe < ../manytests/typed/003fib.ml
  $ ./riscv_run.sh  out.ll
  3
  3
  $ ocaml -w -a ../manytests/typed/003fib.ml
  33
  $ ./llvm_demo.exe < ../manytests/typed/004manyargs.ml
  $ ./riscv_run.sh  out.ll
  1111111111
  1
  10
  100
  $ ocaml -w -a ../manytests/typed/004manyargs.ml
  1111111111110100
  $ ./llvm_demo.exe < ../manytests/typed/005fix.ml
  $ ./riscv_run.sh  out.ll
  720
  $ ocaml -w -a ../manytests/typed/005fix.ml
  720

  $ ./llvm_demo.exe < ../manytests/typed/006partial.ml
  $ ./riscv_run.sh  out.ll
  1122

  $ ocaml -w -a ../manytests/typed/006partial.ml
  1122
  $ ./llvm_demo.exe < ../manytests/typed/006partial2.ml
  $ ./riscv_run.sh  out.ll
  1
  2
  3
  7
  $ ocaml -w -a ../manytests/typed/006partial2.ml
  1237
  $ ./llvm_demo.exe < ../manytests/typed/006partial3.ml
  $ ./riscv_run.sh  out.ll
  4
  8
  9

  $ ocaml -w -a ../manytests/typed/006partial3.ml
  489
  $ ./llvm_demo.exe < ../manytests/typed/007order.ml
  $ ./riscv_run.sh  out.ll
  -1
  4
  2
  1
  103
  -555555
  10000
  $ ocaml -w -a ../manytests/typed/007order.ml
  -1421103-55555510000
  $ ./llvm_demo.exe < ../manytests/typed/008ascription.ml
  $ ./riscv_run.sh  out.ll
  8
  $ ocaml -w -a ../manytests/typed/008ascription.ml
  8
No output :(
  $ ./llvm_demo.exe < ../manytests/typed/009let_poly.ml
  $ ./riscv_run.sh  out.ll
  $ ocaml -w -a ../manytests/typed/009let_poly.ml
TU Andrey
  $ ./llvm_demo.exe < ../manytests/typed/010sukharev.ml
  Parser error: : end_of_input
  $ ./riscv_run.sh  out.ll
  $ ocaml -w -a ../manytests/typed/010sukharev.ml
  File "../manytests/typed/010sukharev.ml", line 9, characters 15-22:
  9 |     let a, _ = 1, 2, 3 in a
                     ^^^^^^^
  Error: This expression has type 'a * 'b * 'c
         but an expression was expected of type 'd * 'e
  [2]
  $ ./llvm_demo.exe < ../manytests/typed/011mapcps.ml
  $ ./riscv_run.sh  out.ll
  2
  3
  4
  $ ocaml -w -a ../manytests/typed/011mapcps.ml
  234
  $ ./llvm_demo.exe < ../manytests/typed/012fibcps.ml
  $ ./riscv_run.sh  out.ll
  8
  $ ocaml -w -a ../manytests/typed/012fibcps.ml
  8
  $ ./llvm_demo.exe < ../manytests/typed/013foldfoldr.ml
  $ ./riscv_run.sh  out.ll
  6
  $ ocaml -w -a ../manytests/typed/013foldfoldr.ml
  6
  $ ./llvm_demo.exe < ../manytests/typed/015tuples.ml
  $ ./riscv_run.sh  out.ll
  1
  1
  Segmentation fault (core dumped)
  [139]
  $ ocaml -w -a ../manytests/typed/015tuples.ml
  1111
  $ ./llvm_demo.exe < ../manytests/typed/016lists.ml
  $ ./riscv_run.sh  out.ll
  1
  2
  3
  8
  $ ocaml -w -a ../manytests/typed/016lists.ml
  1238

Some llcode demonstration 
  $ filter_output() { grep -E 'source_filename|target datalayout|ModuleID|global |declare|target' --invert-match |  sed '/define i64 @init_llvm() {/,/}/d' |  grep -v '^[[:space:]]*$'; } 
  $ ./llvm_demo.exe << EOF
  > let f a b = a*1 + b + 0 
  > let _ = print_int (f 2 3)

  $ cat out.ll | filter_output
  define i64 @main() {
  entry:
    %0 = call i64 @init_llvm()
    %1 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @f_0 to i64), i64 2)
    store i64 %1, ptr @f_0_glob_llvm, align 4
    %global_wildcard0_glob_llvm = call i64 @global_wildcard0()
    store i64 %global_wildcard0_glob_llvm, ptr @global_wildcard0_glob_llvm, align 4
    ret i64 0
  }
  define i64 @f_0(i64 %0, i64 %1) {
  entry:
    %2 = call i64 @plus_mlint(i64 %1, i64 %0)
    ret i64 %2
  }
  define i64 @global_wildcard0() {
  entry:
    %0 = call i64 @f_0(i64 5, i64 7)
    %1 = call i64 @print_int(i64 %0)
    ret i64 %1
  }


  $ ./llvm_demo.exe << EOF
  > let f a = a*1*1*1*1*1*1*1
  > let _ = print_int (f 2)

  $ cat out.ll | filter_output
  define i64 @main() {
  entry:
    %0 = call i64 @init_llvm()
    %1 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @f_0 to i64), i64 1)
    store i64 %1, ptr @f_0_glob_llvm, align 4
    %global_wildcard0_glob_llvm = call i64 @global_wildcard0()
    store i64 %global_wildcard0_glob_llvm, ptr @global_wildcard0_glob_llvm, align 4
    ret i64 0
  }
  define i64 @f_0(i64 %0) {
  entry:
    %1 = call i64 @mult_mlint(i64 %0, i64 3)
    ret i64 %1
  }
  define i64 @global_wildcard0() {
  entry:
    %0 = call i64 @f_0(i64 5)
    %1 = call i64 @print_int(i64 %0)
    ret i64 %1
  }


  $ ./llvm_demo.exe << EOF
  > let f a b = a == b 
  > let bool1 = f 2 3
  $ cat out.ll | filter_output
  define i64 @main() {
  entry:
    %0 = call i64 @init_llvm()
    %1 = call i64 @mlrt_create_empty_closure(i64 ptrtoint (ptr @f_0 to i64), i64 2)
    store i64 %1, ptr @f_0_glob_llvm, align 4
    %bool1_0_glob_llvm = call i64 @bool1_0()
    store i64 %bool1_0_glob_llvm, ptr @bool1_0_glob_llvm, align 4
    ret i64 0
  }
  define i64 @f_0(i64 %0, i64 %1) {
  entry:
    %2 = icmp eq i64 %0, %1
    %3 = zext i1 %2 to i64
    %4 = shl i64 %3, 1
    %5 = add i64 %4, 1
    ret i64 %5
  }
  define i64 @bool1_0() {
  entry:
    %0 = call i64 @f_0(i64 5, i64 7)
    ret i64 %0
  }
