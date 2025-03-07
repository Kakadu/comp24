  $ clang -c ../lib/llvm/runtime.c -o runtime.o

  $ ./llvm_compiler_tests.exe < manytests/typed/001fac.ml | tee obaml_llvm.ll ; echo "------" ; clang -Wno-override-module -lffi obaml_llvm.ll runtime.o -o a.out; ./a.out
  ; ModuleID = 'ObaML'
  source_filename = "ObaML"
  target triple = "arm64-apple-darwin23.4.0"
  
  declare ptr @create_int_val(i32)
  
  declare ptr @create_bool_val(i1)
  
  declare ptr @create_string_val(ptr)
  
  declare ptr @mult(ptr, ptr)
  
  declare ptr @divv(ptr, ptr)
  
  declare ptr @plus(ptr, ptr)
  
  declare ptr @minus(ptr, ptr)
  
  declare ptr @eq(ptr, ptr)
  
  declare ptr @req(ptr, ptr)
  
  declare ptr @neq(ptr, ptr)
  
  declare ptr @rneq(ptr, ptr)
  
  declare ptr @lt(ptr, ptr)
  
  declare ptr @lte(ptr, ptr)
  
  declare ptr @gt(ptr, ptr)
  
  declare ptr @gte(ptr, ptr)
  
  declare ptr @uplus(ptr)
  
  declare ptr @uminus(ptr)
  
  declare ptr @lor(ptr, ptr)
  
  declare ptr @land(ptr, ptr)
  
  declare ptr @create_empty_list_val()
  
  declare ptr @add_elem_to_list_val(ptr, ptr)
  
  declare ptr @list_head_getter(ptr)
  
  declare ptr @list_tail_getter(ptr)
  
  declare ptr @list_length_getter(ptr)
  
  declare ptr @create_tuple(i32, ...)
  
  declare ptr @tuple_getter(ptr, ptr)
  
  declare void @matching_failed()
  
  declare void @print_int(ptr)
  
  declare void @print_string(ptr)
  
  declare ptr @create_closure(ptr, i32)
  
  declare ptr @apply_closure(ptr, i32, ...)
  
  declare ptr @call_closure(ptr)
  
  declare i1 @get_i1_val(ptr)
  
  define ptr @fac(ptr %0) {
  entry:
    %empty_closure = call ptr @create_closure(ptr @lte, i32 2)
    %int_var = call ptr @create_int_val(i32 1)
    %apply_closure_res = call ptr (ptr, i32, ...) @apply_closure(ptr %empty_closure, i32 1, ptr %0)
    %apply_closure_res1 = call ptr (ptr, i32, ...) @apply_closure(ptr %apply_closure_res, i32 1, ptr %int_var)
    %cond_i1 = call i1 @get_i1_val(ptr %apply_closure_res1)
    br i1 %cond_i1, label %then, label %else
  
  then:                                             ; preds = %entry
    %int_var2 = call ptr @create_int_val(i32 1)
    br label %merge
  
  else:                                             ; preds = %entry
    %empty_closure3 = call ptr @create_closure(ptr @minus, i32 2)
    %int_var4 = call ptr @create_int_val(i32 1)
    %apply_closure_res5 = call ptr (ptr, i32, ...) @apply_closure(ptr %empty_closure3, i32 1, ptr %0)
    %apply_closure_res6 = call ptr (ptr, i32, ...) @apply_closure(ptr %apply_closure_res5, i32 1, ptr %int_var4)
    %empty_closure7 = call ptr @create_closure(ptr @fac, i32 1)
    %apply_closure_res8 = call ptr (ptr, i32, ...) @apply_closure(ptr %empty_closure7, i32 1, ptr %apply_closure_res6)
    %empty_closure9 = call ptr @create_closure(ptr @mult, i32 2)
    %apply_closure_res10 = call ptr (ptr, i32, ...) @apply_closure(ptr %empty_closure9, i32 1, ptr %0)
    %apply_closure_res11 = call ptr (ptr, i32, ...) @apply_closure(ptr %apply_closure_res10, i32 1, ptr %apply_closure_res8)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %if_res = phi ptr [ %int_var2, %then ], [ %apply_closure_res11, %else ]
    ret ptr %if_res
  }
  
  define i32 @main() {
  entry:
    %empty_closure = call ptr @create_closure(ptr @fac, i32 1)
    %int_var = call ptr @create_int_val(i32 4)
    %apply_closure_res = call ptr (ptr, i32, ...) @apply_closure(ptr %empty_closure, i32 1, ptr %int_var)
    %empty_closure1 = call ptr @create_closure(ptr @print_int, i32 1)
    %apply_closure_res2 = call ptr (ptr, i32, ...) @apply_closure(ptr %empty_closure1, i32 1, ptr %apply_closure_res)
    %int_var3 = call ptr @create_int_val(i32 0)
    ret i32 0
  }
  ------
  24

  $ ./llvm_compiler_tests.exe < manytests/typed/002fac.ml > obaml_llvm.ll ; echo "------" ; clang -Wno-override-module -lffi obaml_llvm.ll runtime.o -o a.out; ./a.out
  ------
  24

  $ ./llvm_compiler_tests.exe < manytests/typed/003fib.ml > obaml_llvm.ll ; echo "------" ; clang -Wno-override-module -lffi obaml_llvm.ll runtime.o -o a.out; ./a.out
  ------
  3
  3

  $ ./llvm_compiler_tests.exe < manytests/typed/004manyargs.ml > obaml_llvm.ll ; echo "------" ; clang -Wno-override-module -lffi obaml_llvm.ll runtime.o -o a.out; ./a.out
  ------
  1111111111
  1
  10
  100

  $ ./llvm_compiler_tests.exe < manytests/typed/005fix.ml > obaml_llvm.ll ; echo "------" ; clang -Wno-override-module -lffi obaml_llvm.ll runtime.o -o a.out; ./a.out
  ------
  720

  $ ./llvm_compiler_tests.exe < manytests/typed/006partial.ml > obaml_llvm.ll ; echo "------" ; clang -Wno-override-module -lffi obaml_llvm.ll runtime.o -o a.out; ./a.out
  ------
  1122

  $ ./llvm_compiler_tests.exe < manytests/typed/006partial2.ml > obaml_llvm.ll ; echo "------" ; clang -Wno-override-module -lffi obaml_llvm.ll runtime.o -o a.out; ./a.out
  ------
  1
  2
  3
  7

  $ ./llvm_compiler_tests.exe < manytests/typed/006partial3.ml > obaml_llvm.ll ; echo "------" ; clang -Wno-override-module -lffi obaml_llvm.ll runtime.o -o a.out; ./a.out
  ------
  4
  8
  9

  $ ./llvm_compiler_tests.exe < manytests/typed/007order.ml > obaml_llvm.ll ; echo "------" ; clang -Wno-override-module -lffi obaml_llvm.ll runtime.o -o a.out; ./a.out
  ------
  -1
  4
  2
  1
  103
  -555555
  10000

  $ ./llvm_compiler_tests.exe < manytests/typed/008ascription.ml > obaml_llvm.ll ; echo "------" ; clang -Wno-override-module -lffi obaml_llvm.ll runtime.o -o a.out; ./a.out
  ------
  8

  $ ./llvm_compiler_tests.exe < manytests/typed/015tuples.ml > obaml_llvm.ll ; echo "------" ; clang -Wno-override-module -lffi obaml_llvm.ll runtime.o -o a.out; ./a.out
  ------
  1
  1
  1
  1

  $ ./llvm_compiler_tests.exe < manytests/typed/016lists.ml > obaml_llvm.ll ; echo "------" ; clang -Wno-override-module -lffi obaml_llvm.ll runtime.o -o a.out; ./a.out
  ------
  1
  2
  3
  8

  $ ./llvm_compiler_tests.exe < manytests/typed/011mapcps.ml > obaml_llvm.ll ; echo "------" ; clang -Wno-override-module -lffi obaml_llvm.ll runtime.o -o a.out; ./a.out
  ------
  2
  3
  4

  $ ./llvm_compiler_tests.exe < manytests/typed/012fibcps.ml > obaml_llvm.ll ; echo "------" ; clang -Wno-override-module -lffi obaml_llvm.ll runtime.o -o a.out; ./a.out
  ------
  8

  $ ./llvm_compiler_tests.exe < manytests/typed/013foldfoldr.ml > obaml_llvm.ll ; echo "------" ; clang -Wno-override-module -lffi obaml_llvm.ll runtime.o -o a.out; ./a.out
  ------
  6

  $ cat > test4.ml <<-EOF 
  > let a = "ObaML"
  > let b = print_string a 
  > let c = print_string a 
  > let main = let () = b in let () = c in 0

  $ ./llvm_compiler_tests.exe < test4.ml > obaml_llvm.ll ; echo "------" ; clang -Wno-override-module -lffi obaml_llvm.ll runtime.o -o a.out; ./a.out
  ------
  ObaML
  ObaML

  $ cat > test5.ml <<-EOF 
  > let ( + ) a b = let () = print_int a in let () = print_int b in a + b;;
  > let main = 5 + 4;;

  $ ./llvm_compiler_tests.exe < test5.ml > obaml_llvm.ll ; echo "------" ; clang -Wno-override-module -lffi obaml_llvm.ll runtime.o -o a.out; ./a.out
  ------
  5
  4
