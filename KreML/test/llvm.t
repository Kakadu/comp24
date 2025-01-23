  $ dune exec llvm_codegen <<- EOF
  > let rec fac n = if n < 1 then 1 else n * fac (n-1)
  ; ModuleID = 'main'
  source_filename = "main"
  
  declare i64 @alloc_tuple(i64 %0)
  
  declare i64 @alloc_closure(i64 %0)
  
  declare i64 @call_closure(i64 %0, i64 %1, i64 %2)
  
  define i64 @fac(i64 %0) {
  entry:
    %n_0 = load i64, i64 %0, align 4
    %t_0 = icmp slt i64 %n_0, 1
    %ifcmp_0 = icmp eq i64 0, i1 %t_0
    br i1 %ifcmp_0, label %else_2, label %then_1
  
  then_1:                                           ; preds = %entry
    br label %merge_6
  
  else_2:                                           ; preds = %entry
    %t_1 = sub i64 %n_0, 1
    %new_tuple_3 = call i64 @alloc_tuple(i64 0)
    %closure_temp_4 = call i64 @alloc_closure(ptr @fac, i64 %new_tuple_3)
    %temp_call_5 = call i64 @fac(i64 %t_1)
    %t_2 = load i64, i64 %temp_call_5, align 4
    %t_3 = mul i64 %n_0, %t_2
  
  merge_6:                                          ; preds = %then_1
    %phi_7 = phi i64 [ 1, %then_1 ], [ %t_3, %else_2 ]
    ret i64 %phi_7
  }

  $ dune exec llvm_codegen <<- EOF
  > let f x =
  >   let a = x + 5 in
  >   fun y z -> y + z + a + x
  ; ModuleID = 'main'
  source_filename = "main"
  
  declare i64 @alloc_tuple(i64 %0)
  
  declare i64 @alloc_closure(i64 %0)
  
  declare i64 @call_closure(i64 %0, i64 %1, i64 %2)
  
  define i64 @fresh_fun_0(i64 %0, i64 %1) {
  entry:
    %envptr_0 = getelementptr i64, i64 %0, 0
    %y_2 = load i64, i64 %envptr_0, align 4
    %envptr_1 = getelementptr i64, i64 %0, 1
    %a_1 = load i64, i64 %envptr_1, align 4
    %envptr_2 = getelementptr i64, i64 %0, 2
    %x_0 = load i64, i64 %envptr_2, align 4
    %z_3 = load i64, i64 %1, align 4
    %t_0 = add i64 %y_2, %z_3
    %t_1 = add i64 %t_0, %a_1
    %t_2 = add i64 %t_1, %x_0
    ret i64 %t_2
  }
  
  define i64 @f(i64 %0) {
  entry:
    %x_0 = load i64, i64 %0, align 4
    %t_3 = add i64 %x_0, 5
    %a_1 = load i64, i64 %t_3, align 4
    %new_tuple_3 = call i64 @alloc_tuple(i64 3)
    %temp_arrange_4 = getelementptr i64, i64 %new_tuple_3, 1
    store i64 %temp_arrange_4, i64 %a_1, align 4
    %temp_arrange_5 = getelementptr i64, i64 %new_tuple_3, 2
    store i64 %temp_arrange_5, i64 %x_0, align 4
    %closure_temp_6 = call i64 @alloc_closure(ptr @fresh_fun_0, i64 %new_tuple_3)
    ret i64 %closure_temp_6
  }

  $ dune exec llvm_codegen <<- EOF
  > let f x =
  >   let a = x + 5 in
  >   let inner y = a + x + y in
  >   inner
  > 
  > let main =
  >   let res = f 10 11 in
  >   let () = print_int res in
  >   0 
