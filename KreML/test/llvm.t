  $ dune exec llvm_codegen <<- EOF
  > let rec fac n = if n < 1 then 1 else n * fac (n-1)
  ; ModuleID = 'main'
  source_filename = "main"
  
  declare i64 @alloc_tuple(i64 %0)
  
  declare i64 @alloc_closure(i64 %0)
  
  declare i64 @call_closure(i64 %0, i64 %1, i64 %2)
  
  declare void @print_int(i64 %0)
  
  define i64 @fac(i64 %0) {
  entry:
    %n_0 = load i64, i64 %0, align 4
    %t_0 = icmp slt i64 %n_0, 1
    %ifcmp_0 = icmp eq i64 0, i1 %t_0
    br i1 %ifcmp_0, label %else_2, label %then_1
  
  then_1:                                           ; preds = %entry
    br label %merge_7
  
  else_2:                                           ; preds = %entry
    %t_1 = sub i64 %n_0, 1
    %tupled_env_3 = call i64 @alloc_tuple(i64 0)
    %closure_temp_4 = call i64 @alloc_closure(ptr @fac, i64 %tupled_env_3)
    %direct_call_5 = call i64 @fac(i64 %t_1)
    %t_2 = load i64, i64 %direct_call_5, align 4
    %temp_6 = mul i64 %n_0, %t_2
  
  merge_7:                                          ; preds = %then_1
    %phi_8 = phi i64 [ 1, %then_1 ], [ %temp_6, %else_2 ]
    ret i64 %phi_8
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
  
  declare void @print_int(i64 %0)
  
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
    %temp_3 = add i64 %t_1, %x_0
    ret i64 %temp_3
  }
  
  define i64 @f(i64 %0) {
  entry:
    %x_0 = load i64, i64 %0, align 4
    %a_1 = add i64 %x_0, 5
    %tupled_env_4 = call i64 @alloc_tuple(i64 3)
    %temp_arrange_5 = getelementptr i64, i64 %tupled_env_4, 1
    store i64 %temp_arrange_5, i64 %a_1, align 4
    %temp_arrange_6 = getelementptr i64, i64 %tupled_env_4, 2
    store i64 %temp_arrange_6, i64 %x_0, align 4
    %closure_temp_7 = call i64 @alloc_closure(ptr @fresh_fun_0, i64 %tupled_env_4)
    ret i64 %closure_temp_7
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
  void (i64); ModuleID = 'main'
  source_filename = "main"
  
  declare i64 @alloc_tuple(i64 %0)
  
  declare i64 @alloc_closure(i64 %0)
  
  declare i64 @call_closure(i64 %0, i64 %1, i64 %2)
  
  declare void @print_int(i64 %0)
  
  define i64 @inner_3(i64 %0, i64 %1) {
  entry:
    %envptr_0 = getelementptr i64, i64 %0, 0
    %a_1 = load i64, i64 %envptr_0, align 4
    %envptr_1 = getelementptr i64, i64 %0, 1
    %x_0 = load i64, i64 %envptr_1, align 4
    %y_2 = load i64, i64 %1, align 4
    %t_0 = add i64 %a_1, %x_0
    %temp_2 = add i64 %t_0, %y_2
    ret i64 %temp_2
  }
  
  define i64 @f(i64 %0) {
  entry:
    %x_0 = load i64, i64 %0, align 4
    %a_1 = add i64 %x_0, 5
    %tupled_env_3 = call i64 @alloc_tuple(i64 2)
    %temp_arrange_4 = getelementptr i64, i64 %tupled_env_3, 0
    store i64 %temp_arrange_4, i64 %a_1, align 4
    %temp_arrange_5 = getelementptr i64, i64 %tupled_env_3, 1
    store i64 %temp_arrange_5, i64 %x_0, align 4
    %closure_temp_6 = call i64 @alloc_closure(ptr @inner_3, i64 %tupled_env_3)
    ret i64 %closure_temp_6
  }
  
  define i64 @main(void %0) {
  entry:
    %tupled_env_7 = call i64 @alloc_tuple(i64 0)
    %closure_temp_8 = call i64 @alloc_closure(ptr @f, i64 %tupled_env_7)
    %tupled_args_10 = call i64 @alloc_tuple(i64 2)
    %elemptr_11 = getelementptr i64, i64 %tupled_args_10, 0
    store i64 10, i64 %elemptr_11, align 4
    %elemptr_12 = getelementptr i64, i64 %tupled_args_10, 1
    store i64 11, i64 %elemptr_12, align 4
    %call_closure_9 = call i64 @call_closure(i64 %closure_temp_8, i64 %tupled_args_10, i64 2)
    %res_4 = load i64, i64 %call_closure_9, align 4
    call void @print_int(i64 %res_4)
    ret i64 0
  }

  $ dune exec llvm_codegen <<- EOF
  > let map_point f p =
  >   let a, b = p in
  >   f a, f b
  ; ModuleID = 'main'
  source_filename = "main"
  
  declare i64 @alloc_tuple(i64 %0)
  
  declare i64 @alloc_closure(i64 %0)
  
  declare i64 @call_closure(i64 %0, i64 %1, i64 %2)
  
  declare void @print_int(i64 %0)
  
  define i64 @map_point(i64 %0, i64 %1) {
  entry:
    %envptr_0 = getelementptr i64, i64 %0, 0
    %f_0 = load i64, i64 %envptr_0, align 4
    %p_1 = load i64, i64 %1, align 4
    %fieldptr_1 = getelementptr i64, i64 %p_1, 0
    %load_2 = load i64, i64 %fieldptr_1, align 4
    %a_2 = load i64, i64 %load_2, align 4
    %fieldptr_3 = getelementptr i64, i64 %p_1, 1
    %load_4 = load i64, i64 %fieldptr_3, align 4
    %b_3 = load i64, i64 %load_4, align 4
    %tupled_args_6 = call i64 @alloc_tuple(i64 1)
    %elemptr_7 = getelementptr i64, i64 %tupled_args_6, 0
    store i64 %a_2, i64 %elemptr_7, align 4
    %call_closure_5 = call i64 @call_closure(i64 %f_0, i64 %tupled_args_6, i64 1)
    %t_0 = load i64, i64 %call_closure_5, align 4
    %tupled_args_9 = call i64 @alloc_tuple(i64 1)
    %elemptr_10 = getelementptr i64, i64 %tupled_args_9, 0
    store i64 %b_3, i64 %elemptr_10, align 4
    %call_closure_8 = call i64 @call_closure(i64 %f_0, i64 %tupled_args_9, i64 1)
    %t_1 = load i64, i64 %call_closure_8, align 4
    %tuple_11 = call i64 @alloc_tuple(i64 2)
    %temp_tuple_12 = getelementptr i64, i64 %tuple_11, 0
    store i64 %temp_tuple_12, i64 %t_0, align 4
    %temp_tuple_13 = getelementptr i64, i64 %tuple_11, 1
    store i64 %temp_tuple_13, i64 %t_1, align 4
    ret i64 %tuple_11
  }
