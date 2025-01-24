  $ dune exec llvm_codegen <<- EOF
  > let rec fac n = if n < 1 then 1 else n * fac (n-1)
  ; ModuleID = 'main'
  source_filename = "main"
  
  declare ptr @alloc_tuple(i64 %0)
  
  declare ptr @alloc_closure(i64 %0)
  
  declare i64 @call_closure(ptr %0, ptr %1, i64 %2)
  
  declare void @print_int(i64 %0)
  
  define i64 @fac(i64 %n_0) {
  entry:
    %t_0 = icmp slt i64 %n_0, 1
    %ifcmp_0 = icmp eq i1 false, %t_0
    br i1 %ifcmp_0, label %else_2, label %then_1
  
  then_1:                                           ; preds = %entry
    br label %merge_7
  
  else_2:                                           ; preds = %entry
    %t_1 = sub i64 %n_0, 1
    %tupled_env_3 = call ptr @alloc_tuple(i64 0)
    %closure_temp_4 = call ptr @alloc_closure(ptr @fac, ptr %tupled_env_3, i64 1)
    %t_2 = call i64 @fac(i64 %t_1)
    %temp_6 = mul i64 %n_0, %t_2
    br label %merge_7
  
  merge_7:                                          ; preds = %else_2, %then_1
    %phi_8 = phi i64 [ 1, %then_1 ], [ %temp_6, %else_2 ]
    ret i64 %phi_8
  }

  $ dune exec llvm_codegen <<- EOF
  > let f x =
  >   let a = x + 5 in
  >   fun y z -> y + z + a + x
  ; ModuleID = 'main'
  source_filename = "main"
  
  declare ptr @alloc_tuple(i64 %0)
  
  declare ptr @alloc_closure(i64 %0)
  
  declare i64 @call_closure(ptr %0, ptr %1, i64 %2)
  
  declare void @print_int(i64 %0)
  
  define i64 @fresh_fun_0(ptr %env, i64 %z_3) {
  entry:
    %envelemptr_0 = getelementptr i64, ptr %env, i64 0
    %y_2 = load i64, ptr %envelemptr_0, align 4
    %envelemptr_1 = getelementptr i64, ptr %env, i64 1
    %a_1 = load i64, ptr %envelemptr_1, align 4
    %envelemptr_2 = getelementptr i64, ptr %env, i64 2
    %x_0 = load i64, ptr %envelemptr_2, align 4
    %t_0 = add i64 %y_2, %z_3
    %t_1 = add i64 %t_0, %a_1
    %temp_3 = add i64 %t_1, %x_0
    ret i64 %temp_3
  }
  
  define i64 @f(i64 %x_0) {
  entry:
    %a_1 = add i64 %x_0, 5
    %tupled_env_4 = call ptr @alloc_tuple(i64 3)
    %envptr_5 = getelementptr i64, ptr %tupled_env_4, i64 1
    store i64 %a_1, ptr %envptr_5, align 4
    %envptr_6 = getelementptr i64, ptr %tupled_env_4, i64 2
    store i64 %x_0, ptr %envptr_6, align 4
    %closure_temp_7 = call ptr @alloc_closure(ptr @fresh_fun_0, ptr %tupled_env_4, i64 4)
    %cast_8 = ptrtoint ptr %closure_temp_7 to i64
    ret i64 %cast_8
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
  ; ModuleID = 'main'
  source_filename = "main"
  
  declare ptr @alloc_tuple(i64 %0)
  
  declare ptr @alloc_closure(i64 %0)
  
  declare i64 @call_closure(ptr %0, ptr %1, i64 %2)
  
  declare void @print_int(i64 %0)
  
  define i64 @inner_3(ptr %env, i64 %y_2) {
  entry:
    %envelemptr_0 = getelementptr i64, ptr %env, i64 0
    %a_1 = load i64, ptr %envelemptr_0, align 4
    %envelemptr_1 = getelementptr i64, ptr %env, i64 1
    %x_0 = load i64, ptr %envelemptr_1, align 4
    %t_0 = add i64 %a_1, %x_0
    %temp_2 = add i64 %t_0, %y_2
    ret i64 %temp_2
  }
  
  define i64 @f(i64 %x_0) {
  entry:
    %a_1 = add i64 %x_0, 5
    %tupled_env_3 = call ptr @alloc_tuple(i64 2)
    %envptr_4 = getelementptr i64, ptr %tupled_env_3, i64 0
    store i64 %a_1, ptr %envptr_4, align 4
    %envptr_5 = getelementptr i64, ptr %tupled_env_3, i64 1
    store i64 %x_0, ptr %envptr_5, align 4
    %closure_temp_6 = call ptr @alloc_closure(ptr @inner_3, ptr %tupled_env_3, i64 3)
    %cast_7 = ptrtoint ptr %closure_temp_6 to i64
    ret i64 %cast_7
  }
  
  define i64 @main() {
  entry:
    %tupled_env_8 = call ptr @alloc_tuple(i64 0)
    %closure_temp_9 = call ptr @alloc_closure(ptr @f, ptr %tupled_env_8, i64 1)
    %tupled_args_11 = call ptr @alloc_tuple(i64 2)
    %elemptr_12 = getelementptr i64, ptr %tupled_args_11, i64 0
    store i64 10, ptr %elemptr_12, align 4
    %elemptr_13 = getelementptr i64, ptr %tupled_args_11, i64 1
    store i64 11, ptr %elemptr_13, align 4
    %res_4 = call i64 @call_closure(ptr %closure_temp_9, ptr %tupled_args_11, i64 2)
    call void @print_int(i64 %res_4)
    ret i64 0
  }

  $ dune exec llvm_codegen <<- EOF
  > let map_point f p =
  >   let a, b = p in
  >   f a, f b
  ; ModuleID = 'main'
  source_filename = "main"
  
  declare ptr @alloc_tuple(i64 %0)
  
  declare ptr @alloc_closure(i64 %0)
  
  declare i64 @call_closure(ptr %0, ptr %1, i64 %2)
  
  declare void @print_int(i64 %0)
  
  define i64 @map_point(ptr %env, i64 %p_1) {
  entry:
    %envelemptr_0 = getelementptr i64, ptr %env, i64 0
    %f_0 = load i64, ptr %envelemptr_0, align 4
    %cast_1 = inttoptr i64 %p_1 to ptr
    %fieldptr_2 = getelementptr i64, ptr %cast_1, i64 0
    %a_2 = load i64, ptr %fieldptr_2, align 4
    %cast_4 = inttoptr i64 %p_1 to ptr
    %fieldptr_5 = getelementptr i64, ptr %cast_4, i64 1
    %b_3 = load i64, ptr %fieldptr_5, align 4
    %cast_7 = inttoptr i64 %f_0 to ptr
    %tupled_args_9 = call ptr @alloc_tuple(i64 1)
    %elemptr_10 = getelementptr i64, ptr %tupled_args_9, i64 0
    store i64 %a_2, ptr %elemptr_10, align 4
    %t_0 = call i64 @call_closure(ptr %cast_7, ptr %tupled_args_9, i64 1)
    %cast_11 = inttoptr i64 %f_0 to ptr
    %tupled_args_13 = call ptr @alloc_tuple(i64 1)
    %elemptr_14 = getelementptr i64, ptr %tupled_args_13, i64 0
    store i64 %b_3, ptr %elemptr_14, align 4
    %t_1 = call i64 @call_closure(ptr %cast_11, ptr %tupled_args_13, i64 1)
    %tuple_15 = call ptr @alloc_tuple(i64 2)
    %temp_tuple_16 = getelementptr i64, ptr %tuple_15, i64 0
    store i64 %t_0, ptr %temp_tuple_16, align 4
    %temp_tuple_17 = getelementptr i64, ptr %tuple_15, i64 1
    store i64 %t_1, ptr %temp_tuple_17, align 4
    %cast_18 = ptrtoint ptr %tuple_15 to i64
    ret i64 %cast_18
  }

  $ dune exec llvm_codegen <<- EOF
  > let rec cps_fac n k =
  >   if n < 1 then k 1 else
  >   cps_fac (n - 1) (fun rec_call -> rec_call * n)
  > let main =
  >   let res =  cps_fac 6 (fun x -> x) in
  >   0
  ; ModuleID = 'main'
  source_filename = "main"
  
  declare ptr @alloc_tuple(i64 %0)
  
  declare ptr @alloc_closure(i64 %0)
  
  declare i64 @call_closure(ptr %0, ptr %1, i64 %2)
  
  declare void @print_int(i64 %0)
  
  define i64 @fresh_fun_0(ptr %env, i64 %rec_call_2) {
  entry:
    %envelemptr_0 = getelementptr i64, ptr %env, i64 0
    %n_0 = load i64, ptr %envelemptr_0, align 4
    %temp_1 = mul i64 %rec_call_2, %n_0
    ret i64 %temp_1
  }
  
  define i64 @cps_fac(ptr %env, i64 %k_1) {
  entry:
    %envelemptr_2 = getelementptr i64, ptr %env, i64 0
    %n_0 = load i64, ptr %envelemptr_2, align 4
    %t_0 = icmp slt i64 %n_0, 1
    %ifcmp_3 = icmp eq i1 false, %t_0
    br i1 %ifcmp_3, label %else_9, label %then_4
  
  then_4:                                           ; preds = %entry
    %cast_5 = inttoptr i64 %k_1 to ptr
    %tupled_args_7 = call ptr @alloc_tuple(i64 1)
    %elemptr_8 = getelementptr i64, ptr %tupled_args_7, i64 0
    store i64 1, ptr %elemptr_8, align 4
    %call_closure_6 = call i64 @call_closure(ptr %cast_5, ptr %tupled_args_7, i64 1)
    br label %merge_19
  
  else_9:                                           ; preds = %entry
    %t_2 = sub i64 %n_0, 1
    %tupled_env_10 = call ptr @alloc_tuple(i64 1)
    %closure_temp_11 = call ptr @alloc_closure(ptr @cps_fac, ptr %tupled_env_10, i64 2)
    %tupled_env_12 = call ptr @alloc_tuple(i64 1)
    %envptr_13 = getelementptr i64, ptr %tupled_env_12, i64 0
    store i64 %n_0, ptr %envptr_13, align 4
    %closure_temp_14 = call ptr @alloc_closure(ptr @fresh_fun_0, ptr %tupled_env_12, i64 2)
    %tupled_args_16 = call ptr @alloc_tuple(i64 2)
    %elemptr_17 = getelementptr i64, ptr %tupled_args_16, i64 0
    store i64 %t_2, ptr %elemptr_17, align 4
    %elemptr_18 = getelementptr i64, ptr %tupled_args_16, i64 1
    store ptr %closure_temp_14, ptr %elemptr_18, align 8
    %call_closure_15 = call i64 @call_closure(ptr %closure_temp_11, ptr %tupled_args_16, i64 2)
    br label %merge_19
  
  merge_19:                                         ; preds = %else_9, %then_4
    %phi_20 = phi i64 [ %call_closure_6, %then_4 ], [ %call_closure_15, %else_9 ]
    ret i64 %phi_20
  }
  
  define i64 @fresh_fun_1(i64 %x_3) {
  entry:
    ret i64 %x_3
  }
  
  define i64 @main() {
  entry:
    %tupled_env_21 = call ptr @alloc_tuple(i64 1)
    %closure_temp_22 = call ptr @alloc_closure(ptr @cps_fac, ptr %tupled_env_21, i64 2)
    %tupled_env_23 = call ptr @alloc_tuple(i64 0)
    %closure_temp_24 = call ptr @alloc_closure(ptr @fresh_fun_1, ptr %tupled_env_23, i64 1)
    %tupled_args_26 = call ptr @alloc_tuple(i64 2)
    %elemptr_27 = getelementptr i64, ptr %tupled_args_26, i64 0
    store i64 6, ptr %elemptr_27, align 4
    %elemptr_28 = getelementptr i64, ptr %tupled_args_26, i64 1
    store ptr %closure_temp_24, ptr %elemptr_28, align 8
    %res_4 = call i64 @call_closure(ptr %closure_temp_22, ptr %tupled_args_26, i64 2)
    ret i64 0
  }
