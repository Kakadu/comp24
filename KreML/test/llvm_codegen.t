  $ dune exec llvm_codegen <<- EOF
  > let rec fac n = if n < 1 then 1 else n * fac (n-1)
  ; ModuleID = 'main'
  source_filename = "main"
  
  @nil = global ptr null
  @unit = global i64 0
  
  declare ptr @alloc_tuple(i64 %0)
  
  declare ptr @alloc_closure(ptr %0, ptr %1, i64 %2, i64 %3)
  
  declare i64 @call_closure(ptr %0, ptr %1, i64 %2)
  
  declare i64 @print_int(i64 %0)
  
  declare ptr @list_cons(i64 %0, ptr %1)
  
  declare void @partial_match(i64 %0)
  
  define i64 @fac(i64 %n_0) {
  entry:
    %t_0 = icmp slt i64 %n_0, 1
    %ifcmp_0 = icmp eq i1 %t_0, false
    br i1 %ifcmp_0, label %else_2, label %then_1
  
  then_1:                                           ; preds = %entry
    br label %merge_3
  
  else_2:                                           ; preds = %entry
    %t_1 = sub i64 %n_0, 1
    %t_2 = call i64 @fac(i64 %t_1)
    %t_21 = mul i64 %n_0, %t_2
    br label %merge_3
  
  merge_3:                                          ; preds = %else_2, %then_1
    %phi_4 = phi i64 [ 1, %then_1 ], [ %t_21, %else_2 ]
    ret i64 %phi_4
  }

  $ dune exec llvm_codegen <<- EOF
  > let f x =
  >   let a = x + 5 in
  >   fun y z -> y + z + a + x
  ; ModuleID = 'main'
  source_filename = "main"
  
  @nil = global ptr null
  @unit = global i64 0
  
  declare ptr @alloc_tuple(i64 %0)
  
  declare ptr @alloc_closure(ptr %0, ptr %1, i64 %2, i64 %3)
  
  declare i64 @call_closure(ptr %0, ptr %1, i64 %2)
  
  declare i64 @print_int(i64 %0)
  
  declare ptr @list_cons(i64 %0, ptr %1)
  
  declare void @partial_match(i64 %0)
  
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
    %t_11 = add i64 %t_1, %x_0
    ret i64 %t_11
  }
  
  define i64 @f(i64 %x_0) {
  entry:
    %a_1 = add i64 %x_0, 5
    %tupled_env_3 = call ptr @alloc_tuple(i64 3)
    %envptr_4 = getelementptr i64, ptr %tupled_env_3, i64 1
    store i64 %a_1, ptr %envptr_4, align 4
    %envptr_5 = getelementptr i64, ptr %tupled_env_3, i64 2
    store i64 %x_0, ptr %envptr_5, align 4
    %closure_temp_6 = call ptr @alloc_closure(ptr @fresh_fun_0, ptr %tupled_env_3, i64 2, i64 3)
    %cast_7 = ptrtoint ptr %closure_temp_6 to i64
    ret i64 %cast_7
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
  
  @nil = global ptr null
  @unit = global i64 0
  
  declare ptr @alloc_tuple(i64 %0)
  
  declare ptr @alloc_closure(ptr %0, ptr %1, i64 %2, i64 %3)
  
  declare i64 @call_closure(ptr %0, ptr %1, i64 %2)
  
  declare i64 @print_int(i64 %0)
  
  declare ptr @list_cons(i64 %0, ptr %1)
  
  declare void @partial_match(i64 %0)
  
  define i64 @inner_3(ptr %env, i64 %y_2) {
  entry:
    %envelemptr_0 = getelementptr i64, ptr %env, i64 0
    %a_1 = load i64, ptr %envelemptr_0, align 4
    %envelemptr_1 = getelementptr i64, ptr %env, i64 1
    %x_0 = load i64, ptr %envelemptr_1, align 4
    %t_0 = add i64 %a_1, %x_0
    %t_01 = add i64 %t_0, %y_2
    ret i64 %t_01
  }
  
  define i64 @f(i64 %x_0) {
  entry:
    %a_1 = add i64 %x_0, 5
    %tupled_env_2 = call ptr @alloc_tuple(i64 2)
    %envptr_3 = getelementptr i64, ptr %tupled_env_2, i64 0
    store i64 %a_1, ptr %envptr_3, align 4
    %envptr_4 = getelementptr i64, ptr %tupled_env_2, i64 1
    store i64 %x_0, ptr %envptr_4, align 4
    %closure_temp_5 = call ptr @alloc_closure(ptr @inner_3, ptr %tupled_env_2, i64 1, i64 2)
    %cast_6 = ptrtoint ptr %closure_temp_5 to i64
    ret i64 %cast_6
  }
  
  define i64 @main() {
  entry:
    %closure_temp_8 = call ptr @alloc_closure(ptr @f, ptr @nil, i64 1, i64 0)
    %tupled_args_10 = call ptr @alloc_tuple(i64 2)
    %elemptr_11 = getelementptr i64, ptr %tupled_args_10, i64 0
    store i64 10, ptr %elemptr_11, align 4
    %elemptr_12 = getelementptr i64, ptr %tupled_args_10, i64 1
    store i64 11, ptr %elemptr_12, align 4
    %res_4 = call i64 @call_closure(ptr %closure_temp_8, ptr %tupled_args_10, i64 2)
    %0 = call i64 @print_int(i64 %res_4)
    ret i64 0
  }

  $ dune exec llvm_codegen <<- EOF
  > let map_point f p =
  >   let a, b = p in
  >   f a, f b
  ; ModuleID = 'main'
  source_filename = "main"
  
  @nil = global ptr null
  @unit = global i64 0
  
  declare ptr @alloc_tuple(i64 %0)
  
  declare ptr @alloc_closure(ptr %0, ptr %1, i64 %2, i64 %3)
  
  declare i64 @call_closure(ptr %0, ptr %1, i64 %2)
  
  declare i64 @print_int(i64 %0)
  
  declare ptr @list_cons(i64 %0, ptr %1)
  
  declare void @partial_match(i64 %0)
  
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
    %t_11 = call ptr @alloc_tuple(i64 2)
    %elemptr_15 = getelementptr i64, ptr %t_11, i64 0
    store i64 %t_0, ptr %elemptr_15, align 4
    %elemptr_16 = getelementptr i64, ptr %t_11, i64 1
    store i64 %t_1, ptr %elemptr_16, align 4
    %cast_17 = ptrtoint ptr %t_11 to i64
    ret i64 %cast_17
  }

  $ dune exec llvm_codegen <<- EOF
  > let rec cps_fac n k =
  >   if n < 1 then k 1 else
  >   cps_fac (n - 1) (fun rec_call -> k (rec_call * n))
  > let main =
  >   let res =  cps_fac 6 (fun x -> x) in
  >   0
  ; ModuleID = 'main'
  source_filename = "main"
  
  @nil = global ptr null
  @unit = global i64 0
  
  declare ptr @alloc_tuple(i64 %0)
  
  declare ptr @alloc_closure(ptr %0, ptr %1, i64 %2, i64 %3)
  
  declare i64 @call_closure(ptr %0, ptr %1, i64 %2)
  
  declare i64 @print_int(i64 %0)
  
  declare ptr @list_cons(i64 %0, ptr %1)
  
  declare void @partial_match(i64 %0)
  
  define i64 @fresh_fun_0(ptr %env, i64 %rec_call_2) {
  entry:
    %envelemptr_0 = getelementptr i64, ptr %env, i64 0
    %k_1 = load i64, ptr %envelemptr_0, align 4
    %envelemptr_1 = getelementptr i64, ptr %env, i64 1
    %n_0 = load i64, ptr %envelemptr_1, align 4
    %t_4 = mul i64 %rec_call_2, %n_0
    %cast_2 = inttoptr i64 %k_1 to ptr
    %tupled_args_4 = call ptr @alloc_tuple(i64 1)
    %elemptr_5 = getelementptr i64, ptr %tupled_args_4, i64 0
    store i64 %t_4, ptr %elemptr_5, align 4
    %call_closure_3 = call i64 @call_closure(ptr %cast_2, ptr %tupled_args_4, i64 1)
    ret i64 %call_closure_3
  }
  
  define i64 @cps_fac(ptr %env, i64 %k_1) {
  entry:
    %envelemptr_6 = getelementptr i64, ptr %env, i64 0
    %n_0 = load i64, ptr %envelemptr_6, align 4
    %t_0 = icmp slt i64 %n_0, 1
    %ifcmp_7 = icmp eq i1 %t_0, false
    br i1 %ifcmp_7, label %else_13, label %then_8
  
  then_8:                                           ; preds = %entry
    %cast_9 = inttoptr i64 %k_1 to ptr
    %tupled_args_11 = call ptr @alloc_tuple(i64 1)
    %elemptr_12 = getelementptr i64, ptr %tupled_args_11, i64 0
    store i64 1, ptr %elemptr_12, align 4
    %call_closure_10 = call i64 @call_closure(ptr %cast_9, ptr %tupled_args_11, i64 1)
    br label %merge_24
  
  else_13:                                          ; preds = %entry
    %t_2 = sub i64 %n_0, 1
    %tupled_env_14 = call ptr @alloc_tuple(i64 2)
    %envptr_15 = getelementptr i64, ptr %tupled_env_14, i64 0
    store i64 %k_1, ptr %envptr_15, align 4
    %envptr_16 = getelementptr i64, ptr %tupled_env_14, i64 1
    store i64 %n_0, ptr %envptr_16, align 4
    %closure_temp_17 = call ptr @alloc_closure(ptr @fresh_fun_0, ptr %tupled_env_14, i64 1, i64 2)
    %tupled_env_18 = call ptr @alloc_tuple(i64 1)
    %closure_temp_19 = call ptr @alloc_closure(ptr @cps_fac, ptr %tupled_env_18, i64 2, i64 1)
    %tupled_args_21 = call ptr @alloc_tuple(i64 2)
    %elemptr_22 = getelementptr i64, ptr %tupled_args_21, i64 0
    store i64 %t_2, ptr %elemptr_22, align 4
    %elemptr_23 = getelementptr i64, ptr %tupled_args_21, i64 1
    store ptr %closure_temp_17, ptr %elemptr_23, align 8
    %call_closure_20 = call i64 @call_closure(ptr %closure_temp_19, ptr %tupled_args_21, i64 2)
    br label %merge_24
  
  merge_24:                                         ; preds = %else_13, %then_8
    %phi_25 = phi i64 [ %call_closure_10, %then_8 ], [ %call_closure_20, %else_13 ]
    ret i64 %phi_25
  }
  
  define i64 @fresh_fun_1(i64 %x_3) {
  entry:
    ret i64 %x_3
  }
  
  define i64 @main() {
  entry:
    %closure_temp_27 = call ptr @alloc_closure(ptr @fresh_fun_1, ptr @nil, i64 1, i64 0)
    %tupled_env_28 = call ptr @alloc_tuple(i64 1)
    %closure_temp_29 = call ptr @alloc_closure(ptr @cps_fac, ptr %tupled_env_28, i64 2, i64 1)
    %tupled_args_31 = call ptr @alloc_tuple(i64 2)
    %elemptr_32 = getelementptr i64, ptr %tupled_args_31, i64 0
    store i64 6, ptr %elemptr_32, align 4
    %elemptr_33 = getelementptr i64, ptr %tupled_args_31, i64 1
    store ptr %closure_temp_27, ptr %elemptr_33, align 8
    %res_4 = call i64 @call_closure(ptr %closure_temp_29, ptr %tupled_args_31, i64 2)
    ret i64 0
  }

  $ dune exec llvm_codegen <<- EOF
  > let rec f x  =
  > if (x < 1) then 1 else f (x-1) * x
  > let main =
  > let id x = x in
  > let () = print_int (f 6) in
  > 0
  ; ModuleID = 'main'
  source_filename = "main"
  
  @nil = global ptr null
  @unit = global i64 0
  
  declare ptr @alloc_tuple(i64 %0)
  
  declare ptr @alloc_closure(ptr %0, ptr %1, i64 %2, i64 %3)
  
  declare i64 @call_closure(ptr %0, ptr %1, i64 %2)
  
  declare i64 @print_int(i64 %0)
  
  declare ptr @list_cons(i64 %0, ptr %1)
  
  declare void @partial_match(i64 %0)
  
  define i64 @f(i64 %x_0) {
  entry:
    %t_0 = icmp slt i64 %x_0, 1
    %ifcmp_0 = icmp eq i1 %t_0, false
    br i1 %ifcmp_0, label %else_2, label %then_1
  
  then_1:                                           ; preds = %entry
    br label %merge_3
  
  else_2:                                           ; preds = %entry
    %t_1 = sub i64 %x_0, 1
    %t_2 = call i64 @f(i64 %t_1)
    %t_21 = mul i64 %t_2, %x_0
    br label %merge_3
  
  merge_3:                                          ; preds = %else_2, %then_1
    %phi_4 = phi i64 [ 1, %then_1 ], [ %t_21, %else_2 ]
    ret i64 %phi_4
  }
  
  define i64 @id_2(i64 %x_1) {
  entry:
    ret i64 %x_1
  }
  
  define i64 @main() {
  entry:
    %t_4 = call i64 @f(i64 6)
    %0 = call i64 @print_int(i64 %t_4)
    ret i64 0
  }

  $ dune exec llvm_codegen <<- EOF
  > let a = 1
  > let f x =
  >   let b = 2 in
  >   fun y -> y + a + x + b
  > let main =
  >   let () = print_int (f 3 4) in
  >   0
  ; ModuleID = 'main'
  source_filename = "main"
  
  @nil = global ptr null
  @unit = global i64 0
  
  declare ptr @alloc_tuple(i64 %0)
  
  declare ptr @alloc_closure(ptr %0, ptr %1, i64 %2, i64 %3)
  
  declare i64 @call_closure(ptr %0, ptr %1, i64 %2)
  
  declare i64 @print_int(i64 %0)
  
  declare ptr @list_cons(i64 %0, ptr %1)
  
  declare void @partial_match(i64 %0)
  
  define i64 @a() {
  entry:
    ret i64 1
  }
  
  define i64 @fresh_fun_0(ptr %env, i64 %y_2) {
  entry:
    %envelemptr_0 = getelementptr i64, ptr %env, i64 0
    %b_1 = load i64, ptr %envelemptr_0, align 4
    %envelemptr_1 = getelementptr i64, ptr %env, i64 1
    %x_0 = load i64, ptr %envelemptr_1, align 4
    %call_2 = call i64 @a()
    %t_0 = add i64 %y_2, %call_2
    %t_1 = add i64 %t_0, %x_0
    %t_11 = add i64 %t_1, %b_1
    ret i64 %t_11
  }
  
  define i64 @f(i64 %x_0) {
  entry:
    %tupled_env_3 = call ptr @alloc_tuple(i64 2)
    %envptr_4 = getelementptr i64, ptr %tupled_env_3, i64 0
    store i64 2, ptr %envptr_4, align 4
    %envptr_5 = getelementptr i64, ptr %tupled_env_3, i64 1
    store i64 %x_0, ptr %envptr_5, align 4
    %closure_temp_6 = call ptr @alloc_closure(ptr @fresh_fun_0, ptr %tupled_env_3, i64 1, i64 2)
    %cast_7 = ptrtoint ptr %closure_temp_6 to i64
    ret i64 %cast_7
  }
  
  define i64 @main() {
  entry:
    %closure_temp_9 = call ptr @alloc_closure(ptr @f, ptr @nil, i64 1, i64 0)
    %tupled_args_11 = call ptr @alloc_tuple(i64 2)
    %elemptr_12 = getelementptr i64, ptr %tupled_args_11, i64 0
    store i64 3, ptr %elemptr_12, align 4
    %elemptr_13 = getelementptr i64, ptr %tupled_args_11, i64 1
    store i64 4, ptr %elemptr_13, align 4
    %t_3 = call i64 @call_closure(ptr %closure_temp_9, ptr %tupled_args_11, i64 2)
    %0 = call i64 @print_int(i64 %t_3)
    ret i64 0
  }

  $ dune exec llvm_codegen <<- EOF
  > let rec map f list =
  > match list with
  > | [] -> []
  > | x::xs -> (f x)::(map f xs)
  > let rec iter action list =
  >  match list with
  >  | [] -> ()
  >  | x::xs -> let () = action x in let () =  iter action xs in ()
  > let main =
  >  let square x = x * x in
  >   let list = [1; 2; 3; 4; 5] in
  >   let squared = map square list in
  >   let () = iter print_int squared in
  >   0
  ; ModuleID = 'main'
  source_filename = "main"
  
  @nil = global ptr null
  @unit = global i64 0
  
  declare ptr @alloc_tuple(i64 %0)
  
  declare ptr @alloc_closure(ptr %0, ptr %1, i64 %2, i64 %3)
  
  declare i64 @call_closure(ptr %0, ptr %1, i64 %2)
  
  declare i64 @print_int(i64 %0)
  
  declare ptr @list_cons(i64 %0, ptr %1)
  
  declare void @partial_match(i64 %0)
  
  define i64 @map(ptr %env, i64 %list_1) {
  entry:
    %envelemptr_0 = getelementptr i64, ptr %env, i64 0
    %f_0 = load i64, ptr %envelemptr_0, align 4
    %cast_1 = inttoptr i64 %list_1 to ptr
    %t_0 = icmp eq ptr %cast_1, @nil
    %ifcmp_2 = icmp eq i1 %t_0, false
    br i1 %ifcmp_2, label %else_4, label %then_3
  
  then_3:                                           ; preds = %entry
    br label %merge_28
  
  else_4:                                           ; preds = %entry
    %cast_5 = inttoptr i64 %list_1 to ptr
    %t_1 = icmp ne ptr %cast_5, @nil
    %ifcmp_6 = icmp eq i1 %t_1, false
    br i1 %ifcmp_6, label %else_25, label %then_7
  
  then_7:                                           ; preds = %else_4
    %cast_8 = inttoptr i64 %list_1 to ptr
    %fieldptr_9 = getelementptr i64, ptr %cast_8, i64 0
    %x_2 = load i64, ptr %fieldptr_9, align 4
    %cast_11 = inttoptr i64 %list_1 to ptr
    %fieldptr_12 = getelementptr i64, ptr %cast_11, i64 1
    %xs_3 = load i64, ptr %fieldptr_12, align 4
    %cast_14 = inttoptr i64 %f_0 to ptr
    %tupled_args_16 = call ptr @alloc_tuple(i64 1)
    %elemptr_17 = getelementptr i64, ptr %tupled_args_16, i64 0
    store i64 %x_2, ptr %elemptr_17, align 4
    %t_2 = call i64 @call_closure(ptr %cast_14, ptr %tupled_args_16, i64 1)
    %tupled_env_18 = call ptr @alloc_tuple(i64 1)
    %closure_temp_19 = call ptr @alloc_closure(ptr @map, ptr %tupled_env_18, i64 2, i64 1)
    %tupled_args_21 = call ptr @alloc_tuple(i64 2)
    %elemptr_22 = getelementptr i64, ptr %tupled_args_21, i64 0
    store i64 %f_0, ptr %elemptr_22, align 4
    %elemptr_23 = getelementptr i64, ptr %tupled_args_21, i64 1
    store i64 %xs_3, ptr %elemptr_23, align 4
    %t_3 = call i64 @call_closure(ptr %closure_temp_19, ptr %tupled_args_21, i64 2)
    %cast_24 = inttoptr i64 %t_3 to ptr
    %t_31 = call ptr @list_cons(i64 %t_2, ptr %cast_24)
    br label %merge_26
  
  else_25:                                          ; preds = %else_4
    call void @partial_match(i64 %list_1)
    unreachable
  
  merge_26:                                         ; preds = %then_7
    %phi_27 = phi ptr [ %t_31, %then_7 ]
    br label %merge_28
  
  merge_28:                                         ; preds = %merge_26, %then_3
    %phi_29 = phi ptr [ @nil, %then_3 ], [ %phi_27, %merge_26 ]
    %cast_30 = ptrtoint ptr %phi_29 to i64
    ret i64 %cast_30
  }
  
  define i64 @iter(ptr %env, i64 %list_5) {
  entry:
    %envelemptr_31 = getelementptr i64, ptr %env, i64 0
    %action_4 = load i64, ptr %envelemptr_31, align 4
    %cast_32 = inttoptr i64 %list_5 to ptr
    %t_8 = icmp eq ptr %cast_32, @nil
    %ifcmp_33 = icmp eq i1 %t_8, false
    br i1 %ifcmp_33, label %else_35, label %then_34
  
  then_34:                                          ; preds = %entry
    br label %merge_58
  
  else_35:                                          ; preds = %entry
    %cast_36 = inttoptr i64 %list_5 to ptr
    %t_9 = icmp ne ptr %cast_36, @nil
    %ifcmp_37 = icmp eq i1 %t_9, false
    br i1 %ifcmp_37, label %else_55, label %then_38
  
  then_38:                                          ; preds = %else_35
    %cast_39 = inttoptr i64 %list_5 to ptr
    %fieldptr_40 = getelementptr i64, ptr %cast_39, i64 0
    %x_6 = load i64, ptr %fieldptr_40, align 4
    %cast_42 = inttoptr i64 %list_5 to ptr
    %fieldptr_43 = getelementptr i64, ptr %cast_42, i64 1
    %xs_7 = load i64, ptr %fieldptr_43, align 4
    %cast_45 = inttoptr i64 %action_4 to ptr
    %tupled_args_47 = call ptr @alloc_tuple(i64 1)
    %elemptr_48 = getelementptr i64, ptr %tupled_args_47, i64 0
    store i64 %x_6, ptr %elemptr_48, align 4
    %call_closure_46 = call i64 @call_closure(ptr %cast_45, ptr %tupled_args_47, i64 1)
    %tupled_env_49 = call ptr @alloc_tuple(i64 1)
    %closure_temp_50 = call ptr @alloc_closure(ptr @iter, ptr %tupled_env_49, i64 2, i64 1)
    %tupled_args_52 = call ptr @alloc_tuple(i64 2)
    %elemptr_53 = getelementptr i64, ptr %tupled_args_52, i64 0
    store i64 %action_4, ptr %elemptr_53, align 4
    %elemptr_54 = getelementptr i64, ptr %tupled_args_52, i64 1
    store i64 %xs_7, ptr %elemptr_54, align 4
    %call_closure_51 = call i64 @call_closure(ptr %closure_temp_50, ptr %tupled_args_52, i64 2)
    br label %merge_56
  
  else_55:                                          ; preds = %else_35
    call void @partial_match(i64 %list_5)
    unreachable
  
  merge_56:                                         ; preds = %then_38
    %phi_57 = phi ptr [ @unit, %then_38 ]
    br label %merge_58
  
  merge_58:                                         ; preds = %merge_56, %then_34
    %phi_59 = phi ptr [ @unit, %then_34 ], [ %phi_57, %merge_56 ]
    %cast_60 = ptrtoint ptr %phi_59 to i64
    ret i64 %cast_60
  }
  
  define i64 @square_11(i64 %x_10) {
  entry:
    %0 = mul i64 %x_10, %x_10
    ret i64 %0
  }
  
  define i64 @main() {
  entry:
    %t_17 = call ptr @list_cons(i64 5, ptr @nil)
    %t_18 = call ptr @list_cons(i64 4, ptr %t_17)
    %t_19 = call ptr @list_cons(i64 3, ptr %t_18)
    %t_20 = call ptr @list_cons(i64 2, ptr %t_19)
    %list_12 = call ptr @list_cons(i64 1, ptr %t_20)
    %closure_temp_62 = call ptr @alloc_closure(ptr @square_11, ptr @nil, i64 1, i64 0)
    %tupled_env_63 = call ptr @alloc_tuple(i64 1)
    %closure_temp_64 = call ptr @alloc_closure(ptr @map, ptr %tupled_env_63, i64 2, i64 1)
    %tupled_args_66 = call ptr @alloc_tuple(i64 2)
    %elemptr_67 = getelementptr i64, ptr %tupled_args_66, i64 0
    store ptr %closure_temp_62, ptr %elemptr_67, align 8
    %elemptr_68 = getelementptr i64, ptr %tupled_args_66, i64 1
    store ptr %list_12, ptr %elemptr_68, align 8
    %squared_13 = call i64 @call_closure(ptr %closure_temp_64, ptr %tupled_args_66, i64 2)
    %closure_temp_70 = call ptr @alloc_closure(ptr @print_int, ptr @nil, i64 1, i64 0)
    %tupled_env_71 = call ptr @alloc_tuple(i64 1)
    %closure_temp_72 = call ptr @alloc_closure(ptr @iter, ptr %tupled_env_71, i64 2, i64 1)
    %tupled_args_74 = call ptr @alloc_tuple(i64 2)
    %elemptr_75 = getelementptr i64, ptr %tupled_args_74, i64 0
    store ptr %closure_temp_70, ptr %elemptr_75, align 8
    %elemptr_76 = getelementptr i64, ptr %tupled_args_74, i64 1
    store i64 %squared_13, ptr %elemptr_76, align 4
    %call_closure_73 = call i64 @call_closure(ptr %closure_temp_72, ptr %tupled_args_74, i64 2)
    ret i64 0
  }

  $ dune exec llvm_codegen <<- EOF
  > let sum_pair (a, b) = a + b
  > let main = 
  >   let () = print_int (sum_pair (1, 2)) in
  >   0
  ; ModuleID = 'main'
  source_filename = "main"
  
  @nil = global ptr null
  @unit = global i64 0
  
  declare ptr @alloc_tuple(i64 %0)
  
  declare ptr @alloc_closure(ptr %0, ptr %1, i64 %2, i64 %3)
  
  declare i64 @call_closure(ptr %0, ptr %1, i64 %2)
  
  declare i64 @print_int(i64 %0)
  
  declare ptr @list_cons(i64 %0, ptr %1)
  
  declare void @partial_match(i64 %0)
  
  define i64 @sum_pair(i64 %fresh_param_0) {
  entry:
    %cast_0 = inttoptr i64 %fresh_param_0 to ptr
    %fieldptr_1 = getelementptr i64, ptr %cast_0, i64 0
    %a_0 = load i64, ptr %fieldptr_1, align 4
    %cast_3 = inttoptr i64 %fresh_param_0 to ptr
    %fieldptr_4 = getelementptr i64, ptr %cast_3, i64 1
    %b_1 = load i64, ptr %fieldptr_4, align 4
    %b_11 = add i64 %a_0, %b_1
    ret i64 %b_11
  }
  
  define i64 @main() {
  entry:
    %t_3 = call ptr @alloc_tuple(i64 2)
    %elemptr_6 = getelementptr i64, ptr %t_3, i64 0
    store i64 1, ptr %elemptr_6, align 4
    %elemptr_7 = getelementptr i64, ptr %t_3, i64 1
    store i64 2, ptr %elemptr_7, align 4
    %cast_8 = ptrtoint ptr %t_3 to i64
    %t_4 = call i64 @sum_pair(i64 %cast_8)
    %0 = call i64 @print_int(i64 %t_4)
    ret i64 0
  }
  $ dune exec llvm_codegen <<- EOF
  > let add x y = x + y
  > let main =
  >   let add5 = add 5 in
  >   let () = print_int (add5 1) in
  >   0
  ; ModuleID = 'main'
  source_filename = "main"
  
  @nil = global ptr null
  @unit = global i64 0
  
  declare ptr @alloc_tuple(i64 %0)
  
  declare ptr @alloc_closure(ptr %0, ptr %1, i64 %2, i64 %3)
  
  declare i64 @call_closure(ptr %0, ptr %1, i64 %2)
  
  declare i64 @print_int(i64 %0)
  
  declare ptr @list_cons(i64 %0, ptr %1)
  
  declare void @partial_match(i64 %0)
  
  define i64 @add(ptr %env, i64 %y_1) {
  entry:
    %envelemptr_0 = getelementptr i64, ptr %env, i64 0
    %x_0 = load i64, ptr %envelemptr_0, align 4
    %0 = add i64 %x_0, %y_1
    ret i64 %0
  }
  
  define i64 @main() {
  entry:
    %tupled_env_1 = call ptr @alloc_tuple(i64 1)
    %closure_temp_2 = call ptr @alloc_closure(ptr @add, ptr %tupled_env_1, i64 2, i64 1)
    %tupled_args_4 = call ptr @alloc_tuple(i64 1)
    %elemptr_5 = getelementptr i64, ptr %tupled_args_4, i64 0
    store i64 5, ptr %elemptr_5, align 4
    %add5_2 = call i64 @call_closure(ptr %closure_temp_2, ptr %tupled_args_4, i64 1)
    %cast_6 = inttoptr i64 %add5_2 to ptr
    %tupled_args_8 = call ptr @alloc_tuple(i64 1)
    %elemptr_9 = getelementptr i64, ptr %tupled_args_8, i64 0
    store i64 1, ptr %elemptr_9, align 4
    %t_1 = call i64 @call_closure(ptr %cast_6, ptr %tupled_args_8, i64 1)
    %0 = call i64 @print_int(i64 %t_1)
    ret i64 0
  }

  $ dune exec llvm_codegen <<- EOF
  > let rec fix f x = f (fix f) x
  > let fac self n = if n<=1 then 1 else n * self (n-1)
  > let main =
  > let () = print_int (fix fac 6) in
  > 0
  ; ModuleID = 'main'
  source_filename = "main"
  
  @nil = global ptr null
  @unit = global i64 0
  
  declare ptr @alloc_tuple(i64 %0)
  
  declare ptr @alloc_closure(ptr %0, ptr %1, i64 %2, i64 %3)
  
  declare i64 @call_closure(ptr %0, ptr %1, i64 %2)
  
  declare i64 @print_int(i64 %0)
  
  declare ptr @list_cons(i64 %0, ptr %1)
  
  declare void @partial_match(i64 %0)
  
  define i64 @fix(ptr %env, i64 %x_1) {
  entry:
    %envelemptr_0 = getelementptr i64, ptr %env, i64 0
    %f_0 = load i64, ptr %envelemptr_0, align 4
    %tupled_env_1 = call ptr @alloc_tuple(i64 1)
    %closure_temp_2 = call ptr @alloc_closure(ptr @fix, ptr %tupled_env_1, i64 2, i64 1)
    %tupled_args_4 = call ptr @alloc_tuple(i64 1)
    %elemptr_5 = getelementptr i64, ptr %tupled_args_4, i64 0
    store i64 %f_0, ptr %elemptr_5, align 4
    %t_0 = call i64 @call_closure(ptr %closure_temp_2, ptr %tupled_args_4, i64 1)
    %cast_6 = inttoptr i64 %f_0 to ptr
    %tupled_args_8 = call ptr @alloc_tuple(i64 2)
    %elemptr_9 = getelementptr i64, ptr %tupled_args_8, i64 0
    store i64 %t_0, ptr %elemptr_9, align 4
    %elemptr_10 = getelementptr i64, ptr %tupled_args_8, i64 1
    store i64 %x_1, ptr %elemptr_10, align 4
    %call_closure_7 = call i64 @call_closure(ptr %cast_6, ptr %tupled_args_8, i64 2)
    ret i64 %call_closure_7
  }
  
  define i64 @fac(ptr %env, i64 %n_3) {
  entry:
    %envelemptr_11 = getelementptr i64, ptr %env, i64 0
    %self_2 = load i64, ptr %envelemptr_11, align 4
    %t_2 = icmp sle i64 %n_3, 1
    %ifcmp_12 = icmp eq i1 %t_2, false
    br i1 %ifcmp_12, label %else_14, label %then_13
  
  then_13:                                          ; preds = %entry
    br label %merge_19
  
  else_14:                                          ; preds = %entry
    %t_3 = sub i64 %n_3, 1
    %cast_15 = inttoptr i64 %self_2 to ptr
    %tupled_args_17 = call ptr @alloc_tuple(i64 1)
    %elemptr_18 = getelementptr i64, ptr %tupled_args_17, i64 0
    store i64 %t_3, ptr %elemptr_18, align 4
    %t_4 = call i64 @call_closure(ptr %cast_15, ptr %tupled_args_17, i64 1)
    %t_41 = mul i64 %n_3, %t_4
    br label %merge_19
  
  merge_19:                                         ; preds = %else_14, %then_13
    %phi_20 = phi i64 [ 1, %then_13 ], [ %t_41, %else_14 ]
    ret i64 %phi_20
  }
  
  define i64 @main() {
  entry:
    %tupled_env_21 = call ptr @alloc_tuple(i64 1)
    %closure_temp_22 = call ptr @alloc_closure(ptr @fac, ptr %tupled_env_21, i64 2, i64 1)
    %tupled_env_23 = call ptr @alloc_tuple(i64 1)
    %closure_temp_24 = call ptr @alloc_closure(ptr @fix, ptr %tupled_env_23, i64 2, i64 1)
    %tupled_args_26 = call ptr @alloc_tuple(i64 2)
    %elemptr_27 = getelementptr i64, ptr %tupled_args_26, i64 0
    store ptr %closure_temp_22, ptr %elemptr_27, align 8
    %elemptr_28 = getelementptr i64, ptr %tupled_args_26, i64 1
    store i64 6, ptr %elemptr_28, align 4
    %t_6 = call i64 @call_closure(ptr %closure_temp_24, ptr %tupled_args_26, i64 2)
    %0 = call i64 @print_int(i64 %t_6)
    ret i64 0
  }

  $ dune exec llvm_codegen <<- EOF
  > let adder x y z = x + y + z
  > let main =
  >   let adder1 = adder 1 in
  >   let adder12 = adder1 2 in
  >   let adder13 = adder1 3 in
  >   let () = print_int (adder12 3) in
  >   let () = print_int (adder13 3) in
  >   0
  ; ModuleID = 'main'
  source_filename = "main"
  
  @nil = global ptr null
  @unit = global i64 0
  
  declare ptr @alloc_tuple(i64 %0)
  
  declare ptr @alloc_closure(ptr %0, ptr %1, i64 %2, i64 %3)
  
  declare i64 @call_closure(ptr %0, ptr %1, i64 %2)
  
  declare i64 @print_int(i64 %0)
  
  declare ptr @list_cons(i64 %0, ptr %1)
  
  declare void @partial_match(i64 %0)
  
  define i64 @adder(ptr %env, i64 %z_2) {
  entry:
    %envelemptr_0 = getelementptr i64, ptr %env, i64 0
    %x_0 = load i64, ptr %envelemptr_0, align 4
    %envelemptr_1 = getelementptr i64, ptr %env, i64 1
    %y_1 = load i64, ptr %envelemptr_1, align 4
    %t_0 = add i64 %x_0, %y_1
    %t_01 = add i64 %t_0, %z_2
    ret i64 %t_01
  }
  
  define i64 @main() {
  entry:
    %tupled_env_2 = call ptr @alloc_tuple(i64 2)
    %closure_temp_3 = call ptr @alloc_closure(ptr @adder, ptr %tupled_env_2, i64 3, i64 2)
    %tupled_args_5 = call ptr @alloc_tuple(i64 1)
    %elemptr_6 = getelementptr i64, ptr %tupled_args_5, i64 0
    store i64 1, ptr %elemptr_6, align 4
    %adder1_3 = call i64 @call_closure(ptr %closure_temp_3, ptr %tupled_args_5, i64 1)
    %cast_7 = inttoptr i64 %adder1_3 to ptr
    %tupled_args_9 = call ptr @alloc_tuple(i64 1)
    %elemptr_10 = getelementptr i64, ptr %tupled_args_9, i64 0
    store i64 2, ptr %elemptr_10, align 4
    %adder12_4 = call i64 @call_closure(ptr %cast_7, ptr %tupled_args_9, i64 1)
    %cast_11 = inttoptr i64 %adder1_3 to ptr
    %tupled_args_13 = call ptr @alloc_tuple(i64 1)
    %elemptr_14 = getelementptr i64, ptr %tupled_args_13, i64 0
    store i64 3, ptr %elemptr_14, align 4
    %adder13_5 = call i64 @call_closure(ptr %cast_11, ptr %tupled_args_13, i64 1)
    %cast_15 = inttoptr i64 %adder12_4 to ptr
    %tupled_args_17 = call ptr @alloc_tuple(i64 1)
    %elemptr_18 = getelementptr i64, ptr %tupled_args_17, i64 0
    store i64 3, ptr %elemptr_18, align 4
    %t_4 = call i64 @call_closure(ptr %cast_15, ptr %tupled_args_17, i64 1)
    %0 = call i64 @print_int(i64 %t_4)
    %cast_19 = inttoptr i64 %adder13_5 to ptr
    %tupled_args_21 = call ptr @alloc_tuple(i64 1)
    %elemptr_22 = getelementptr i64, ptr %tupled_args_21, i64 0
    store i64 3, ptr %elemptr_22, align 4
    %t_2 = call i64 @call_closure(ptr %cast_19, ptr %tupled_args_21, i64 1)
    %1 = call i64 @print_int(i64 %t_2)
    ret i64 0
  }

  $ dune exec llvm_codegen < manytests/typed/008ascription.ml
  ; ModuleID = 'main'
  source_filename = "main"
  
  @nil = global ptr null
  @unit = global i64 0
  
  declare ptr @alloc_tuple(i64 %0)
  
  declare ptr @alloc_closure(ptr %0, ptr %1, i64 %2, i64 %3)
  
  declare i64 @call_closure(ptr %0, ptr %1, i64 %2)
  
  declare i64 @print_int(i64 %0)
  
  declare ptr @list_cons(i64 %0, ptr %1)
  
  declare void @partial_match(i64 %0)
  
  define i64 @addi(ptr %env, i64 %x_2) {
  entry:
    %envelemptr_0 = getelementptr i64, ptr %env, i64 0
    %f_0 = load i64, ptr %envelemptr_0, align 4
    %envelemptr_1 = getelementptr i64, ptr %env, i64 1
    %g_1 = load i64, ptr %envelemptr_1, align 4
    %cast_2 = inttoptr i64 %g_1 to ptr
    %tupled_args_4 = call ptr @alloc_tuple(i64 1)
    %elemptr_5 = getelementptr i64, ptr %tupled_args_4, i64 0
    store i64 %x_2, ptr %elemptr_5, align 4
    %t_0 = call i64 @call_closure(ptr %cast_2, ptr %tupled_args_4, i64 1)
    %cast_6 = inttoptr i64 %f_0 to ptr
    %tupled_args_8 = call ptr @alloc_tuple(i64 2)
    %elemptr_9 = getelementptr i64, ptr %tupled_args_8, i64 0
    store i64 %x_2, ptr %elemptr_9, align 4
    %elemptr_10 = getelementptr i64, ptr %tupled_args_8, i64 1
    store i64 %t_0, ptr %elemptr_10, align 4
    %call_closure_7 = call i64 @call_closure(ptr %cast_6, ptr %tupled_args_8, i64 2)
    ret i64 %call_closure_7
  }
  
  define i64 @fresh_fun_0(ptr %env, i64 %b_4) {
  entry:
    %envelemptr_11 = getelementptr i64, ptr %env, i64 0
    %x_3 = load i64, ptr %envelemptr_11, align 4
    %0 = trunc i64 %b_4 to i1
    %ifcmp_12 = icmp eq i1 %0, false
    br i1 %ifcmp_12, label %else_14, label %then_13
  
  then_13:                                          ; preds = %entry
    %t_0 = add i64 %x_3, 1
    br label %merge_15
  
  else_14:                                          ; preds = %entry
    %t_01 = mul i64 %x_3, 2
    br label %merge_15
  
  merge_15:                                         ; preds = %else_14, %then_13
    %phi_16 = phi i64 [ %t_0, %then_13 ], [ %t_01, %else_14 ]
    ret i64 %phi_16
  }
  
  define i64 @fresh_fun_1(i64 %_start_5) {
  entry:
    %t_4 = sdiv i64 %_start_5, 2
    %t_41 = icmp eq i64 %t_4, 0
    %0 = sext i1 %t_41 to i64
    ret i64 %0
  }
  
  define i64 @main() {
  entry:
    %closure_temp_18 = call ptr @alloc_closure(ptr @fresh_fun_1, ptr @nil, i64 1, i64 0)
    %tupled_env_19 = call ptr @alloc_tuple(i64 1)
    %closure_temp_20 = call ptr @alloc_closure(ptr @fresh_fun_0, ptr %tupled_env_19, i64 2, i64 1)
    %tupled_env_21 = call ptr @alloc_tuple(i64 2)
    %closure_temp_22 = call ptr @alloc_closure(ptr @addi, ptr %tupled_env_21, i64 3, i64 2)
    %tupled_args_24 = call ptr @alloc_tuple(i64 3)
    %elemptr_25 = getelementptr i64, ptr %tupled_args_24, i64 0
    store ptr %closure_temp_20, ptr %elemptr_25, align 8
    %elemptr_26 = getelementptr i64, ptr %tupled_args_24, i64 1
    store ptr %closure_temp_18, ptr %elemptr_26, align 8
    %elemptr_27 = getelementptr i64, ptr %tupled_args_24, i64 2
    store i64 4, ptr %elemptr_27, align 4
    %t_2 = call i64 @call_closure(ptr %closure_temp_22, ptr %tupled_args_24, i64 3)
    %0 = call i64 @print_int(i64 %t_2)
    ret i64 0
  }
