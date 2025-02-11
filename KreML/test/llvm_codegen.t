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
  
  define i64 @fresh_fun_0(ptr %env, i64 %y_2, i64 %z_3) {
  entry:
    %envelemptr_0 = getelementptr i64, ptr %env, i64 0
    %a_1 = load i64, ptr %envelemptr_0, align 4
    %envelemptr_1 = getelementptr i64, ptr %env, i64 1
    %x_0 = load i64, ptr %envelemptr_1, align 4
    %t_0 = add i64 %y_2, %z_3
    %t_1 = add i64 %t_0, %a_1
    %t_11 = add i64 %t_1, %x_0
    ret i64 %t_11
  }
  
  define i64 @f(i64 %x_0) {
  entry:
    %a_1 = add i64 %x_0, 5
    %tupled_env_2 = call ptr @alloc_tuple(i64 4)
    %envptr_3 = getelementptr i64, ptr %tupled_env_2, i64 2
    store i64 %a_1, ptr %envptr_3, align 4
    %envptr_4 = getelementptr i64, ptr %tupled_env_2, i64 3
    store i64 %x_0, ptr %envptr_4, align 4
    %closure_temp_5 = call ptr @alloc_closure(ptr @fresh_fun_0, ptr %tupled_env_2, i64 2, i64 2)
    %cast_6 = ptrtoint ptr %closure_temp_5 to i64
    ret i64 %cast_6
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
    %tupled_env_2 = call ptr @alloc_tuple(i64 3)
    %envptr_3 = getelementptr i64, ptr %tupled_env_2, i64 1
    store i64 %a_1, ptr %envptr_3, align 4
    %envptr_4 = getelementptr i64, ptr %tupled_env_2, i64 2
    store i64 %x_0, ptr %envptr_4, align 4
    %closure_temp_5 = call ptr @alloc_closure(ptr @inner_3, ptr %tupled_env_2, i64 1, i64 2)
    %cast_6 = ptrtoint ptr %closure_temp_5 to i64
    ret i64 %cast_6
  }
  
  define i64 @main() {
  entry:
    %tupled_env_7 = call ptr @alloc_tuple(i64 1)
    %closure_temp_8 = call ptr @alloc_closure(ptr @f, ptr %tupled_env_7, i64 1, i64 0)
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
  
  define i64 @map_point(i64 %f_0, i64 %p_1) {
  entry:
    %cast_0 = inttoptr i64 %p_1 to ptr
    %fieldptr_1 = getelementptr i64, ptr %cast_0, i64 0
    %a_2 = load i64, ptr %fieldptr_1, align 4
    %cast_3 = inttoptr i64 %p_1 to ptr
    %fieldptr_4 = getelementptr i64, ptr %cast_3, i64 1
    %b_3 = load i64, ptr %fieldptr_4, align 4
    %cast_6 = inttoptr i64 %f_0 to ptr
    %tupled_args_8 = call ptr @alloc_tuple(i64 1)
    %elemptr_9 = getelementptr i64, ptr %tupled_args_8, i64 0
    store i64 %a_2, ptr %elemptr_9, align 4
    %t_0 = call i64 @call_closure(ptr %cast_6, ptr %tupled_args_8, i64 1)
    %cast_10 = inttoptr i64 %f_0 to ptr
    %tupled_args_12 = call ptr @alloc_tuple(i64 1)
    %elemptr_13 = getelementptr i64, ptr %tupled_args_12, i64 0
    store i64 %b_3, ptr %elemptr_13, align 4
    %t_1 = call i64 @call_closure(ptr %cast_10, ptr %tupled_args_12, i64 1)
    %t_11 = call ptr @alloc_tuple(i64 2)
    %elemptr_14 = getelementptr i64, ptr %t_11, i64 0
    store i64 %t_0, ptr %elemptr_14, align 4
    %elemptr_15 = getelementptr i64, ptr %t_11, i64 1
    store i64 %t_1, ptr %elemptr_15, align 4
    %cast_16 = ptrtoint ptr %t_11 to i64
    ret i64 %cast_16
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
  
  define i64 @cps_fac(i64 %n_0, i64 %k_1) {
  entry:
    %t_0 = icmp slt i64 %n_0, 1
    %ifcmp_6 = icmp eq i1 %t_0, false
    br i1 %ifcmp_6, label %else_12, label %then_7
  
  then_7:                                           ; preds = %entry
    %cast_8 = inttoptr i64 %k_1 to ptr
    %tupled_args_10 = call ptr @alloc_tuple(i64 1)
    %elemptr_11 = getelementptr i64, ptr %tupled_args_10, i64 0
    store i64 1, ptr %elemptr_11, align 4
    %call_closure_9 = call i64 @call_closure(ptr %cast_8, ptr %tupled_args_10, i64 1)
    br label %merge_17
  
  else_12:                                          ; preds = %entry
    %t_2 = sub i64 %n_0, 1
    %tupled_env_13 = call ptr @alloc_tuple(i64 3)
    %envptr_14 = getelementptr i64, ptr %tupled_env_13, i64 1
    store i64 %k_1, ptr %envptr_14, align 4
    %envptr_15 = getelementptr i64, ptr %tupled_env_13, i64 2
    store i64 %n_0, ptr %envptr_15, align 4
    %closure_temp_16 = call ptr @alloc_closure(ptr @fresh_fun_0, ptr %tupled_env_13, i64 1, i64 2)
    %0 = ptrtoint ptr %closure_temp_16 to i64
    %t_21 = call i64 @cps_fac(i64 %t_2, i64 %0)
    br label %merge_17
  
  merge_17:                                         ; preds = %else_12, %then_7
    %phi_18 = phi i64 [ %call_closure_9, %then_7 ], [ %t_21, %else_12 ]
    ret i64 %phi_18
  }
  
  define i64 @fresh_fun_1(i64 %x_3) {
  entry:
    ret i64 %x_3
  }
  
  define i64 @main() {
  entry:
    %tupled_env_19 = call ptr @alloc_tuple(i64 1)
    %closure_temp_20 = call ptr @alloc_closure(ptr @fresh_fun_1, ptr %tupled_env_19, i64 1, i64 0)
    %0 = ptrtoint ptr %closure_temp_20 to i64
    %res_4 = call i64 @cps_fac(i64 6, i64 %0)
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
    %tupled_env_3 = call ptr @alloc_tuple(i64 3)
    %envptr_4 = getelementptr i64, ptr %tupled_env_3, i64 1
    store i64 2, ptr %envptr_4, align 4
    %envptr_5 = getelementptr i64, ptr %tupled_env_3, i64 2
    store i64 %x_0, ptr %envptr_5, align 4
    %closure_temp_6 = call ptr @alloc_closure(ptr @fresh_fun_0, ptr %tupled_env_3, i64 1, i64 2)
    %cast_7 = ptrtoint ptr %closure_temp_6 to i64
    ret i64 %cast_7
  }
  
  define i64 @main() {
  entry:
    %tupled_env_8 = call ptr @alloc_tuple(i64 1)
    %closure_temp_9 = call ptr @alloc_closure(ptr @f, ptr %tupled_env_8, i64 1, i64 0)
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
  
  define i64 @map(i64 %f_0, i64 %list_1) {
  entry:
    %cast_0 = inttoptr i64 %list_1 to ptr
    %t_0 = icmp eq ptr %cast_0, @nil
    %ifcmp_1 = icmp eq i1 %t_0, false
    br i1 %ifcmp_1, label %else_3, label %then_2
  
  then_2:                                           ; preds = %entry
    br label %merge_21
  
  else_3:                                           ; preds = %entry
    %cast_4 = inttoptr i64 %list_1 to ptr
    %t_1 = icmp ne ptr %cast_4, @nil
    %ifcmp_5 = icmp eq i1 %t_1, false
    br i1 %ifcmp_5, label %else_18, label %then_6
  
  then_6:                                           ; preds = %else_3
    %cast_7 = inttoptr i64 %list_1 to ptr
    %fieldptr_8 = getelementptr i64, ptr %cast_7, i64 0
    %x_2 = load i64, ptr %fieldptr_8, align 4
    %cast_10 = inttoptr i64 %list_1 to ptr
    %fieldptr_11 = getelementptr i64, ptr %cast_10, i64 1
    %xs_3 = load i64, ptr %fieldptr_11, align 4
    %cast_13 = inttoptr i64 %f_0 to ptr
    %tupled_args_15 = call ptr @alloc_tuple(i64 1)
    %elemptr_16 = getelementptr i64, ptr %tupled_args_15, i64 0
    store i64 %x_2, ptr %elemptr_16, align 4
    %t_2 = call i64 @call_closure(ptr %cast_13, ptr %tupled_args_15, i64 1)
    %t_3 = call i64 @map(i64 %f_0, i64 %xs_3)
    %cast_17 = inttoptr i64 %t_3 to ptr
    %t_31 = call ptr @list_cons(i64 %t_2, ptr %cast_17)
    br label %merge_19
  
  else_18:                                          ; preds = %else_3
    call void @partial_match(i64 %list_1)
    unreachable
  
  merge_19:                                         ; preds = %then_6
    %phi_20 = phi ptr [ %t_31, %then_6 ]
    br label %merge_21
  
  merge_21:                                         ; preds = %merge_19, %then_2
    %phi_22 = phi ptr [ @nil, %then_2 ], [ %phi_20, %merge_19 ]
    %cast_23 = ptrtoint ptr %phi_22 to i64
    ret i64 %cast_23
  }
  
  define i64 @iter(i64 %action_4, i64 %list_5) {
  entry:
    %cast_24 = inttoptr i64 %list_5 to ptr
    %t_8 = icmp eq ptr %cast_24, @nil
    %ifcmp_25 = icmp eq i1 %t_8, false
    br i1 %ifcmp_25, label %else_27, label %then_26
  
  then_26:                                          ; preds = %entry
    br label %merge_44
  
  else_27:                                          ; preds = %entry
    %cast_28 = inttoptr i64 %list_5 to ptr
    %t_9 = icmp ne ptr %cast_28, @nil
    %ifcmp_29 = icmp eq i1 %t_9, false
    br i1 %ifcmp_29, label %else_41, label %then_30
  
  then_30:                                          ; preds = %else_27
    %cast_31 = inttoptr i64 %list_5 to ptr
    %fieldptr_32 = getelementptr i64, ptr %cast_31, i64 0
    %x_6 = load i64, ptr %fieldptr_32, align 4
    %cast_34 = inttoptr i64 %list_5 to ptr
    %fieldptr_35 = getelementptr i64, ptr %cast_34, i64 1
    %xs_7 = load i64, ptr %fieldptr_35, align 4
    %cast_37 = inttoptr i64 %action_4 to ptr
    %tupled_args_39 = call ptr @alloc_tuple(i64 1)
    %elemptr_40 = getelementptr i64, ptr %tupled_args_39, i64 0
    store i64 %x_6, ptr %elemptr_40, align 4
    %call_closure_38 = call i64 @call_closure(ptr %cast_37, ptr %tupled_args_39, i64 1)
    %0 = call i64 @iter(i64 %action_4, i64 %xs_7)
    br label %merge_42
  
  else_41:                                          ; preds = %else_27
    call void @partial_match(i64 %list_5)
    unreachable
  
  merge_42:                                         ; preds = %then_30
    %phi_43 = phi ptr [ @unit, %then_30 ]
    br label %merge_44
  
  merge_44:                                         ; preds = %merge_42, %then_26
    %phi_45 = phi ptr [ @unit, %then_26 ], [ %phi_43, %merge_42 ]
    %cast_46 = ptrtoint ptr %phi_45 to i64
    ret i64 %cast_46
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
    %tupled_env_47 = call ptr @alloc_tuple(i64 1)
    %closure_temp_48 = call ptr @alloc_closure(ptr @square_11, ptr %tupled_env_47, i64 1, i64 0)
    %0 = ptrtoint ptr %closure_temp_48 to i64
    %1 = ptrtoint ptr %list_12 to i64
    %squared_13 = call i64 @map(i64 %0, i64 %1)
    %tupled_env_49 = call ptr @alloc_tuple(i64 1)
    %closure_temp_50 = call ptr @alloc_closure(ptr @print_int, ptr %tupled_env_49, i64 1, i64 0)
    %2 = ptrtoint ptr %closure_temp_50 to i64
    %3 = call i64 @iter(i64 %2, i64 %squared_13)
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
    %0 = ptrtoint ptr %t_3 to i64
    %t_4 = call i64 @sum_pair(i64 %0)
    %1 = call i64 @print_int(i64 %t_4)
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
  
  define i64 @add(i64 %x_0, i64 %y_1) {
  entry:
    %0 = add i64 %x_0, %y_1
    ret i64 %0
  }
  
  define i64 @main() {
  entry:
    %tupled_env_0 = call ptr @alloc_tuple(i64 2)
    %closure_temp_1 = call ptr @alloc_closure(ptr @add, ptr %tupled_env_0, i64 2, i64 0)
    %tupled_args_3 = call ptr @alloc_tuple(i64 1)
    %elemptr_4 = getelementptr i64, ptr %tupled_args_3, i64 0
    store i64 5, ptr %elemptr_4, align 4
    %add5_2 = call i64 @call_closure(ptr %closure_temp_1, ptr %tupled_args_3, i64 1)
    %cast_5 = inttoptr i64 %add5_2 to ptr
    %tupled_args_7 = call ptr @alloc_tuple(i64 1)
    %elemptr_8 = getelementptr i64, ptr %tupled_args_7, i64 0
    store i64 1, ptr %elemptr_8, align 4
    %t_1 = call i64 @call_closure(ptr %cast_5, ptr %tupled_args_7, i64 1)
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
  
  define i64 @fix(i64 %f_0, i64 %x_1) {
  entry:
    %tupled_env_0 = call ptr @alloc_tuple(i64 2)
    %closure_temp_1 = call ptr @alloc_closure(ptr @fix, ptr %tupled_env_0, i64 2, i64 0)
    %tupled_args_3 = call ptr @alloc_tuple(i64 1)
    %elemptr_4 = getelementptr i64, ptr %tupled_args_3, i64 0
    store i64 %f_0, ptr %elemptr_4, align 4
    %t_0 = call i64 @call_closure(ptr %closure_temp_1, ptr %tupled_args_3, i64 1)
    %cast_5 = inttoptr i64 %f_0 to ptr
    %tupled_args_7 = call ptr @alloc_tuple(i64 2)
    %elemptr_8 = getelementptr i64, ptr %tupled_args_7, i64 0
    store i64 %t_0, ptr %elemptr_8, align 4
    %elemptr_9 = getelementptr i64, ptr %tupled_args_7, i64 1
    store i64 %x_1, ptr %elemptr_9, align 4
    %call_closure_6 = call i64 @call_closure(ptr %cast_5, ptr %tupled_args_7, i64 2)
    ret i64 %call_closure_6
  }
  
  define i64 @fac(i64 %self_2, i64 %n_3) {
  entry:
    %t_2 = icmp sle i64 %n_3, 1
    %ifcmp_10 = icmp eq i1 %t_2, false
    br i1 %ifcmp_10, label %else_12, label %then_11
  
  then_11:                                          ; preds = %entry
    br label %merge_17
  
  else_12:                                          ; preds = %entry
    %t_3 = sub i64 %n_3, 1
    %cast_13 = inttoptr i64 %self_2 to ptr
    %tupled_args_15 = call ptr @alloc_tuple(i64 1)
    %elemptr_16 = getelementptr i64, ptr %tupled_args_15, i64 0
    store i64 %t_3, ptr %elemptr_16, align 4
    %t_4 = call i64 @call_closure(ptr %cast_13, ptr %tupled_args_15, i64 1)
    %t_41 = mul i64 %n_3, %t_4
    br label %merge_17
  
  merge_17:                                         ; preds = %else_12, %then_11
    %phi_18 = phi i64 [ 1, %then_11 ], [ %t_41, %else_12 ]
    ret i64 %phi_18
  }
  
  define i64 @main() {
  entry:
    %tupled_env_19 = call ptr @alloc_tuple(i64 2)
    %closure_temp_20 = call ptr @alloc_closure(ptr @fac, ptr %tupled_env_19, i64 2, i64 0)
    %0 = ptrtoint ptr %closure_temp_20 to i64
    %t_6 = call i64 @fix(i64 %0, i64 6)
    %1 = call i64 @print_int(i64 %t_6)
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
  
  define i64 @adder(i64 %x_0, i64 %y_1, i64 %z_2) {
  entry:
    %t_0 = add i64 %x_0, %y_1
    %t_01 = add i64 %t_0, %z_2
    ret i64 %t_01
  }
  
  define i64 @main() {
  entry:
    %tupled_env_0 = call ptr @alloc_tuple(i64 3)
    %closure_temp_1 = call ptr @alloc_closure(ptr @adder, ptr %tupled_env_0, i64 3, i64 0)
    %tupled_args_3 = call ptr @alloc_tuple(i64 1)
    %elemptr_4 = getelementptr i64, ptr %tupled_args_3, i64 0
    store i64 1, ptr %elemptr_4, align 4
    %adder1_3 = call i64 @call_closure(ptr %closure_temp_1, ptr %tupled_args_3, i64 1)
    %cast_5 = inttoptr i64 %adder1_3 to ptr
    %tupled_args_7 = call ptr @alloc_tuple(i64 1)
    %elemptr_8 = getelementptr i64, ptr %tupled_args_7, i64 0
    store i64 2, ptr %elemptr_8, align 4
    %adder12_4 = call i64 @call_closure(ptr %cast_5, ptr %tupled_args_7, i64 1)
    %cast_9 = inttoptr i64 %adder1_3 to ptr
    %tupled_args_11 = call ptr @alloc_tuple(i64 1)
    %elemptr_12 = getelementptr i64, ptr %tupled_args_11, i64 0
    store i64 3, ptr %elemptr_12, align 4
    %adder13_5 = call i64 @call_closure(ptr %cast_9, ptr %tupled_args_11, i64 1)
    %cast_13 = inttoptr i64 %adder12_4 to ptr
    %tupled_args_15 = call ptr @alloc_tuple(i64 1)
    %elemptr_16 = getelementptr i64, ptr %tupled_args_15, i64 0
    store i64 3, ptr %elemptr_16, align 4
    %t_4 = call i64 @call_closure(ptr %cast_13, ptr %tupled_args_15, i64 1)
    %0 = call i64 @print_int(i64 %t_4)
    %cast_17 = inttoptr i64 %adder13_5 to ptr
    %tupled_args_19 = call ptr @alloc_tuple(i64 1)
    %elemptr_20 = getelementptr i64, ptr %tupled_args_19, i64 0
    store i64 3, ptr %elemptr_20, align 4
    %t_2 = call i64 @call_closure(ptr %cast_17, ptr %tupled_args_19, i64 1)
    %1 = call i64 @print_int(i64 %t_2)
    ret i64 0
  }

  $ dune exec llvm_codegen < manytests/typed/007order.ml 
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
  
  define i64 @_start(ptr %env, i64 %unused_0, i64 %unused_1, i64 %a_2, i64 %unused_3, i64 %b_4, i64 %_c_5, i64 %unused_6) {
  entry:
    %envelemptr_0 = getelementptr i64, ptr %env, i64 0
    %d_7 = load i64, ptr %envelemptr_0, align 4
    %envelemptr_1 = getelementptr i64, ptr %env, i64 1
    %___8 = load i64, ptr %envelemptr_1, align 4
    %t_4 = add i64 %a_2, %b_4
    %0 = call i64 @print_int(i64 %t_4)
    %1 = call i64 @print_int(i64 %___8)
    %t_0 = mul i64 %a_2, %b_4
    %t_1 = sdiv i64 %t_0, %_c_5
    %t_11 = add i64 %t_1, %d_7
    ret i64 %t_11
  }
  
  define i64 @main() {
  entry:
    %t_6 = call i64 @print_int(i64 1)
    %t_7 = call i64 @print_int(i64 2)
    %t_8 = call i64 @print_int(i64 4)
    %t_9 = call i64 @print_int(i64 -1)
    %tupled_env_2 = call ptr @alloc_tuple(i64 9)
    %closure_temp_3 = call ptr @alloc_closure(ptr @_start, ptr %tupled_env_2, i64 9, i64 0)
    %tupled_args_5 = call ptr @alloc_tuple(i64 9)
    %elemptr_6 = getelementptr i64, ptr %tupled_args_5, i64 0
    store i64 %t_6, ptr %elemptr_6, align 4
    %elemptr_7 = getelementptr i64, ptr %tupled_args_5, i64 1
    store i64 %t_7, ptr %elemptr_7, align 4
    %elemptr_8 = getelementptr i64, ptr %tupled_args_5, i64 2
    store i64 3, ptr %elemptr_8, align 4
    %elemptr_9 = getelementptr i64, ptr %tupled_args_5, i64 3
    store i64 %t_8, ptr %elemptr_9, align 4
    %elemptr_10 = getelementptr i64, ptr %tupled_args_5, i64 4
    store i64 100, ptr %elemptr_10, align 4
    %elemptr_11 = getelementptr i64, ptr %tupled_args_5, i64 5
    store i64 1000, ptr %elemptr_11, align 4
    %elemptr_12 = getelementptr i64, ptr %tupled_args_5, i64 6
    store i64 %t_9, ptr %elemptr_12, align 4
    %elemptr_13 = getelementptr i64, ptr %tupled_args_5, i64 7
    store i64 10000, ptr %elemptr_13, align 4
    %elemptr_14 = getelementptr i64, ptr %tupled_args_5, i64 8
    store i64 -555555, ptr %elemptr_14, align 4
    %t_10 = call i64 @call_closure(ptr %closure_temp_3, ptr %tupled_args_5, i64 9)
    %t_101 = call i64 @print_int(i64 %t_10)
    ret i64 %t_101
  }

$ dune exec llvm_codegen < manytests/typed/004manyargs.ml
