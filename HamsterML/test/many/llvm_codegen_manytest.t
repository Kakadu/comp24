  $ ./LlvmCodegenRunner.exe << EOF | lli-16 -load ../../lib/runtime.so
  > let a = print_int 1
  1

  $ ./LlvmCodegenRunner.exe < manytests/typed/001fac.ml | lli-16 -load ../../lib/runtime.so
  24

  $ ./LlvmCodegenRunner.exe << EOF
  > let a = print_int 1
  ; ModuleID = 'HamsterML_LLVM_Compiler'
  source_filename = "HamsterML_LLVM_Compiler"
  
  declare i64 @print_int(i64)
  
  define i64 @"+"(i64 %0, i64 %1) {
  entry:
    %result = add i64 %0, %1
    ret i64 %result
  }
  
  define i64 @-(i64 %0, i64 %1) {
  entry:
    %result = sub i64 %0, %1
    ret i64 %result
  }
  
  define i64 @"*"(i64 %0, i64 %1) {
  entry:
    %result = mul i64 %0, %1
    ret i64 %result
  }
  
  define i64 @"/"(i64 %0, i64 %1) {
  entry:
    %result = udiv i64 %0, %1
    ret i64 %result
  }
  
  define i64 @"<="(i64 %0, i64 %1) {
  entry:
    %result = icmp sle i64 %0, %1
    %extended_result = zext i1 %result to i64
    ret i64 %extended_result
  }
  
  define i64 @">="(i64 %0, i64 %1) {
  entry:
    %result = icmp sge i64 %0, %1
    %extended_result = zext i1 %result to i64
    ret i64 %extended_result
  }
  
  define i64 @"<"(i64 %0, i64 %1) {
  entry:
    %result = icmp slt i64 %0, %1
    %extended_result = zext i1 %result to i64
    ret i64 %extended_result
  }
  
  define i64 @">"(i64 %0, i64 %1) {
  entry:
    %result = icmp sgt i64 %0, %1
    %extended_result = zext i1 %result to i64
    ret i64 %extended_result
  }
  
  define i64 @"="(i64 %0, i64 %1) {
  entry:
    %result = icmp eq i64 %0, %1
    %extended_result = zext i1 %result to i64
    ret i64 %extended_result
  }
  
  define i64 @"!="(i64 %0, i64 %1) {
  entry:
    %result = icmp ne i64 %0, %1
    %extended_result = zext i1 %result to i64
    ret i64 %extended_result
  }
  
  define i64 @ll_var_0() {
  entry:
    %calltmp = call i64 @print_int(i64 1)
    %anf_0 = alloca i64, align 8
    store i64 %calltmp, ptr %anf_0, align 4
    %anf_01 = load i64, ptr %anf_0, align 4
    ret i64 %anf_01
  }
  
  define i64 @main() {
  entry:
    %main_result = call i64 @ll_var_0()
    ret i64 %main_result
  }

  $ ./LlvmCodegenRunner.exe << EOF
  > let rec fac n = if n<=1 then 1 else n * fac (n-1)
  > let main = let () = print_int (fac 4) in 0
  ; ModuleID = 'HamsterML_LLVM_Compiler'
  source_filename = "HamsterML_LLVM_Compiler"
  
  declare i64 @print_int(i64)
  
  define i64 @"+"(i64 %0, i64 %1) {
  entry:
    %result = add i64 %0, %1
    ret i64 %result
  }
  
  define i64 @-(i64 %0, i64 %1) {
  entry:
    %result = sub i64 %0, %1
    ret i64 %result
  }
  
  define i64 @"*"(i64 %0, i64 %1) {
  entry:
    %result = mul i64 %0, %1
    ret i64 %result
  }
  
  define i64 @"/"(i64 %0, i64 %1) {
  entry:
    %result = udiv i64 %0, %1
    ret i64 %result
  }
  
  define i64 @"<="(i64 %0, i64 %1) {
  entry:
    %result = icmp sle i64 %0, %1
    %extended_result = zext i1 %result to i64
    ret i64 %extended_result
  }
  
  define i64 @">="(i64 %0, i64 %1) {
  entry:
    %result = icmp sge i64 %0, %1
    %extended_result = zext i1 %result to i64
    ret i64 %extended_result
  }
  
  define i64 @"<"(i64 %0, i64 %1) {
  entry:
    %result = icmp slt i64 %0, %1
    %extended_result = zext i1 %result to i64
    ret i64 %extended_result
  }
  
  define i64 @">"(i64 %0, i64 %1) {
  entry:
    %result = icmp sgt i64 %0, %1
    %extended_result = zext i1 %result to i64
    ret i64 %extended_result
  }
  
  define i64 @"="(i64 %0, i64 %1) {
  entry:
    %result = icmp eq i64 %0, %1
    %extended_result = zext i1 %result to i64
    ret i64 %extended_result
  }
  
  define i64 @"!="(i64 %0, i64 %1) {
  entry:
    %result = icmp ne i64 %0, %1
    %extended_result = zext i1 %result to i64
    ret i64 %extended_result
  }
  
  define i64 @ll_var_0(i64 %arg_1) {
  entry:
    %arg_11 = alloca i64, align 8
    store i64 %arg_1, ptr %arg_11, align 4
    %arg_12 = load i64, ptr %arg_11, align 4
    %indirect_calltmp = call i64 @"<="(i64 %arg_12, i64 1)
    %anf_0 = alloca i64, align 8
    store i64 %indirect_calltmp, ptr %anf_0, align 4
    %anf_03 = load i64, ptr %anf_0, align 4
    %ifcond = icmp ne i64 %anf_03, 0
    br i1 %ifcond, label %then, label %else
  
  then:                                             ; preds = %entry
    br label %ifcont
  
  else:                                             ; preds = %entry
    %arg_14 = load i64, ptr %arg_11, align 4
    %indirect_calltmp5 = call i64 @-(i64 %arg_14, i64 1)
    %anf_1 = alloca i64, align 8
    store i64 %indirect_calltmp5, ptr %anf_1, align 4
    %anf_16 = load i64, ptr %anf_1, align 4
    %calltmp = call i64 @ll_var_0(i64 %anf_16)
    %anf_2 = alloca i64, align 8
    store i64 %calltmp, ptr %anf_2, align 4
    %arg_17 = load i64, ptr %arg_11, align 4
    %anf_28 = load i64, ptr %anf_2, align 4
    %indirect_calltmp9 = call i64 @"*"(i64 %arg_17, i64 %anf_28)
    %anf_3 = alloca i64, align 8
    store i64 %indirect_calltmp9, ptr %anf_3, align 4
    %anf_310 = load i64, ptr %anf_3, align 4
    br label %ifcont
  
  ifcont:                                           ; preds = %else, %then
    %iftmp = phi i64 [ 1, %then ], [ %anf_310, %else ]
    %anf_4 = alloca i64, align 8
    store i64 %iftmp, ptr %anf_4, align 4
    %anf_411 = load i64, ptr %anf_4, align 4
    ret i64 %anf_411
  }
  
  define i64 @ll_var_1() {
  entry:
    %calltmp = call i64 @ll_var_0(i64 4)
    %anf_5 = alloca i64, align 8
    store i64 %calltmp, ptr %anf_5, align 4
    %anf_51 = load i64, ptr %anf_5, align 4
    %calltmp2 = call i64 @print_int(i64 %anf_51)
    %anf_6 = alloca i64, align 8
    store i64 %calltmp2, ptr %anf_6, align 4
    %anf_63 = load i64, ptr %anf_6, align 4
    ret i64 0
  }
  
  define i64 @main() {
  entry:
    %main_result = call i64 @ll_var_1()
    ret i64 %main_result
  }
