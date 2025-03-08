
  $ ./llvm_demo.exe < manytests/typed/001fac.ml 
  declare i64 @print_int(i64)
  
  declare i64 @appClosure(i64, i64)
  
  declare i64 @addInClosure(i64, i64)
  
  define i64 @bin_op(i64 %n) {
  entry:
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 4
    %n2 = load i64, ptr %n1, align 4
    %leq_result = icmp sle i64 %n2, 1
    %leq_result3 = zext i1 %leq_result to i64
    ret i64 %leq_result3
  }
  
  define i64 @bin_op0(i64 %n) {
  entry:
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 4
    %n2 = load i64, ptr %n1, align 4
    %sub_result = sub i64 %n2, 1
    ret i64 %sub_result
  }
  
  define i64 @app(i64 %fac, i64 %n) {
  entry:
    %fac1 = alloca i64, align 8
    store i64 %fac, ptr %fac1, align 4
    %n2 = alloca i64, align 8
    store i64 %n, ptr %n2, align 4
    %fac3 = load i64, ptr %fac1, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op0 to i64), i64 1)
    %n4 = load i64, ptr %n2, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %n4)
    %AppClosure5 = call i64 @appClosure(i64 %fac3, i64 %AppClosure)
    ret i64 %AppClosure5
  }
  
  define i64 @bin_op1(i64 %fac, i64 %n) {
  entry:
    %fac1 = alloca i64, align 8
    store i64 %fac, ptr %fac1, align 4
    %n2 = alloca i64, align 8
    store i64 %n, ptr %n2, align 4
    %n3 = load i64, ptr %n2, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @app to i64), i64 2)
    %fac4 = load i64, ptr %fac1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %fac4)
    %n5 = load i64, ptr %n2, align 4
    %AppClosure6 = call i64 @appClosure(i64 %AppClosure, i64 %n5)
    %mul_result = mul i64 %n3, %AppClosure6
    ret i64 %mul_result
  }
  
  define i64 @fac(i64 %n) {
  entry:
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op to i64), i64 1)
    %n2 = load i64, ptr %n1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %n2)
    %if = icmp ne i64 %AppClosure, 0
    br i1 %if, label %then, label %else
  
  then:                                             ; preds = %entry
    br label %merge
  
  else:                                             ; preds = %entry
    %Closure3 = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op1 to i64), i64 2)
    %Closure4 = call i64 @addInClosure(i64 ptrtoint (ptr @fac to i64), i64 1)
    %AppClosure5 = call i64 @appClosure(i64 %Closure3, i64 %Closure4)
    %n6 = load i64, ptr %n1, align 4
    %AppClosure7 = call i64 @appClosure(i64 %AppClosure5, i64 %n6)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %iftmp = phi i64 [ 1, %then ], [ %AppClosure7, %else ]
    ret i64 %iftmp
  }
  
  define i64 @app0() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @fac to i64), i64 1)
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 4)
    ret i64 %AppClosure
  }
  
  define i64 @unit() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    %Closure1 = call i64 @addInClosure(i64 ptrtoint (ptr @app0 to i64), i64 0)
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %Closure1)
    ret i64 %AppClosure
  }
  
  define i64 @main() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @unit to i64), i64 0)
    ret i64 0
  }
  
  $ ./llvm_demo.exe < manytests/typed/002fac.ml 
  declare i64 @print_int(i64)
  
  declare i64 @appClosure(i64, i64)
  
  declare i64 @addInClosure(i64, i64)
  
  define i64 @bin_op(i64 %n, i64 %p) {
  entry:
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 4
    %p2 = alloca i64, align 8
    store i64 %p, ptr %p2, align 4
    %p3 = load i64, ptr %p2, align 4
    %n4 = load i64, ptr %n1, align 4
    %mul_result = mul i64 %p3, %n4
    ret i64 %mul_result
  }
  
  define i64 @lambada(i64 %k, i64 %n, i64 %p) {
  entry:
    %k1 = alloca i64, align 8
    store i64 %k, ptr %k1, align 4
    %n2 = alloca i64, align 8
    store i64 %n, ptr %n2, align 4
    %p3 = alloca i64, align 8
    store i64 %p, ptr %p3, align 4
    %k4 = load i64, ptr %k1, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op to i64), i64 2)
    %n5 = load i64, ptr %n2, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %n5)
    %p6 = load i64, ptr %p3, align 4
    %AppClosure7 = call i64 @appClosure(i64 %AppClosure, i64 %p6)
    %AppClosure8 = call i64 @appClosure(i64 %k4, i64 %AppClosure7)
    ret i64 %AppClosure8
  }
  
  define i64 @bin_op0(i64 %n) {
  entry:
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 4
    %n2 = load i64, ptr %n1, align 4
    %eq_result = icmp eq i64 %n2, 1
    %eq_result3 = zext i1 %eq_result to i64
    ret i64 %eq_result3
  }
  
  define i64 @app(i64 %k, i64 %n) {
  entry:
    %k1 = alloca i64, align 8
    store i64 %k, ptr %k1, align 4
    %n2 = alloca i64, align 8
    store i64 %n, ptr %n2, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @lambada to i64), i64 3)
    %k3 = load i64, ptr %k1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %k3)
    %n4 = load i64, ptr %n2, align 4
    %AppClosure5 = call i64 @appClosure(i64 %AppClosure, i64 %n4)
    ret i64 %AppClosure5
  }
  
  define i64 @bin_op1(i64 %n) {
  entry:
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 4
    %n2 = load i64, ptr %n1, align 4
    %sub_result = sub i64 %n2, 1
    ret i64 %sub_result
  }
  
  define i64 @fac_cps(i64 %n, i64 %k) {
  entry:
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 4
    %k2 = alloca i64, align 8
    store i64 %k, ptr %k2, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op0 to i64), i64 1)
    %n3 = load i64, ptr %n1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %n3)
    %if = icmp ne i64 %AppClosure, 0
    br i1 %if, label %then, label %else
  
  then:                                             ; preds = %entry
    %k4 = load i64, ptr %k2, align 4
    %AppClosure5 = call i64 @appClosure(i64 %k4, i64 1)
    br label %merge
  
  else:                                             ; preds = %entry
    %Closure6 = call i64 @addInClosure(i64 ptrtoint (ptr @fac_cps to i64), i64 2)
    %Closure7 = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op1 to i64), i64 1)
    %n8 = load i64, ptr %n1, align 4
    %AppClosure9 = call i64 @appClosure(i64 %Closure7, i64 %n8)
    %AppClosure10 = call i64 @appClosure(i64 %Closure6, i64 %AppClosure9)
    %Closure11 = call i64 @addInClosure(i64 ptrtoint (ptr @app to i64), i64 2)
    %k12 = load i64, ptr %k2, align 4
    %AppClosure13 = call i64 @appClosure(i64 %Closure11, i64 %k12)
    %n14 = load i64, ptr %n1, align 4
    %AppClosure15 = call i64 @appClosure(i64 %AppClosure13, i64 %n14)
    %AppClosure16 = call i64 @appClosure(i64 %AppClosure10, i64 %AppClosure15)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %iftmp = phi i64 [ %AppClosure5, %then ], [ %AppClosure16, %else ]
    ret i64 %iftmp
  }
  
  define i64 @lambada0(i64 %print_int) {
  entry:
    %print_int1 = alloca i64, align 8
    store i64 %print_int, ptr %print_int1, align 4
    %print_int2 = load i64, ptr %print_int1, align 4
    ret i64 %print_int2
  }
  
  define i64 @app0() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @fac_cps to i64), i64 2)
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 4)
    %Closure1 = call i64 @addInClosure(i64 ptrtoint (ptr @lambada0 to i64), i64 1)
    %AppClosure2 = call i64 @appClosure(i64 %AppClosure, i64 %Closure1)
    ret i64 %AppClosure2
  }
  
  define i64 @unit() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    %Closure1 = call i64 @addInClosure(i64 ptrtoint (ptr @app0 to i64), i64 0)
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %Closure1)
    ret i64 %AppClosure
  }
  
  define i64 @main() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @unit to i64), i64 0)
    ret i64 0
  }
  
  $ ./llvm_demo.exe < manytests/typed/003fib.ml 
  declare i64 @print_int(i64)
  
  declare i64 @appClosure(i64, i64)
  
  declare i64 @addInClosure(i64, i64)
  
  define i64 @bin_op(i64 %n) {
  entry:
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 4
    %n2 = load i64, ptr %n1, align 4
    %sub_result = sub i64 %n2, 1
    ret i64 %sub_result
  }
  
  define i64 @n1(i64 %n) {
  entry:
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op to i64), i64 1)
    %n2 = load i64, ptr %n1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %n2)
    ret i64 %AppClosure
  }
  
  define i64 @bin_op0(i64 %a, i64 %b) {
  entry:
    %a1 = alloca i64, align 8
    store i64 %a, ptr %a1, align 4
    %b2 = alloca i64, align 8
    store i64 %b, ptr %b2, align 4
    %a3 = load i64, ptr %a1, align 4
    %b4 = load i64, ptr %b2, align 4
    %add_result = add i64 %a3, %b4
    ret i64 %add_result
  }
  
  define i64 @ab(i64 %a, i64 %b) {
  entry:
    %a1 = alloca i64, align 8
    store i64 %a, ptr %a1, align 4
    %b2 = alloca i64, align 8
    store i64 %b, ptr %b2, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op0 to i64), i64 2)
    %a3 = load i64, ptr %a1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %a3)
    %b4 = load i64, ptr %b2, align 4
    %AppClosure5 = call i64 @appClosure(i64 %AppClosure, i64 %b4)
    ret i64 %AppClosure5
  }
  
  define i64 @bin_op1(i64 %n) {
  entry:
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 4
    %n2 = load i64, ptr %n1, align 4
    %eq_result = icmp eq i64 %n2, 1
    %eq_result3 = zext i1 %eq_result to i64
    ret i64 %eq_result3
  }
  
  define i64 @app(i64 %n) {
  entry:
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @n1 to i64), i64 1)
    %n2 = load i64, ptr %n1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %n2)
    ret i64 %AppClosure
  }
  
  define i64 @app0(i64 %a, i64 %b) {
  entry:
    %a1 = alloca i64, align 8
    store i64 %a, ptr %a1, align 4
    %b2 = alloca i64, align 8
    store i64 %b, ptr %b2, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @ab to i64), i64 2)
    %a3 = load i64, ptr %a1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %a3)
    %b4 = load i64, ptr %b2, align 4
    %AppClosure5 = call i64 @appClosure(i64 %AppClosure, i64 %b4)
    ret i64 %AppClosure5
  }
  
  define i64 @fib_acc(i64 %a, i64 %b, i64 %n) {
  entry:
    %a1 = alloca i64, align 8
    store i64 %a, ptr %a1, align 4
    %b2 = alloca i64, align 8
    store i64 %b, ptr %b2, align 4
    %n3 = alloca i64, align 8
    store i64 %n, ptr %n3, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op1 to i64), i64 1)
    %n4 = load i64, ptr %n3, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %n4)
    %if = icmp ne i64 %AppClosure, 0
    br i1 %if, label %then, label %else
  
  then:                                             ; preds = %entry
    %b5 = load i64, ptr %b2, align 4
    br label %merge
  
  else:                                             ; preds = %entry
    %Closure6 = call i64 @addInClosure(i64 ptrtoint (ptr @fib_acc to i64), i64 3)
    %b7 = load i64, ptr %b2, align 4
    %AppClosure8 = call i64 @appClosure(i64 %Closure6, i64 %b7)
    %Closure9 = call i64 @addInClosure(i64 ptrtoint (ptr @app0 to i64), i64 2)
    %a10 = load i64, ptr %a1, align 4
    %AppClosure11 = call i64 @appClosure(i64 %Closure9, i64 %a10)
    %b12 = load i64, ptr %b2, align 4
    %AppClosure13 = call i64 @appClosure(i64 %AppClosure11, i64 %b12)
    %AppClosure14 = call i64 @appClosure(i64 %AppClosure8, i64 %AppClosure13)
    %Closure15 = call i64 @addInClosure(i64 ptrtoint (ptr @app to i64), i64 1)
    %n16 = load i64, ptr %n3, align 4
    %AppClosure17 = call i64 @appClosure(i64 %Closure15, i64 %n16)
    %AppClosure18 = call i64 @appClosure(i64 %AppClosure14, i64 %AppClosure17)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %iftmp = phi i64 [ %b5, %then ], [ %AppClosure18, %else ]
    ret i64 %iftmp
  }
  
  define i64 @bin_op2(i64 %n) {
  entry:
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 4
    %n2 = load i64, ptr %n1, align 4
    %less_result = icmp slt i64 %n2, 2
    %less_result3 = zext i1 %less_result to i64
    ret i64 %less_result3
  }
  
  define i64 @bin_op3(i64 %n) {
  entry:
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 4
    %n2 = load i64, ptr %n1, align 4
    %sub_result = sub i64 %n2, 2
    ret i64 %sub_result
  }
  
  define i64 @app1(i64 %fib, i64 %n) {
  entry:
    %fib1 = alloca i64, align 8
    store i64 %fib, ptr %fib1, align 4
    %n2 = alloca i64, align 8
    store i64 %n, ptr %n2, align 4
    %fib3 = load i64, ptr %fib1, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op3 to i64), i64 1)
    %n4 = load i64, ptr %n2, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %n4)
    %AppClosure5 = call i64 @appClosure(i64 %fib3, i64 %AppClosure)
    ret i64 %AppClosure5
  }
  
  define i64 @bin_op5(i64 %n) {
  entry:
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 4
    %n2 = load i64, ptr %n1, align 4
    %sub_result = sub i64 %n2, 1
    ret i64 %sub_result
  }
  
  define i64 @bin_op4(i64 %fib, i64 %n) {
  entry:
    %fib1 = alloca i64, align 8
    store i64 %fib, ptr %fib1, align 4
    %n2 = alloca i64, align 8
    store i64 %n, ptr %n2, align 4
    %fib3 = load i64, ptr %fib1, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op5 to i64), i64 1)
    %n4 = load i64, ptr %n2, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %n4)
    %AppClosure5 = call i64 @appClosure(i64 %fib3, i64 %AppClosure)
    %Closure6 = call i64 @addInClosure(i64 ptrtoint (ptr @app1 to i64), i64 2)
    %fib7 = load i64, ptr %fib1, align 4
    %AppClosure8 = call i64 @appClosure(i64 %Closure6, i64 %fib7)
    %n9 = load i64, ptr %n2, align 4
    %AppClosure10 = call i64 @appClosure(i64 %AppClosure8, i64 %n9)
    %add_result = add i64 %AppClosure5, %AppClosure10
    ret i64 %add_result
  }
  
  define i64 @fib(i64 %n) {
  entry:
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op2 to i64), i64 1)
    %n2 = load i64, ptr %n1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %n2)
    %if = icmp ne i64 %AppClosure, 0
    br i1 %if, label %then, label %else
  
  then:                                             ; preds = %entry
    %n3 = load i64, ptr %n1, align 4
    br label %merge
  
  else:                                             ; preds = %entry
    %Closure4 = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op4 to i64), i64 2)
    %Closure5 = call i64 @addInClosure(i64 ptrtoint (ptr @fib to i64), i64 1)
    %AppClosure6 = call i64 @appClosure(i64 %Closure4, i64 %Closure5)
    %n7 = load i64, ptr %n1, align 4
    %AppClosure8 = call i64 @appClosure(i64 %AppClosure6, i64 %n7)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %iftmp = phi i64 [ %n3, %then ], [ %AppClosure8, %else ]
    ret i64 %iftmp
  }
  
  define i64 @app2() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @fib_acc to i64), i64 3)
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 0)
    %AppClosure1 = call i64 @appClosure(i64 %AppClosure, i64 1)
    %AppClosure2 = call i64 @appClosure(i64 %AppClosure1, i64 4)
    ret i64 %AppClosure2
  }
  
  define i64 @unit() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    %Closure1 = call i64 @addInClosure(i64 ptrtoint (ptr @app2 to i64), i64 0)
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %Closure1)
    ret i64 %AppClosure
  }
  
  define i64 @app3() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @fib to i64), i64 1)
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 4)
    ret i64 %AppClosure
  }
  
  define i64 @unit0() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    %Closure1 = call i64 @addInClosure(i64 ptrtoint (ptr @app3 to i64), i64 0)
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %Closure1)
    ret i64 %AppClosure
  }
  
  define i64 @main() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @unit to i64), i64 0)
    %Closure1 = call i64 @addInClosure(i64 ptrtoint (ptr @unit0 to i64), i64 0)
    ret i64 0
  }
  
  $ ./llvm_demo.exe < manytests/typed/004manyargs.ml 
  declare i64 @print_int(i64)
  
  declare i64 @appClosure(i64, i64)
  
  declare i64 @addInClosure(i64, i64)
  
  define i64 @bin_op() {
  entry:
    ret i64 1
  }
  
  define i64 @wrap(i64 %f) {
  entry:
    %f1 = alloca i64, align 8
    store i64 %f, ptr %f1, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op to i64), i64 0)
    %if = icmp ne i64 %Closure, 0
    br i1 %if, label %then, label %else
  
  then:                                             ; preds = %entry
    %f2 = load i64, ptr %f1, align 4
    br label %merge
  
  else:                                             ; preds = %entry
    %f3 = load i64, ptr %f1, align 4
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %iftmp = phi i64 [ %f2, %then ], [ %f3, %else ]
    ret i64 %iftmp
  }
  
  define i64 @a0(i64 %a) {
  entry:
    %a1 = alloca i64, align 8
    store i64 %a, ptr %a1, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    %a2 = load i64, ptr %a1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %a2)
    ret i64 %AppClosure
  }
  
  define i64 @b0(i64 %b) {
  entry:
    %b1 = alloca i64, align 8
    store i64 %b, ptr %b1, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    %b2 = load i64, ptr %b1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %b2)
    ret i64 %AppClosure
  }
  
  define i64 @c0(i64 %c) {
  entry:
    %c1 = alloca i64, align 8
    store i64 %c, ptr %c1, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    %c2 = load i64, ptr %c1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %c2)
    ret i64 %AppClosure
  }
  
  define i64 @test3(i64 %a, i64 %b, i64 %c) {
  entry:
    %a1 = alloca i64, align 8
    store i64 %a, ptr %a1, align 4
    %b2 = alloca i64, align 8
    store i64 %b, ptr %b2, align 4
    %c3 = alloca i64, align 8
    store i64 %c, ptr %c3, align 4
    ret i64 0
  }
  
  define i64 @bin_op8(i64 %a, i64 %b) {
  entry:
    %a1 = alloca i64, align 8
    store i64 %a, ptr %a1, align 4
    %b2 = alloca i64, align 8
    store i64 %b, ptr %b2, align 4
    %a3 = load i64, ptr %a1, align 4
    %b4 = load i64, ptr %b2, align 4
    %add_result = add i64 %a3, %b4
    ret i64 %add_result
  }
  
  define i64 @bin_op7(i64 %a, i64 %b, i64 %c) {
  entry:
    %a1 = alloca i64, align 8
    store i64 %a, ptr %a1, align 4
    %b2 = alloca i64, align 8
    store i64 %b, ptr %b2, align 4
    %c3 = alloca i64, align 8
    store i64 %c, ptr %c3, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op8 to i64), i64 2)
    %a4 = load i64, ptr %a1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %a4)
    %b5 = load i64, ptr %b2, align 4
    %AppClosure6 = call i64 @appClosure(i64 %AppClosure, i64 %b5)
    %c7 = load i64, ptr %c3, align 4
    %add_result = add i64 %AppClosure6, %c7
    ret i64 %add_result
  }
  
  define i64 @bin_op6(i64 %a, i64 %b, i64 %c, i64 %d) {
  entry:
    %a1 = alloca i64, align 8
    store i64 %a, ptr %a1, align 4
    %b2 = alloca i64, align 8
    store i64 %b, ptr %b2, align 4
    %c3 = alloca i64, align 8
    store i64 %c, ptr %c3, align 4
    %d4 = alloca i64, align 8
    store i64 %d, ptr %d4, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op7 to i64), i64 3)
    %a5 = load i64, ptr %a1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %a5)
    %b6 = load i64, ptr %b2, align 4
    %AppClosure7 = call i64 @appClosure(i64 %AppClosure, i64 %b6)
    %c8 = load i64, ptr %c3, align 4
    %AppClosure9 = call i64 @appClosure(i64 %AppClosure7, i64 %c8)
    %d10 = load i64, ptr %d4, align 4
    %add_result = add i64 %AppClosure9, %d10
    ret i64 %add_result
  }
  
  define i64 @bin_op5(i64 %a, i64 %b, i64 %c, i64 %d, i64 %e) {
  entry:
    %a1 = alloca i64, align 8
    store i64 %a, ptr %a1, align 4
    %b2 = alloca i64, align 8
    store i64 %b, ptr %b2, align 4
    %c3 = alloca i64, align 8
    store i64 %c, ptr %c3, align 4
    %d4 = alloca i64, align 8
    store i64 %d, ptr %d4, align 4
    %e5 = alloca i64, align 8
    store i64 %e, ptr %e5, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op6 to i64), i64 4)
    %a6 = load i64, ptr %a1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %a6)
    %b7 = load i64, ptr %b2, align 4
    %AppClosure8 = call i64 @appClosure(i64 %AppClosure, i64 %b7)
    %c9 = load i64, ptr %c3, align 4
    %AppClosure10 = call i64 @appClosure(i64 %AppClosure8, i64 %c9)
    %d11 = load i64, ptr %d4, align 4
    %AppClosure12 = call i64 @appClosure(i64 %AppClosure10, i64 %d11)
    %e13 = load i64, ptr %e5, align 4
    %add_result = add i64 %AppClosure12, %e13
    ret i64 %add_result
  }
  
  define i64 @bin_op4(i64 %a, i64 %b, i64 %c, i64 %d, i64 %e, i64 %f) {
  entry:
    %a1 = alloca i64, align 8
    store i64 %a, ptr %a1, align 4
    %b2 = alloca i64, align 8
    store i64 %b, ptr %b2, align 4
    %c3 = alloca i64, align 8
    store i64 %c, ptr %c3, align 4
    %d4 = alloca i64, align 8
    store i64 %d, ptr %d4, align 4
    %e5 = alloca i64, align 8
    store i64 %e, ptr %e5, align 4
    %f6 = alloca i64, align 8
    store i64 %f, ptr %f6, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op5 to i64), i64 5)
    %a7 = load i64, ptr %a1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %a7)
    %b8 = load i64, ptr %b2, align 4
    %AppClosure9 = call i64 @appClosure(i64 %AppClosure, i64 %b8)
    %c10 = load i64, ptr %c3, align 4
    %AppClosure11 = call i64 @appClosure(i64 %AppClosure9, i64 %c10)
    %d12 = load i64, ptr %d4, align 4
    %AppClosure13 = call i64 @appClosure(i64 %AppClosure11, i64 %d12)
    %e14 = load i64, ptr %e5, align 4
    %AppClosure15 = call i64 @appClosure(i64 %AppClosure13, i64 %e14)
    %f16 = load i64, ptr %f6, align 4
    %add_result = add i64 %AppClosure15, %f16
    ret i64 %add_result
  }
  
  define i64 @bin_op3(i64 %a, i64 %b, i64 %c, i64 %d, i64 %e, i64 %f, i64 %g) {
  entry:
    %a1 = alloca i64, align 8
    store i64 %a, ptr %a1, align 4
    %b2 = alloca i64, align 8
    store i64 %b, ptr %b2, align 4
    %c3 = alloca i64, align 8
    store i64 %c, ptr %c3, align 4
    %d4 = alloca i64, align 8
    store i64 %d, ptr %d4, align 4
    %e5 = alloca i64, align 8
    store i64 %e, ptr %e5, align 4
    %f6 = alloca i64, align 8
    store i64 %f, ptr %f6, align 4
    %g7 = alloca i64, align 8
    store i64 %g, ptr %g7, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op4 to i64), i64 6)
    %a8 = load i64, ptr %a1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %a8)
    %b9 = load i64, ptr %b2, align 4
    %AppClosure10 = call i64 @appClosure(i64 %AppClosure, i64 %b9)
    %c11 = load i64, ptr %c3, align 4
    %AppClosure12 = call i64 @appClosure(i64 %AppClosure10, i64 %c11)
    %d13 = load i64, ptr %d4, align 4
    %AppClosure14 = call i64 @appClosure(i64 %AppClosure12, i64 %d13)
    %e15 = load i64, ptr %e5, align 4
    %AppClosure16 = call i64 @appClosure(i64 %AppClosure14, i64 %e15)
    %f17 = load i64, ptr %f6, align 4
    %AppClosure18 = call i64 @appClosure(i64 %AppClosure16, i64 %f17)
    %g19 = load i64, ptr %g7, align 4
    %add_result = add i64 %AppClosure18, %g19
    ret i64 %add_result
  }
  
  define i64 @bin_op2(i64 %a, i64 %b, i64 %c, i64 %d, i64 %e, i64 %f, i64 %g, i64 %h) {
  entry:
    %a1 = alloca i64, align 8
    store i64 %a, ptr %a1, align 4
    %b2 = alloca i64, align 8
    store i64 %b, ptr %b2, align 4
    %c3 = alloca i64, align 8
    store i64 %c, ptr %c3, align 4
    %d4 = alloca i64, align 8
    store i64 %d, ptr %d4, align 4
    %e5 = alloca i64, align 8
    store i64 %e, ptr %e5, align 4
    %f6 = alloca i64, align 8
    store i64 %f, ptr %f6, align 4
    %g7 = alloca i64, align 8
    store i64 %g, ptr %g7, align 4
    %h8 = alloca i64, align 8
    store i64 %h, ptr %h8, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op3 to i64), i64 7)
    %a9 = load i64, ptr %a1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %a9)
    %b10 = load i64, ptr %b2, align 4
    %AppClosure11 = call i64 @appClosure(i64 %AppClosure, i64 %b10)
    %c12 = load i64, ptr %c3, align 4
    %AppClosure13 = call i64 @appClosure(i64 %AppClosure11, i64 %c12)
    %d14 = load i64, ptr %d4, align 4
    %AppClosure15 = call i64 @appClosure(i64 %AppClosure13, i64 %d14)
    %e16 = load i64, ptr %e5, align 4
    %AppClosure17 = call i64 @appClosure(i64 %AppClosure15, i64 %e16)
    %f18 = load i64, ptr %f6, align 4
    %AppClosure19 = call i64 @appClosure(i64 %AppClosure17, i64 %f18)
    %g20 = load i64, ptr %g7, align 4
    %AppClosure21 = call i64 @appClosure(i64 %AppClosure19, i64 %g20)
    %h22 = load i64, ptr %h8, align 4
    %add_result = add i64 %AppClosure21, %h22
    ret i64 %add_result
  }
  
  define i64 @bin_op1(i64 %a, i64 %b, i64 %c, i64 %d, i64 %e, i64 %f, i64 %g, i64 %h, i64 %i) {
  entry:
    %a1 = alloca i64, align 8
    store i64 %a, ptr %a1, align 4
    %b2 = alloca i64, align 8
    store i64 %b, ptr %b2, align 4
    %c3 = alloca i64, align 8
    store i64 %c, ptr %c3, align 4
    %d4 = alloca i64, align 8
    store i64 %d, ptr %d4, align 4
    %e5 = alloca i64, align 8
    store i64 %e, ptr %e5, align 4
    %f6 = alloca i64, align 8
    store i64 %f, ptr %f6, align 4
    %g7 = alloca i64, align 8
    store i64 %g, ptr %g7, align 4
    %h8 = alloca i64, align 8
    store i64 %h, ptr %h8, align 4
    %i9 = alloca i64, align 8
    store i64 %i, ptr %i9, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op2 to i64), i64 8)
    %a10 = load i64, ptr %a1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %a10)
    %b11 = load i64, ptr %b2, align 4
    %AppClosure12 = call i64 @appClosure(i64 %AppClosure, i64 %b11)
    %c13 = load i64, ptr %c3, align 4
    %AppClosure14 = call i64 @appClosure(i64 %AppClosure12, i64 %c13)
    %d15 = load i64, ptr %d4, align 4
    %AppClosure16 = call i64 @appClosure(i64 %AppClosure14, i64 %d15)
    %e17 = load i64, ptr %e5, align 4
    %AppClosure18 = call i64 @appClosure(i64 %AppClosure16, i64 %e17)
    %f19 = load i64, ptr %f6, align 4
    %AppClosure20 = call i64 @appClosure(i64 %AppClosure18, i64 %f19)
    %g21 = load i64, ptr %g7, align 4
    %AppClosure22 = call i64 @appClosure(i64 %AppClosure20, i64 %g21)
    %h23 = load i64, ptr %h8, align 4
    %AppClosure24 = call i64 @appClosure(i64 %AppClosure22, i64 %h23)
    %i25 = load i64, ptr %i9, align 4
    %add_result = add i64 %AppClosure24, %i25
    ret i64 %add_result
  }
  
  define i64 @bin_op0(i64 %a, i64 %b, i64 %c, i64 %d, i64 %e, i64 %f, i64 %g, i64 %h, i64 %i, i64 %j) {
  entry:
    %a1 = alloca i64, align 8
    store i64 %a, ptr %a1, align 4
    %b2 = alloca i64, align 8
    store i64 %b, ptr %b2, align 4
    %c3 = alloca i64, align 8
    store i64 %c, ptr %c3, align 4
    %d4 = alloca i64, align 8
    store i64 %d, ptr %d4, align 4
    %e5 = alloca i64, align 8
    store i64 %e, ptr %e5, align 4
    %f6 = alloca i64, align 8
    store i64 %f, ptr %f6, align 4
    %g7 = alloca i64, align 8
    store i64 %g, ptr %g7, align 4
    %h8 = alloca i64, align 8
    store i64 %h, ptr %h8, align 4
    %i9 = alloca i64, align 8
    store i64 %i, ptr %i9, align 4
    %j10 = alloca i64, align 8
    store i64 %j, ptr %j10, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op1 to i64), i64 9)
    %a11 = load i64, ptr %a1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %a11)
    %b12 = load i64, ptr %b2, align 4
    %AppClosure13 = call i64 @appClosure(i64 %AppClosure, i64 %b12)
    %c14 = load i64, ptr %c3, align 4
    %AppClosure15 = call i64 @appClosure(i64 %AppClosure13, i64 %c14)
    %d16 = load i64, ptr %d4, align 4
    %AppClosure17 = call i64 @appClosure(i64 %AppClosure15, i64 %d16)
    %e18 = load i64, ptr %e5, align 4
    %AppClosure19 = call i64 @appClosure(i64 %AppClosure17, i64 %e18)
    %f20 = load i64, ptr %f6, align 4
    %AppClosure21 = call i64 @appClosure(i64 %AppClosure19, i64 %f20)
    %g22 = load i64, ptr %g7, align 4
    %AppClosure23 = call i64 @appClosure(i64 %AppClosure21, i64 %g22)
    %h24 = load i64, ptr %h8, align 4
    %AppClosure25 = call i64 @appClosure(i64 %AppClosure23, i64 %h24)
    %i26 = load i64, ptr %i9, align 4
    %AppClosure27 = call i64 @appClosure(i64 %AppClosure25, i64 %i26)
    %j28 = load i64, ptr %j10, align 4
    %add_result = add i64 %AppClosure27, %j28
    ret i64 %add_result
  }
  
  define i64 @test10(i64 %a, i64 %b, i64 %c, i64 %d, i64 %e, i64 %f, i64 %g, i64 %h, i64 %i, i64 %j) {
  entry:
    %a1 = alloca i64, align 8
    store i64 %a, ptr %a1, align 4
    %b2 = alloca i64, align 8
    store i64 %b, ptr %b2, align 4
    %c3 = alloca i64, align 8
    store i64 %c, ptr %c3, align 4
    %d4 = alloca i64, align 8
    store i64 %d, ptr %d4, align 4
    %e5 = alloca i64, align 8
    store i64 %e, ptr %e5, align 4
    %f6 = alloca i64, align 8
    store i64 %f, ptr %f6, align 4
    %g7 = alloca i64, align 8
    store i64 %g, ptr %g7, align 4
    %h8 = alloca i64, align 8
    store i64 %h, ptr %h8, align 4
    %i9 = alloca i64, align 8
    store i64 %i, ptr %i9, align 4
    %j10 = alloca i64, align 8
    store i64 %j, ptr %j10, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op0 to i64), i64 10)
    %a11 = load i64, ptr %a1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %a11)
    %b12 = load i64, ptr %b2, align 4
    %AppClosure13 = call i64 @appClosure(i64 %AppClosure, i64 %b12)
    %c14 = load i64, ptr %c3, align 4
    %AppClosure15 = call i64 @appClosure(i64 %AppClosure13, i64 %c14)
    %d16 = load i64, ptr %d4, align 4
    %AppClosure17 = call i64 @appClosure(i64 %AppClosure15, i64 %d16)
    %e18 = load i64, ptr %e5, align 4
    %AppClosure19 = call i64 @appClosure(i64 %AppClosure17, i64 %e18)
    %f20 = load i64, ptr %f6, align 4
    %AppClosure21 = call i64 @appClosure(i64 %AppClosure19, i64 %f20)
    %g22 = load i64, ptr %g7, align 4
    %AppClosure23 = call i64 @appClosure(i64 %AppClosure21, i64 %g22)
    %h24 = load i64, ptr %h8, align 4
    %AppClosure25 = call i64 @appClosure(i64 %AppClosure23, i64 %h24)
    %i26 = load i64, ptr %i9, align 4
    %AppClosure27 = call i64 @appClosure(i64 %AppClosure25, i64 %i26)
    %j28 = load i64, ptr %j10, align 4
    %AppClosure29 = call i64 @appClosure(i64 %AppClosure27, i64 %j28)
    ret i64 %AppClosure29
  }
  
  define i64 @rez() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @wrap to i64), i64 1)
    %Closure1 = call i64 @addInClosure(i64 ptrtoint (ptr @test10 to i64), i64 10)
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %Closure1)
    %AppClosure2 = call i64 @appClosure(i64 %AppClosure, i64 1)
    %AppClosure3 = call i64 @appClosure(i64 %AppClosure2, i64 10)
    %AppClosure4 = call i64 @appClosure(i64 %AppClosure3, i64 100)
    %AppClosure5 = call i64 @appClosure(i64 %AppClosure4, i64 1000)
    %AppClosure6 = call i64 @appClosure(i64 %AppClosure5, i64 10000)
    %AppClosure7 = call i64 @appClosure(i64 %AppClosure6, i64 100000)
    %AppClosure8 = call i64 @appClosure(i64 %AppClosure7, i64 1000000)
    %AppClosure9 = call i64 @appClosure(i64 %AppClosure8, i64 10000000)
    %AppClosure10 = call i64 @appClosure(i64 %AppClosure9, i64 100000000)
    %AppClosure11 = call i64 @appClosure(i64 %AppClosure10, i64 1000000000)
    ret i64 %AppClosure11
  }
  
  define i64 @unit() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    %Closure1 = call i64 @addInClosure(i64 ptrtoint (ptr @rez to i64), i64 0)
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %Closure1)
    ret i64 %AppClosure
  }
  
  define i64 @temp2() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @wrap to i64), i64 1)
    %Closure1 = call i64 @addInClosure(i64 ptrtoint (ptr @test3 to i64), i64 3)
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %Closure1)
    %AppClosure2 = call i64 @appClosure(i64 %AppClosure, i64 1)
    %AppClosure3 = call i64 @appClosure(i64 %AppClosure2, i64 10)
    %AppClosure4 = call i64 @appClosure(i64 %AppClosure3, i64 100)
    ret i64 %AppClosure4
  }
  
  define i64 @main() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @unit to i64), i64 0)
    ret i64 0
  }
  
  $ ./llvm_demo.exe < manytests/typed/005fix.ml 
  declare i64 @print_int(i64)
  
  declare i64 @appClosure(i64, i64)
  
  declare i64 @addInClosure(i64, i64)
  
  define i64 @app(i64 %f, i64 %fix) {
  entry:
    %f1 = alloca i64, align 8
    store i64 %f, ptr %f1, align 4
    %fix2 = alloca i64, align 8
    store i64 %fix, ptr %fix2, align 4
    %fix3 = load i64, ptr %fix2, align 4
    %f4 = load i64, ptr %f1, align 4
    %AppClosure = call i64 @appClosure(i64 %fix3, i64 %f4)
    ret i64 %AppClosure
  }
  
  define i64 @fix(i64 %f, i64 %x) {
  entry:
    %f1 = alloca i64, align 8
    store i64 %f, ptr %f1, align 4
    %x2 = alloca i64, align 8
    store i64 %x, ptr %x2, align 4
    %f3 = load i64, ptr %f1, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @app to i64), i64 2)
    %f4 = load i64, ptr %f1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %f4)
    %Closure5 = call i64 @addInClosure(i64 ptrtoint (ptr @fix to i64), i64 2)
    %AppClosure6 = call i64 @appClosure(i64 %AppClosure, i64 %Closure5)
    %AppClosure7 = call i64 @appClosure(i64 %f3, i64 %AppClosure6)
    %x8 = load i64, ptr %x2, align 4
    %AppClosure9 = call i64 @appClosure(i64 %AppClosure7, i64 %x8)
    ret i64 %AppClosure9
  }
  
  define i64 @bin_op(i64 %n) {
  entry:
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 4
    %n2 = load i64, ptr %n1, align 4
    %leq_result = icmp sle i64 %n2, 1
    %leq_result3 = zext i1 %leq_result to i64
    ret i64 %leq_result3
  }
  
  define i64 @bin_op0(i64 %n) {
  entry:
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 4
    %n2 = load i64, ptr %n1, align 4
    %sub_result = sub i64 %n2, 1
    ret i64 %sub_result
  }
  
  define i64 @app0(i64 %n, i64 %self) {
  entry:
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 4
    %self2 = alloca i64, align 8
    store i64 %self, ptr %self2, align 4
    %self3 = load i64, ptr %self2, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op0 to i64), i64 1)
    %n4 = load i64, ptr %n1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %n4)
    %AppClosure5 = call i64 @appClosure(i64 %self3, i64 %AppClosure)
    ret i64 %AppClosure5
  }
  
  define i64 @bin_op1(i64 %n, i64 %self) {
  entry:
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 4
    %self2 = alloca i64, align 8
    store i64 %self, ptr %self2, align 4
    %n3 = load i64, ptr %n1, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @app0 to i64), i64 2)
    %n4 = load i64, ptr %n1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %n4)
    %self5 = load i64, ptr %self2, align 4
    %AppClosure6 = call i64 @appClosure(i64 %AppClosure, i64 %self5)
    %mul_result = mul i64 %n3, %AppClosure6
    ret i64 %mul_result
  }
  
  define i64 @fac(i64 %self, i64 %n) {
  entry:
    %self1 = alloca i64, align 8
    store i64 %self, ptr %self1, align 4
    %n2 = alloca i64, align 8
    store i64 %n, ptr %n2, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op to i64), i64 1)
    %n3 = load i64, ptr %n2, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %n3)
    %if = icmp ne i64 %AppClosure, 0
    br i1 %if, label %then, label %else
  
  then:                                             ; preds = %entry
    br label %merge
  
  else:                                             ; preds = %entry
    %Closure4 = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op1 to i64), i64 2)
    %n5 = load i64, ptr %n2, align 4
    %AppClosure6 = call i64 @appClosure(i64 %Closure4, i64 %n5)
    %self7 = load i64, ptr %self1, align 4
    %AppClosure8 = call i64 @appClosure(i64 %AppClosure6, i64 %self7)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %iftmp = phi i64 [ 1, %then ], [ %AppClosure8, %else ]
    ret i64 %iftmp
  }
  
  define i64 @app1() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @fix to i64), i64 2)
    %Closure1 = call i64 @addInClosure(i64 ptrtoint (ptr @fac to i64), i64 2)
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %Closure1)
    %AppClosure2 = call i64 @appClosure(i64 %AppClosure, i64 6)
    ret i64 %AppClosure2
  }
  
  define i64 @unit() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    %Closure1 = call i64 @addInClosure(i64 ptrtoint (ptr @app1 to i64), i64 0)
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %Closure1)
    ret i64 %AppClosure
  }
  
  define i64 @main() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @unit to i64), i64 0)
    ret i64 0
  }
  

  $ ./llvm_demo.exe < manytests/typed/006partial2.ml 
  declare i64 @print_int(i64)
  
  declare i64 @appClosure(i64, i64)
  
  declare i64 @addInClosure(i64, i64)
  
  define i64 @unit(i64 %a) {
  entry:
    %a1 = alloca i64, align 8
    store i64 %a, ptr %a1, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    %a2 = load i64, ptr %a1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %a2)
    ret i64 %AppClosure
  }
  
  define i64 @unit0(i64 %b) {
  entry:
    %b1 = alloca i64, align 8
    store i64 %b, ptr %b1, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    %b2 = load i64, ptr %b1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %b2)
    ret i64 %AppClosure
  }
  
  define i64 @unit1(i64 %c) {
  entry:
    %c1 = alloca i64, align 8
    store i64 %c, ptr %c1, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    %c2 = load i64, ptr %c1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %c2)
    ret i64 %AppClosure
  }
  
  define i64 @bin_op0(i64 %b, i64 %c) {
  entry:
    %b1 = alloca i64, align 8
    store i64 %b, ptr %b1, align 4
    %c2 = alloca i64, align 8
    store i64 %c, ptr %c2, align 4
    %b3 = load i64, ptr %b1, align 4
    %c4 = load i64, ptr %c2, align 4
    %mul_result = mul i64 %b3, %c4
    ret i64 %mul_result
  }
  
  define i64 @bin_op(i64 %a, i64 %b, i64 %c) {
  entry:
    %a1 = alloca i64, align 8
    store i64 %a, ptr %a1, align 4
    %b2 = alloca i64, align 8
    store i64 %b, ptr %b2, align 4
    %c3 = alloca i64, align 8
    store i64 %c, ptr %c3, align 4
    %a4 = load i64, ptr %a1, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op0 to i64), i64 2)
    %b5 = load i64, ptr %b2, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %b5)
    %c6 = load i64, ptr %c3, align 4
    %AppClosure7 = call i64 @appClosure(i64 %AppClosure, i64 %c6)
    %add_result = add i64 %a4, %AppClosure7
    ret i64 %add_result
  }
  
  define i64 @foo(i64 %a, i64 %b, i64 %c) {
  entry:
    %a1 = alloca i64, align 8
    store i64 %a, ptr %a1, align 4
    %b2 = alloca i64, align 8
    store i64 %b, ptr %b2, align 4
    %c3 = alloca i64, align 8
    store i64 %c, ptr %c3, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @unit to i64), i64 1)
    %a4 = load i64, ptr %a1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %a4)
    %Closure5 = call i64 @addInClosure(i64 ptrtoint (ptr @unit0 to i64), i64 1)
    %b6 = load i64, ptr %b2, align 4
    %AppClosure7 = call i64 @appClosure(i64 %Closure5, i64 %b6)
    %Closure8 = call i64 @addInClosure(i64 ptrtoint (ptr @unit1 to i64), i64 1)
    %c9 = load i64, ptr %c3, align 4
    %AppClosure10 = call i64 @appClosure(i64 %Closure8, i64 %c9)
    %Closure11 = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op to i64), i64 3)
    %a12 = load i64, ptr %a1, align 4
    %AppClosure13 = call i64 @appClosure(i64 %Closure11, i64 %a12)
    %b14 = load i64, ptr %b2, align 4
    %AppClosure15 = call i64 @appClosure(i64 %AppClosure13, i64 %b14)
    %c16 = load i64, ptr %c3, align 4
    %AppClosure17 = call i64 @appClosure(i64 %AppClosure15, i64 %c16)
    ret i64 %AppClosure17
  }
  
  define i64 @foo0() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @foo to i64), i64 3)
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 1)
    ret i64 %AppClosure
  }
  
  define i64 @foo1() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @foo0 to i64), i64 0)
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 2)
    ret i64 %AppClosure
  }
  
  define i64 @foo2() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @foo1 to i64), i64 0)
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 3)
    ret i64 %AppClosure
  }
  
  define i64 @unit2() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    %Closure1 = call i64 @addInClosure(i64 ptrtoint (ptr @foo2 to i64), i64 0)
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %Closure1)
    ret i64 %AppClosure
  }
  
  define i64 @main() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @unit2 to i64), i64 0)
    ret i64 0
  }
  
  $ ./llvm_demo.exe < manytests/typed/006partial3.ml 
  declare i64 @print_int(i64)
  
  declare i64 @appClosure(i64, i64)
  
  declare i64 @addInClosure(i64, i64)
  
  define i64 @lambada0(i64 %print_int, i64 %c) {
  entry:
    %print_int1 = alloca i64, align 8
    store i64 %print_int, ptr %print_int1, align 4
    %c2 = alloca i64, align 8
    store i64 %c, ptr %c2, align 4
    %print_int3 = load i64, ptr %print_int1, align 4
    %c4 = load i64, ptr %c2, align 4
    %AppClosure = call i64 @appClosure(i64 %print_int3, i64 %c4)
    ret i64 %AppClosure
  }
  
  define i64 @unit(i64 %b) {
  entry:
    %b1 = alloca i64, align 8
    store i64 %b, ptr %b1, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    %b2 = load i64, ptr %b1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %b2)
    ret i64 %AppClosure
  }
  
  define i64 @lambada(i64 %b) {
  entry:
    %b1 = alloca i64, align 8
    store i64 %b, ptr %b1, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @unit to i64), i64 1)
    %b2 = load i64, ptr %b1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %b2)
    %Closure3 = call i64 @addInClosure(i64 ptrtoint (ptr @lambada0 to i64), i64 2)
    %Closure4 = call i64 @addInClosure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    %AppClosure5 = call i64 @appClosure(i64 %Closure3, i64 %Closure4)
    ret i64 %AppClosure5
  }
  
  define i64 @unit0(i64 %a) {
  entry:
    %a1 = alloca i64, align 8
    store i64 %a, ptr %a1, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    %a2 = load i64, ptr %a1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %a2)
    ret i64 %AppClosure
  }
  
  define i64 @foo(i64 %a) {
  entry:
    %a1 = alloca i64, align 8
    store i64 %a, ptr %a1, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @unit0 to i64), i64 1)
    %a2 = load i64, ptr %a1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %a2)
    %Closure3 = call i64 @addInClosure(i64 ptrtoint (ptr @lambada to i64), i64 1)
    ret i64 %Closure3
  }
  
  define i64 @unit1() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @foo to i64), i64 1)
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 4)
    %AppClosure1 = call i64 @appClosure(i64 %AppClosure, i64 8)
    %AppClosure2 = call i64 @appClosure(i64 %AppClosure1, i64 9)
    ret i64 %AppClosure2
  }
  
  define i64 @main() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @unit1 to i64), i64 0)
    ret i64 0
  }
  
  $ ./llvm_demo.exe < manytests/typed/007order.ml 
  declare i64 @print_int(i64)
  
  declare i64 @appClosure(i64, i64)
  
  declare i64 @addInClosure(i64, i64)
  
  define i64 @bin_op(i64 %a, i64 %b) {
  entry:
    %a1 = alloca i64, align 8
    store i64 %a, ptr %a1, align 4
    %b2 = alloca i64, align 8
    store i64 %b, ptr %b2, align 4
    %a3 = load i64, ptr %a1, align 4
    %b4 = load i64, ptr %b2, align 4
    %add_result = add i64 %a3, %b4
    ret i64 %add_result
  }
  
  define i64 @unit(i64 %a, i64 %b) {
  entry:
    %a1 = alloca i64, align 8
    store i64 %a, ptr %a1, align 4
    %b2 = alloca i64, align 8
    store i64 %b, ptr %b2, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    %Closure3 = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op to i64), i64 2)
    %a4 = load i64, ptr %a1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure3, i64 %a4)
    %b5 = load i64, ptr %b2, align 4
    %AppClosure6 = call i64 @appClosure(i64 %AppClosure, i64 %b5)
    %AppClosure7 = call i64 @appClosure(i64 %Closure, i64 %AppClosure6)
    ret i64 %AppClosure7
  }
  
  define i64 @unit0(i64 %__) {
  entry:
    %__1 = alloca i64, align 8
    store i64 %__, ptr %__1, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    %__2 = load i64, ptr %__1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %__2)
    ret i64 %AppClosure
  }
  
  define i64 @bin_op2(i64 %a, i64 %b) {
  entry:
    %a1 = alloca i64, align 8
    store i64 %a, ptr %a1, align 4
    %b2 = alloca i64, align 8
    store i64 %b, ptr %b2, align 4
    %a3 = load i64, ptr %a1, align 4
    %b4 = load i64, ptr %b2, align 4
    %mul_result = mul i64 %a3, %b4
    ret i64 %mul_result
  }
  
  define i64 @bin_op1(i64 %_c, i64 %a, i64 %b) {
  entry:
    %_c1 = alloca i64, align 8
    store i64 %_c, ptr %_c1, align 4
    %a2 = alloca i64, align 8
    store i64 %a, ptr %a2, align 4
    %b3 = alloca i64, align 8
    store i64 %b, ptr %b3, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op2 to i64), i64 2)
    %a4 = load i64, ptr %a2, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %a4)
    %b5 = load i64, ptr %b3, align 4
    %AppClosure6 = call i64 @appClosure(i64 %AppClosure, i64 %b5)
    %_c7 = load i64, ptr %_c1, align 4
    %div_result = sdiv i64 %AppClosure6, %_c7
    ret i64 %div_result
  }
  
  define i64 @bin_op0(i64 %_c, i64 %a, i64 %b, i64 %d) {
  entry:
    %_c1 = alloca i64, align 8
    store i64 %_c, ptr %_c1, align 4
    %a2 = alloca i64, align 8
    store i64 %a, ptr %a2, align 4
    %b3 = alloca i64, align 8
    store i64 %b, ptr %b3, align 4
    %d4 = alloca i64, align 8
    store i64 %d, ptr %d4, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op1 to i64), i64 3)
    %_c5 = load i64, ptr %_c1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %_c5)
    %a6 = load i64, ptr %a2, align 4
    %AppClosure7 = call i64 @appClosure(i64 %AppClosure, i64 %a6)
    %b8 = load i64, ptr %b3, align 4
    %AppClosure9 = call i64 @appClosure(i64 %AppClosure7, i64 %b8)
    %d10 = load i64, ptr %d4, align 4
    %add_result = add i64 %AppClosure9, %d10
    ret i64 %add_result
  }
  
  define i64 @_start(i64 %"()", i64 %"()1", i64 %a, i64 %"()2", i64 %b, i64 %_c, i64 %"()3", i64 %d, i64 %__) {
  entry:
    %"()4" = alloca i64, align 8
    store i64 %"()", ptr %"()4", align 4
    %"()5" = alloca i64, align 8
    store i64 %"()1", ptr %"()5", align 4
    %a6 = alloca i64, align 8
    store i64 %a, ptr %a6, align 4
    %"()7" = alloca i64, align 8
    store i64 %"()2", ptr %"()7", align 4
    %b8 = alloca i64, align 8
    store i64 %b, ptr %b8, align 4
    %_c9 = alloca i64, align 8
    store i64 %_c, ptr %_c9, align 4
    %"()10" = alloca i64, align 8
    store i64 %"()3", ptr %"()10", align 4
    %d11 = alloca i64, align 8
    store i64 %d, ptr %d11, align 4
    %__12 = alloca i64, align 8
    store i64 %__, ptr %__12, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @unit to i64), i64 2)
    %a13 = load i64, ptr %a6, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %a13)
    %b14 = load i64, ptr %b8, align 4
    %AppClosure15 = call i64 @appClosure(i64 %AppClosure, i64 %b14)
    %Closure16 = call i64 @addInClosure(i64 ptrtoint (ptr @unit0 to i64), i64 1)
    %__17 = load i64, ptr %__12, align 4
    %AppClosure18 = call i64 @appClosure(i64 %Closure16, i64 %__17)
    %Closure19 = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op0 to i64), i64 4)
    %_c20 = load i64, ptr %_c9, align 4
    %AppClosure21 = call i64 @appClosure(i64 %Closure19, i64 %_c20)
    %a22 = load i64, ptr %a6, align 4
    %AppClosure23 = call i64 @appClosure(i64 %AppClosure21, i64 %a22)
    %b24 = load i64, ptr %b8, align 4
    %AppClosure25 = call i64 @appClosure(i64 %AppClosure23, i64 %b24)
    %d26 = load i64, ptr %d11, align 4
    %AppClosure27 = call i64 @appClosure(i64 %AppClosure25, i64 %d26)
    ret i64 %AppClosure27
  }
  
  define i64 @app0() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 -1)
    ret i64 %AppClosure
  }
  
  define i64 @app1() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 4)
    ret i64 %AppClosure
  }
  
  define i64 @app2() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 2)
    ret i64 %AppClosure
  }
  
  define i64 @app3() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 1)
    ret i64 %AppClosure
  }
  
  define i64 @app() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @_start to i64), i64 9)
    %Closure1 = call i64 @addInClosure(i64 ptrtoint (ptr @app3 to i64), i64 0)
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %Closure1)
    %Closure2 = call i64 @addInClosure(i64 ptrtoint (ptr @app2 to i64), i64 0)
    %AppClosure3 = call i64 @appClosure(i64 %AppClosure, i64 %Closure2)
    %AppClosure4 = call i64 @appClosure(i64 %AppClosure3, i64 3)
    %Closure5 = call i64 @addInClosure(i64 ptrtoint (ptr @app1 to i64), i64 0)
    %AppClosure6 = call i64 @appClosure(i64 %AppClosure4, i64 %Closure5)
    %AppClosure7 = call i64 @appClosure(i64 %AppClosure6, i64 100)
    %AppClosure8 = call i64 @appClosure(i64 %AppClosure7, i64 1000)
    %Closure9 = call i64 @addInClosure(i64 ptrtoint (ptr @app0 to i64), i64 0)
    %AppClosure10 = call i64 @appClosure(i64 %AppClosure8, i64 %Closure9)
    %AppClosure11 = call i64 @appClosure(i64 %AppClosure10, i64 10000)
    %AppClosure12 = call i64 @appClosure(i64 %AppClosure11, i64 -555555)
    ret i64 %AppClosure12
  }
  
  define i64 @main() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    %Closure1 = call i64 @addInClosure(i64 ptrtoint (ptr @app to i64), i64 0)
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %Closure1)
    ret i64 0
  }
  
  $ ./llvm_demo.exe < manytests/typed/008ascription.ml 
  declare i64 @print_int(i64)
  
  declare i64 @appClosure(i64, i64)
  
  declare i64 @addInClosure(i64, i64)
  
  define i64 @app(i64 %g, i64 %x) {
  entry:
    %g1 = alloca i64, align 8
    store i64 %g, ptr %g1, align 4
    %x2 = alloca i64, align 8
    store i64 %x, ptr %x2, align 4
    %g3 = load i64, ptr %g1, align 4
    %x4 = load i64, ptr %x2, align 4
    %AppClosure = call i64 @appClosure(i64 %g3, i64 %x4)
    ret i64 %AppClosure
  }
  
  define i64 @addi(i64 %f, i64 %g, i64 %x) {
  entry:
    %f1 = alloca i64, align 8
    store i64 %f, ptr %f1, align 4
    %g2 = alloca i64, align 8
    store i64 %g, ptr %g2, align 4
    %x3 = alloca i64, align 8
    store i64 %x, ptr %x3, align 4
    %f4 = load i64, ptr %f1, align 4
    %x5 = load i64, ptr %x3, align 4
    %AppClosure = call i64 @appClosure(i64 %f4, i64 %x5)
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @app to i64), i64 2)
    %g6 = load i64, ptr %g2, align 4
    %AppClosure7 = call i64 @appClosure(i64 %Closure, i64 %g6)
    %x8 = load i64, ptr %x3, align 4
    %AppClosure9 = call i64 @appClosure(i64 %AppClosure7, i64 %x8)
    %AppClosure10 = call i64 @appClosure(i64 %AppClosure, i64 %AppClosure9)
    ret i64 %AppClosure10
  }
  
  define i64 @bin_op(i64 %x) {
  entry:
    %x1 = alloca i64, align 8
    store i64 %x, ptr %x1, align 4
    %x2 = load i64, ptr %x1, align 4
    %add_result = add i64 %x2, 1
    ret i64 %add_result
  }
  
  define i64 @bin_op0(i64 %x) {
  entry:
    %x1 = alloca i64, align 8
    store i64 %x, ptr %x1, align 4
    %x2 = load i64, ptr %x1, align 4
    %mul_result = mul i64 %x2, 2
    ret i64 %mul_result
  }
  
  define i64 @lambada(i64 %x, i64 %b) {
  entry:
    %x1 = alloca i64, align 8
    store i64 %x, ptr %x1, align 4
    %b2 = alloca i64, align 8
    store i64 %b, ptr %b2, align 4
    %b3 = load i64, ptr %b2, align 4
    %if = icmp ne i64 %b3, 0
    br i1 %if, label %then, label %else
  
  then:                                             ; preds = %entry
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op to i64), i64 1)
    %x4 = load i64, ptr %x1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %x4)
    br label %merge
  
  else:                                             ; preds = %entry
    %Closure5 = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op0 to i64), i64 1)
    %x6 = load i64, ptr %x1, align 4
    %AppClosure7 = call i64 @appClosure(i64 %Closure5, i64 %x6)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %iftmp = phi i64 [ %AppClosure, %then ], [ %AppClosure7, %else ]
    ret i64 %iftmp
  }
  
  define i64 @bin_op2(i64 %_start) {
  entry:
    %_start1 = alloca i64, align 8
    store i64 %_start, ptr %_start1, align 4
    %_start2 = load i64, ptr %_start1, align 4
    %div_result = sdiv i64 %_start2, 2
    ret i64 %div_result
  }
  
  define i64 @bin_op1(i64 %_start) {
  entry:
    %_start1 = alloca i64, align 8
    store i64 %_start, ptr %_start1, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op2 to i64), i64 1)
    %_start2 = load i64, ptr %_start1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %_start2)
    %eq_result = icmp eq i64 %AppClosure, 0
    %eq_result3 = zext i1 %eq_result to i64
    ret i64 %eq_result3
  }
  
  define i64 @lambada0(i64 %_start) {
  entry:
    %_start1 = alloca i64, align 8
    store i64 %_start, ptr %_start1, align 4
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @bin_op1 to i64), i64 1)
    %_start2 = load i64, ptr %_start1, align 4
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %_start2)
    ret i64 %AppClosure
  }
  
  define i64 @app0() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @addi to i64), i64 3)
    %Closure1 = call i64 @addInClosure(i64 ptrtoint (ptr @lambada to i64), i64 2)
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %Closure1)
    %Closure2 = call i64 @addInClosure(i64 ptrtoint (ptr @lambada0 to i64), i64 1)
    %AppClosure3 = call i64 @appClosure(i64 %AppClosure, i64 %Closure2)
    %AppClosure4 = call i64 @appClosure(i64 %AppClosure3, i64 4)
    ret i64 %AppClosure4
  }
  
  define i64 @unit() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    %Closure1 = call i64 @addInClosure(i64 ptrtoint (ptr @app0 to i64), i64 0)
    %AppClosure = call i64 @appClosure(i64 %Closure, i64 %Closure1)
    ret i64 %AppClosure
  }
  
  define i64 @main() {
  entry:
    %Closure = call i64 @addInClosure(i64 ptrtoint (ptr @unit to i64), i64 0)
    ret i64 0
  }
  
