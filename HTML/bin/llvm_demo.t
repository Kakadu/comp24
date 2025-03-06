  $ clang-16 -c runtime.c -o runtime.o

  $ ./test_llvm.sh manytests/typed/001fac.ml
  llvm_demo.exe
  manytests
  out.ll
  runtime.c
  runtime.o
  test_llvm.sh
  target triple = "x86_64-pc-linux-gnu"
  
  declare i64 @print_int(i64)
  
  declare i64 @print_newline(i64)
  
  declare i64 @"base +"(i64)
  
  declare i64 @"base -"(i64)
  
  declare i64 @"+"(i64, i64)
  
  declare i64 @-(i64, i64)
  
  declare i64 @"*"(i64, i64)
  
  declare i64 @"/"(i64, i64)
  
  declare i64 @"<="(i64, i64)
  
  declare i64 @"<"(i64, i64)
  
  declare i64 @">="(i64, i64)
  
  declare i64 @">"(i64, i64)
  
  declare i64 @"="(i64, i64)
  
  declare i64 @"=="(i64, i64)
  
  declare i64 @"!="(i64, i64)
  
  declare i64 @"&&"(i64, i64)
  
  declare i64 @"||"(i64, i64)
  
  declare i64 @RTE_ERROR_MATCH_FAILURE(i64)
  
  declare i64 @"( = )"(i64, i64)
  
  declare i64 @"( != )"(i64, i64)
  
  declare i64 @"( && )"(i64, i64)
  
  define i64 @a.1() {
  entry:
    ret i64 3
  }
