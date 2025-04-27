  $ ./compiler.exe < manytests/typed/001fac.ml
  $  cat < out.ll
  ; ModuleID = 'FML'
  source_filename = "FML"
  target triple = "x86_64-pc-linux-gnu"
  
  declare i64 @new_closure(i64, i64)
  
  declare i64 @apply_args(i64, i64, i64, ...)
  
  declare i64 @print_int(i64)
  
  declare i64 @rt_add(i64, i64)
  
  declare i64 @rt_sub(i64, i64)
  
  declare i64 @rt_mul(i64, i64)
  
  declare i64 @rt_div(i64, i64)
  
  declare i64 @rt_leq(i64, i64)
  
  declare i64 @rt_less(i64, i64)
  
  declare i64 @rt_geq(i64, i64)
  
  declare i64 @rt_gre(i64, i64)
  
  declare i64 @rt_eq(i64, i64)
  
  declare i64 @rt_neq(i64, i64)
  
  declare i64 @rt_and(i64, i64)
  
  declare i64 @rt_or(i64, i64)
  
  declare i64 @fail_match(i64)
  
  define i64 @fac(i64 %n) {
  entry:
    %sle = icmp sle i64 %n, 1
    %sle_i64t = zext i1 %sle to i64
    %cond_v = icmp ne i64 %sle_i64t, 0
    br i1 %cond_v, label %then, label %else
  
  then:                                             ; preds = %entry
    br label %merge
  
  else:                                             ; preds = %entry
    %sub = sub i64 %n, 1
    %call = call i64 @fac(i64 %sub)
    %mul = mul i64 %n, %call
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %phi = phi i64 [ 1, %then ], [ %mul, %else ]
    ret i64 %phi
  }
  
  define i64 @main() {
  entry:
    %call = call i64 @fac(i64 4)
    %call1 = call i64 @print_int(i64 %call)
    ret i64 0
  }
 
  $ ./compiler.exe < manytests/typed/002fac.ml
  Fatal error: exception Invalid_argument("option is None")
  Raised at Stdlib.invalid_arg in file "stdlib.ml" (inlined), line 30, characters 20-45
  Called from Stdlib__Option.get in file "option.ml", line 21, characters 41-69
  Called from Fml_lib__Gen.compile_immexpr in file "lib/llvm/gen.ml", line 48, characters 9-65
  Called from Fml_lib__Gen.compile_cexpr in file "lib/llvm/gen.ml", line 108, characters 15-51
  Called from Fml_lib__Gen.compile_aexpr in file "lib/llvm/gen.ml", line 146, characters 12-31
  Called from Fml_lib__Gen.compile_cexpr in file "lib/llvm/gen.ml", line 128, characters 16-36
  Called from Fml_lib__Gen.compile_anf_binding in file "lib/llvm/gen.ml", line 167, characters 17-35
  Called from Fml_lib__Gen.compile_anf_decl.(fun) in file "lib/llvm/gen.ml", line 177, characters 37-66
  Called from Stdlib__List.iter in file "list.ml", line 110, characters 12-15
  Called from Fml_lib__Gen.create_main.(fun) in file "lib/llvm/gen.ml", line 209, characters 32-55
  Called from Stdlib__List.iter in file "list.ml", line 110, characters 12-15
  Called from Fml_lib__Gen.compile_program in file "lib/llvm/gen.ml", line 213, characters 10-29
  Called from Dune__exe__Compiler in file "bin/compiler.ml", line 24, characters 4-31
  [2]
  $  cat < out.ll
  ; ModuleID = 'FML'
  source_filename = "FML"
  target triple = "x86_64-pc-linux-gnu"
  
  declare i64 @new_closure(i64, i64)
  
  declare i64 @apply_args(i64, i64, i64, ...)
  
  declare i64 @print_int(i64)
  
  declare i64 @rt_add(i64, i64)
  
  declare i64 @rt_sub(i64, i64)
  
  declare i64 @rt_mul(i64, i64)
  
  declare i64 @rt_div(i64, i64)
  
  declare i64 @rt_leq(i64, i64)
  
  declare i64 @rt_less(i64, i64)
  
  declare i64 @rt_geq(i64, i64)
  
  declare i64 @rt_gre(i64, i64)
  
  declare i64 @rt_eq(i64, i64)
  
  declare i64 @rt_neq(i64, i64)
  
  declare i64 @rt_and(i64, i64)
  
  declare i64 @rt_or(i64, i64)
  
  declare i64 @fail_match(i64)
  
  define i64 @fac(i64 %n) {
  entry:
    %sle = icmp sle i64 %n, 1
    %sle_i64t = zext i1 %sle to i64
    %cond_v = icmp ne i64 %sle_i64t, 0
    br i1 %cond_v, label %then, label %else
  
  then:                                             ; preds = %entry
    br label %merge
  
  else:                                             ; preds = %entry
    %sub = sub i64 %n, 1
    %call = call i64 @fac(i64 %sub)
    %mul = mul i64 %n, %call
    br label %merge
  
  merge:                                            ; preds = %else, %then
    %phi = phi i64 [ 1, %then ], [ %mul, %else ]
    ret i64 %phi
  }
  
  define i64 @main() {
  entry:
    %call = call i64 @fac(i64 4)
    %call1 = call i64 @print_int(i64 %call)
    ret i64 0
  }
