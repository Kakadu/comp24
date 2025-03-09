open Middleend.Anf_ast
open My_llvm
open Llvm

let () =
  let compiled1 =
    Codegen.compile_aexp
      (AExp_tuple
         [ AExp_constant (Const_int 42)
         ; AExp_constant (Const_char 'c')
         ; AExp_constant (Const_bool true)
         ; AExp_constant (Const_string "BDSM")
         ; AExp_constant Const_unit
         ])
  in
  let compiled2 =
    Codegen.compile_aexp (AExp_construct ("Some", Some (AExp_constant (Const_int 1))))
  in
  let compiled3 =
    Codegen.compile_aexp
      (AExp_construct
         ( "::"
         , Some
             (AExp_tuple
                [ AExp_constant (Const_int 1)
                ; AExp_construct
                    ( "::"
                    , Some
                        (AExp_tuple
                           [ AExp_constant (Const_int 2); AExp_construct ("[]", None) ])
                    )
                ]) ))
  in
  dump_value compiled1;
  dump_value compiled2;
  dump_value compiled3;
  dump_module Llvm_init.my_module
;;
