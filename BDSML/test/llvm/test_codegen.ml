open Middleend.Anf_ast
open My_llvm
open Llvm

let () =
  (* let compiled1 =
     Codegen.compile_aexpr
     (AExp_tuple
     [ AExp_constant (Const_int 42)
         ; AExp_constant (Const_char 'c')
         ; AExp_constant (Const_bool true)
         ; AExp_constant (Const_string "BDSM")
         ; AExp_constant Const_unit
         ])
     in
     let compiled2 =
     Codegen.compile_aexpr (AExp_construct ("Some", Some (AExp_constant (Const_int 1))))
     in
     let compiled3 =
     Codegen.compile_aexpr
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
     in *)
  let compiled4 =
    Codegen.compile_lexpr
      (LLet_in
         ( "x"
         , CExp_atom (AExp_constant (Const_int 42))
         , LComplex (CExp_atom (AExp_ident "x")) ))
  in
  dump_value compiled4;
  dump_module Llvm_init.my_module
;;
