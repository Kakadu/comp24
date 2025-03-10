open Middleend.Anf_ast
open My_llvm
open Llvm

let () =
  let compiled4 =
    Codegen.compile_program
      ~verbose:true
      [ AbsStr_value
          ( "x"
          , LComplex
              (CExp_atom
                 (AExp_tuple
                    [ AExp_constant (Const_int 42)
                    ; AExp_constant (Const_char 'c')
                    ; AExp_constant (Const_bool true)
                    ; AExp_constant (Const_string "BDSM")
                    ; AExp_constant Const_unit
                    ])) )
      ]
  in
  ignore compiled4
;;
