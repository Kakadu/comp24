open Middleend.Anf_ast
open My_llvm
open Llvm

let () =
  let compiled4 =
    Codegen.compile_program
      ~verbose:true
      [ AbsStr_func
          ( "abs_value"
          , [ "n" ]
          , LComplex
              (CExp_apply ("__op_plus", [ AExp_constant (Const_int 52); AExp_ident "n" ]))
          )

      ]
  in
  ignore compiled4
;;
