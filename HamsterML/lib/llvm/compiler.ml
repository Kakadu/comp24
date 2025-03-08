open Llvm
open Anf

(* LLVM setup *)
let global_context = global_context ()
let builder = builder global_context
let the_module = create_module global_context "HamsterML_LLVM_Compiler"
let i64 = i16_type global_context
let lookup_function_exception id llmodule = Option.get @@ lookup_function id llmodule

(* TODO: codegen functions *)
let codegen_immexpr = function
  | ImmInt i -> const_int i64 i
  | ImmBool b -> const_int i64 (Base.Bool.to_int b)
  | _ -> failwith "not yet implemented"
;;

(* let codegen_immexpr = failwith "not yet implemented"
   let codegen_cexpr = failwith "not yet implemented"
   let codegen_aexpr = failwith "not yet implemented"
   let codegen_global_scope_function = failwith "not yet implemented"
   let codegen = failwith "not yet implemented" *)
