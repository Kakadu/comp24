open Llvm

(* LLVM setup *)
let context = global_context ()
let builder = builder context
let the_module = create_module context "HamsterML_LLVM_Compiler"
let i64 = i16_type context
let lookup_function_exception id llmodule = Option.get @@ lookup_function id llmodule

(* TODO: codegen functions *)

(* let codegen_immexpr = failwith "not yet implemented"
   let codegen_cexpr = failwith "not yet implemented"
   let codegen_aexpr = failwith "not yet implemented"
   let codegen_global_scope_function = failwith "not yet implemented"
   let codegen = failwith "not yet implemented" *)
