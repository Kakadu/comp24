open Llvm
open Flambda
let context = global_context()
let mdl = create_module context "main"
let builder = Llvm.builder context

let named_values : (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 16

let int_type = Llvm.i16_type context



