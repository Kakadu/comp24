open Llvm

val block_terminates_with_unreachable : llbasicblock -> bool
val cast_firstclass_value : lltype -> llvalue -> lltype -> lltype -> llbuilder -> llvalue