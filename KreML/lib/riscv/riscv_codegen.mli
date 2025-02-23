open Flambda
open Riscv

val codegen_program : flstructure -> instruction list

val dump : instruction list -> unit