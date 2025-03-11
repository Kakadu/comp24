open Ll_ast
open Anf_ast

val anf_program : lldecl list -> (adecl list, string) result
