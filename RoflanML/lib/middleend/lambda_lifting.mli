open Ast
open Ll_ast

val lift_program : decl list -> (lldecl list, string) result
