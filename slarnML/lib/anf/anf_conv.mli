open Ll_ast
open Anf_ast

val anf : gl_expr list -> afun list

val clear_free : 'a -> int
