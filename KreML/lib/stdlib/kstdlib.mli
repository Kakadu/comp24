open Ast

val print_int : ident

val is_stdlib_fun : ident -> bool

val stdlib_funs : ident list

type stdlib_fun = ident * int (* name, arity *)
val stdlib_funs_with_arity : stdlib_fun list