open Ast

(** This file contains runtime functions names *)
val access_closure : ident
val access_tuple : ident

val list_cons : ident
val list_head : ident
val list_tail : ident

val runtime_funs: ident list
val is_runtime_fun : ident -> bool

val stdlib_funs : ident list
val is_stdlib_fun : ident -> bool