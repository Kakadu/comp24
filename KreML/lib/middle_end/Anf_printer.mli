open Anf
open Ast

val pp_const : Format.formatter -> const -> unit 
val binop_to_string : binop -> string
val pp : Format.formatter -> astructure -> unit