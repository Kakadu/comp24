open Ast

val pp_typ : Format.formatter -> typ -> unit
val pp_expr : Format.formatter -> expr -> unit
val pp_structure : Format.formatter -> structure -> unit
val pattern_to_code : pattern -> string
val expr_to_code : expr -> string
val structure_to_code : structure -> string
