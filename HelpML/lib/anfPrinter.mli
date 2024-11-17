open RestrictedAst
open Format
open Ast

val pp_immexpr : formatter -> immexpr -> unit

val pp_binop : formatter -> bin_op -> unit

val pp_cexpr : formatter -> cexpr -> unit

val pp_pexpr : formatter -> pexpr -> unit

val pp_aexpr : formatter -> aexpr -> unit

val pp_list
  :  formatter
  -> (formatter -> 'a -> unit)
  -> (unit, Format.formatter, unit) format
  -> 'a list
  -> unit

val pp_bexpr : formatter -> bexpr -> unit

val pp_prexpr : formatter -> bexpr list -> unit