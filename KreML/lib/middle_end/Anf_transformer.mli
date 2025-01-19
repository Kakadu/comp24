open Ast

type immediate = Avar of ident | Aconst of const

type cexpr =
  | CImm of immediate
  | CTuple of immediate list
  | CCons of immediate * immediate
  | CFun of ident * aexpr
  | CApp of immediate * immediate list
  | CIte of immediate * aexpr * aexpr


and aexpr =
    | ALet of rec_flag * ident * cexpr * aexpr
    | AExpr of cexpr

type astructure_item = AStr_value of rec_flag * (ident * aexpr) list
type astructure = astructure_item list

val transform : structure -> astructure