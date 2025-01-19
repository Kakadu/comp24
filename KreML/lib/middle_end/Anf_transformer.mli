open Ast

type immediate = Avar of ident | Aconst of const

type cexpr =
  | CImm of immediate
  | CTuple of immediate list
  | CNil
  | CCons of immediate * immediate
  | CFun of ident * cexpr
  | CApp of immediate * immediate
  | CIte of immediate * aexpr * aexpr
  | CConstrained of cexpr * typ
  | CSwitch (* TODO rewrite pattern matching *)



and aexpr =
    | ALet of rec_flag * ident * cexpr * aexpr
    | AExpr of cexpr

type astructure = (ident * aexpr) list