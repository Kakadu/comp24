open Ast

type id = string

type imm_expr =
  | ImmInt of int
  | ImmBool of bool
  | ImmId of id

type cexpr =
  | CApplication of imm_expr * imm_expr
  | CIfElse of imm_expr * aexpr * aexpr
  | CImmExpr of imm_expr

and aexpr =
  | ALet of id * cexpr * aexpr
  | ACExpr of cexpr

type functions = id * id list * aexpr
