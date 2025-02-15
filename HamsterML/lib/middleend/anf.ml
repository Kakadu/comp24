open Ast

type imm_expr =
  | ImmInt of int
  | ImmString of string
  | ImmBool of bool
  | ImmId of id
  | ImmList of imm_expr list
  | ImmTuple of imm_expr list
  | ImmUnit

type cexpr =
  | CApplication of cexpr * cexpr
  | CIf of imm_expr * aexpr * aexpr
  | CConstructList of imm_expr * imm_expr
  | CMatch of imm_expr * (pattern * aexpr) list
  | CImm of imm_expr

and aexpr =
  | ALetIn of pattern * cexpr * aexpr
  | ACExpr of cexpr

type single_anf_binding = ALet of pattern * pattern list * aexpr

type anf_decl =
  | ADSingleLet of funType * single_anf_binding
  | ADMutualRecDecl of funType * single_anf_binding

type anf_prog = anf_decl list
