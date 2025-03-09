open Ast

(* Immediate Expressions, don't require further computation *)
type imm_expr =
  | ImmInt of int
  | ImmString of string
  | ImmBool of bool
  | ImmId of id
  | ImmList of imm_expr list
  | ImmTuple of imm_expr list
  | ImmUnit
  | ImmConstraint of imm_expr * dataType

(* Computed Expressions *)
type cexpr =
  | COperation of op
  | CApplication of cexpr * cexpr
  | CIf of imm_expr * aexpr * aexpr
  | CConstructList of imm_expr * imm_expr
  | CImm of imm_expr

(* Atomic Expressions *)
and aexpr =
  | ALetIn of pattern * cexpr * aexpr
  | ACExpr of cexpr

(* Let binding in ANF form *)
type single_anf_binding = ALet of pattern * id list * aexpr

type anf_decl =
  | ADSingleLet of funType * single_anf_binding
  | ADMutualRecDecl of single_anf_binding list

type anf_prog = anf_decl list
