open Ast

type id = string

type imm_expr =
  | ImmInt of int
  | ImmBool of bool
  | ImmId of id
  | ImmTuple of imm_expr list
  | ImmList of imm_expr list
[@@deriving show { with_path = false }]

type cexpr =
  | CApp of imm_expr * imm_expr
  | CIfElse of imm_expr * aexpr * aexpr
  | CImmExpr of imm_expr
[@@deriving show { with_path = false }]

and aexpr =
  | ALet of id * cexpr * aexpr
  | ACExpr of cexpr
[@@deriving show { with_path = false }]

type fn = Fn of id * id list * aexpr [@@deriving show { with_path = false }]
type program = fn list [@@deriving show { with_path = false }]

