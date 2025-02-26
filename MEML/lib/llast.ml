open Ast

type llexpression =
  | LLVars of llexpression * llexpression
  | LLConst of const
  | LLVar of name
  | LLIfElse of llexpression * llexpression * llexpression
  | LLEbinOp of binary_op * llexpression * llexpression
  | LLLetIn of name list * pattern list * llexpression * llexpression
  | LLApp of llexpression * llexpression
  | LLTuple of llexpression list
  | LLMatch of llexpression * (pattern * llexpression) list
  | LLList of llexpression * llexpression
[@@deriving show { with_path = false }]

type llbindings =
  | LLLet of rec_flag * name list * pattern list * llexpression
  | LLExpression of llexpression
[@@deriving show { with_path = false }]

type llstatements = llbindings list [@@deriving show { with_path = false }]
