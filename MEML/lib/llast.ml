open Ast

type llexpression =
  | LLVars of llexpression * llexpression
  | LLConst of const
  | LLVar of name
  | LLIfElse of llexpression * llexpression * llexpression
  | LLEbinOp of binary_op * llexpression * llexpression
  | LLPatLetIn of pattern * llexpression * llexpression
  | LLApp of llexpression * llexpression
  | LLTuple of llexpression list
  | LLMatch of llexpression * (pattern * llexpression) list
  | LLList of llexpression * llexpression
[@@deriving show { with_path = false }]

type llbindings =
  | LLLet of (rec_flag * name * pattern list * llexpression) (** let id = expr *)
  | LLLetPat of (pattern * llexpression) (** let id = expr *)
  | LLExpression of llexpression (** simple expressions *)
[@@deriving show { with_path = false }]

type llstatements = llbindings list [@@deriving show { with_path = false }]
