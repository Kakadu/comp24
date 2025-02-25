open Ast

type cexpression =
  | CConst of const
  | CVar of name
  | CIfElse of cexpression * cexpression * cexpression
  | CEbinOp of binary_op * cexpression * cexpression
  | CApp of cexpression * cexpression
  | CLetIn of rec_flag * name list * pattern list * cexpression * cexpression
  | CTuple of cexpression list
  | CMatch of cexpression * (pattern * cexpression) list
  | CList of cexpression * cexpression
[@@deriving show { with_path = false }]

type cbindings =
  | CLet of (rec_flag * name list * pattern list * cexpression) list
  | CExpression of cexpression
[@@deriving show { with_path = false }]

type cstatements = cbindings list [@@deriving show { with_path = false }]
