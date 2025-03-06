open Ast

type cexpression =
  | CConst of const
  | CVar of name
  | CIfElse of cexpression * cexpression * cexpression
  | CEbinOp of binary_op * cexpression * cexpression
  | CApp of cexpression * cexpression
  | CLetIn of rec_flag * name * pattern list * cexpression * cexpression
  | CPatLetIn of pattern * cexpression * cexpression
  | CTuple of cexpression list
  | CMatch of cexpression * (pattern * cexpression) list
  | CList of cexpression * cexpression
[@@deriving show { with_path = false }]

type cbindings =
  | CLets of rec_flag * clets list
  | CExpression of cexpression (** simple expressions *)

and clets =
  | CLet of (name * pattern list * cexpression) (** let id = expr *)
  | CLetPat of (pattern * cexpression) (** let id = expr *)
[@@deriving show { with_path = false }]

type cstatements = cbindings list [@@deriving show { with_path = false }]
