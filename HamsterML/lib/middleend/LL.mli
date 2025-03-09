type ll_expr =
  | LLConst of Ast.value
  | LLVar of string
  | LLOperation of Ast.op
  | LLTuple of ll_expr * ll_expr * ll_expr list
  | LLList of ll_expr list
  | LLListConcat of ll_expr * ll_expr
  | LLConstraint of ll_expr * Ast.dataType
  | LLApplication of ll_expr * ll_expr
  | LLLet of Ast.funType * ll_bind list * ll_expr option
  | LLIf of ll_expr * ll_expr * ll_expr option
  | LLMatch of ll_expr * ll_case list

and ll_bind = Ast.pattern * Ast.args * ll_expr
and ll_case = Ast.pattern * ll_expr
and ll_prog = ll_expr list

(* Apply lambda lifting *)
val ll_prog : Ast.prog -> ll_prog Utils.R.t
