type me_expr =
  | MEConst of Ast.value
  | MEVar of string
  | MEOperation of Ast.op
  | METuple of me_expr * me_expr * me_expr list
  | MEList of me_expr list
  | MEListConcat of me_expr * me_expr
  | MEConstraint of me_expr * Ast.dataType
  | MEApplication of me_expr * me_expr
  | MELet of Ast.funType * me_bind list * me_expr option
  | MEIf of me_expr * me_expr * me_expr option

and me_bind = Ast.pattern * Ast.args * me_expr
and me_prog = me_expr list

(* Apply match elimination *)
val convert_prog : LL.ll_prog -> me_prog
