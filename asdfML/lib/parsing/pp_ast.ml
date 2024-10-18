open Ast
open Format

let rec pp_const fmt = function
  | CInt i -> fprintf fmt "%d" i
  | CBool b -> fprintf fmt "%b" b
  | CUnit -> fprintf fmt "()"

and pp_unary_operator fmt = function
  | Neg -> fprintf fmt "-"
  | Not -> fprintf fmt "not "

and pp_binary_operator fmt = function
  | Add -> fprintf fmt "+"
  | Sub -> fprintf fmt "-"
  | Mul -> fprintf fmt "*"
  | Div -> fprintf fmt "/"
  | Eq -> fprintf fmt "=="
  | Ne -> fprintf fmt "!="
  | Gt -> fprintf fmt ">"
  | Lt -> fprintf fmt "<"
  | Ge -> fprintf fmt ">="
  | Le -> fprintf fmt "<="
  | And -> fprintf fmt "&&"
  | Or -> fprintf fmt "||"

and pp_type_ann fmt = function 
  | TInt -> fprintf fmt "int"
  | TBool -> fprintf fmt "bool"
  | TUnit -> fprintf fmt "()"
  | TFun(a,b) ->  fprintf fmt "%a -> %a" pp_type_ann a pp_type_ann b

and pp_pattern fmt = function
  | PConst(c) ->  fprintf fmt "%a" pp_const c
  | PWild -> fprintf fmt "_"
  | PIdent (id, None) -> fprintf fmt "%s" id
  | PIdent (id, Some(ty)) -> fprintf fmt "(%s:%a)" id pp_type_ann ty

and pp_expr fmt = function
  | EConst c -> fprintf fmt "%a" pp_const c
  | EVar v -> fprintf fmt "%s" v
  | EUnaryOp (op, e) -> fprintf fmt "%a%a" pp_unary_operator op pp_expr e
  | EBinaryOp (op, e1, e2) ->
    fprintf fmt "%a %a %a" pp_expr e1 pp_binary_operator op pp_expr e2
  | EApp (e1, e2) -> fprintf fmt "(%a %a)" pp_expr e1 pp_expr e2
  | EIfElse (c, t, e) -> fprintf fmt "if %a then %a else %a" pp_expr c pp_expr t pp_expr e
  | EFun (p, e) -> fprintf fmt "fun %a -> %a" pp_pattern p pp_expr e
  | ELetIn (d, e) -> fprintf fmt "%a in %a" pp_definition d pp_expr e

and pp_definition fmt = function
  | DLet (NonRec, pat, e) -> fprintf fmt "let %a = %a" pp_pattern pat pp_expr e
  | DLet (Rec, pat, e) -> fprintf fmt "let rec %a = %a" pp_pattern pat pp_expr e
;;
