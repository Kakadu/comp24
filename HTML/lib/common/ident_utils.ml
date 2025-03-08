open AstLib.Ast

let ident_to_string id =
  match id with
  | IdentOfDefinable (IdentLetters s) -> s
  | IdentOfDefinable (IdentOp s) -> s
  | IdentOfBaseOp base_op ->
    (match base_op with
     | Plus -> "base_plus"
     | Minus -> "base_minus")
;;
