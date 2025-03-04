open HamsterML__Ast
open Base

let rec pretty_print_expr e =
  match e with
  | EConst v -> pretty_print_value v
  | EVar id -> id
  | EOperation op -> pretty_print_op op
  | ETuple (e1, e2, rest) ->
    "("
    ^ pretty_print_expr e1
    ^ ", "
    ^ pretty_print_expr e2
    ^ (if List.is_empty rest
       then ""
       else ", " ^ String.concat ~sep:", " (List.map ~f:pretty_print_expr rest))
    ^ ")"
  | EList exprs ->
    "[" ^ String.concat ~sep:"; " (List.map ~f:pretty_print_expr exprs) ^ "]"
  | EListConcat (e1, e2) -> pretty_print_expr e1 ^ "::" ^ pretty_print_expr e2
  | EConstraint (e, dt) -> pretty_print_expr e ^ " : " ^ pretty_print_datatype dt
  | Application (e1, e2) -> "(" ^ pretty_print_expr e1 ^ " " ^ pretty_print_expr e2 ^ ")"
  | Let (ft, binds, body_opt) -> pretty_print_let ft binds body_opt
  | Fun (args, body) ->
    "(fun "
    ^ String.concat ~sep:" " (List.map ~f:pretty_print_pattern args)
    ^ " -> "
    ^ pretty_print_expr body
    ^ ")"
  | If (cond, then_expr, else_opt) ->
    "if "
    ^ pretty_print_expr cond
    ^ " then "
    ^ pretty_print_expr then_expr
    ^
      (match else_opt with
      | Some e -> " else " ^ pretty_print_expr e
      | None -> "")
  | Match (e, cases) ->
    "match "
    ^ pretty_print_expr e
    ^ " with"
    ^ String.concat
        ~sep:""
        (List.map cases ~f:(fun (pat, expr) ->
           "\n| " ^ pretty_print_pattern pat ^ " -> " ^ pretty_print_expr expr))

and pretty_print_value = function
  | Int i -> Int.to_string i
  | Bool b -> Bool.to_string b
  | String s -> "\"" ^ s ^ "\""
  | Unit -> "()"

and pretty_print_op = function
  | Binary bop -> pretty_print_bop bop
  | Unary uop -> pretty_print_uop uop

and pretty_print_bop = function
  | ADD -> "( + )"
  | SUB -> "( - )"
  | MUL -> "( * )"
  | DIV -> "( / )"
  | EQ -> "="
  | ID_EQ -> "=="
  | NEQ -> "!="
  | GT -> ">"
  | GTE -> ">="
  | LT -> "<"
  | LTE -> "<="
  | AND -> "&&"
  | OR -> "||"
  | CONCAT -> "^"

and pretty_print_uop = function
  | NOT -> "not"
  | UMINUS -> "-"
  | UPLUS -> "+"

and pretty_print_datatype = function
  | PInt -> "Int"
  | PBool -> "Bool"
  | PString -> "String"

and pretty_print_pattern = function
  | Const v -> pretty_print_value v
  | Var id -> id
  | Wildcard -> "_"
  | Tuple (p1, p2, rest) ->
    "("
    ^ pretty_print_pattern p1
    ^ ", "
    ^ pretty_print_pattern p2
    ^ (if List.is_empty rest
       then ""
       else ", " ^ String.concat ~sep:", " (List.map ~f:pretty_print_pattern rest))
    ^ ")"
  | List pats ->
    "[" ^ String.concat ~sep:"; " (List.map ~f:pretty_print_pattern pats) ^ "]"
  | ListConcat (p1, p2) -> pretty_print_pattern p1 ^ "::" ^ pretty_print_pattern p2
  | Constraint (p, dt) -> pretty_print_pattern p ^ " : " ^ pretty_print_datatype dt
  | Operation op -> "(" ^ pretty_print_op op ^ ")"

and pretty_print_let ft binds body_opt =
  let rec_flag =
    match ft with
    | Recursive -> "rec "
    | Nonrecursive -> ""
  in
  let bindings =
    String.concat
      ~sep:" and "
      (List.map binds ~f:(fun (pat, args, expr) ->
         pretty_print_pattern pat
         ^ (if List.is_empty args
            then ""
            else " " ^ String.concat ~sep:" " (List.map ~f:pretty_print_pattern args))
         ^ " = "
         ^ pretty_print_expr expr))
  in
  "let "
  ^ rec_flag
  ^ bindings
  ^
  match body_opt with
  | Some e -> " in " ^ pretty_print_expr e
  | None -> ""
;;
