(** Copyright 2025, tepa46 *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Ast
open Angstrom

let is_keyword = function
  | "and"
  | "else"
  | "false"
  | "fun"
  | "function"
  | "if"
  | "in"
  | "let"
  | "match"
  | "rec"
  | "then"
  | "true"
  | "with" -> true
  | _ -> false
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_wildcard = function
  | '_' -> true
  | _ -> false
;;

let is_lchar = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_uchar = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_char ch = is_lchar ch || is_uchar ch

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_core_operator_char = function
  | '$' | '&' | '*' | '+' | '-' | '/' | '=' | '>' | '@' | '^' | '|' -> true
  | _ -> false
;;

let is_operator_char = function
  | '~' | '!' | '?' | '%' | '<' | ':' | '.' -> true
  | c -> is_core_operator_char c
;;

let operator_name oper_chars = "( " ^ oper_chars ^ " )"
let unary_operator_name oper_chars = operator_name ("~" ^ oper_chars)
let spaces = take_while is_space
let spaces1 = take_while1 is_space
let pchunk p = spaces *> p
let check_chunk s = pchunk @@ string s

let check_token s =
  let* token = spaces *> take_while (fun c -> is_char c || is_digit c || is_wildcard c) in
  if s = token then return s else fail "invalid token name"
;;

let pparens p = check_chunk "(" *> p <* check_chunk ")"

(****************************Identifier*parser**************************)

let parse_var_without_parens pv =
  pchunk
    (take_while (fun ch -> is_uchar ch || is_digit ch)
     >>= function
     | "" ->
       take_while1 (fun ch -> is_wildcard ch || is_char ch || is_digit ch)
       >>= fun str ->
       if is_keyword str then fail "invalid identifier name" else return (Id str)
     | _ -> fail "invalid identifier name")
  <|> pv
;;

let parse_var =
  fix
  @@ fun parse_var ->
  let pv = pparens parse_var in
  let pv = parse_var_without_parens pv in
  pv
;;

let parse_operator_without_parens =
  pchunk (take_while1 is_operator_char >>= fun str -> return (Id (operator_name str)))
;;

let parse_operator_with_parens po = pparens @@ parse_operator_without_parens <|> po

let parse_operator =
  fix
  @@ fun parse_operator ->
  let po = pparens parse_operator in
  let po = parse_operator_with_parens po in
  po
;;

let parse_id = parse_var <|> parse_operator

(****************************Constant*parser**************************)

let parse_string_constant =
  check_chunk "\""
  *> (take_while (fun ch -> ch != '\"') >>= fun str -> return (CString str))
  <* check_chunk "\""
;;

let parse_number_constant =
  int_of_string <$> spaces *> take_while1 is_digit >>= fun num -> return (CInt num)
;;

let parse_bool_constant =
  check_token "true" *> (return @@ CBool true)
  <|> check_token "false" *> (return @@ CBool false)
;;

let parse_empty_list_constant = check_chunk "[]" *> return CEmptyList
let parse_unit_constant = check_chunk "()" *> return CUnit

let parse_constant =
  parse_string_constant
  <|> parse_number_constant
  <|> parse_bool_constant
  <|> parse_empty_list_constant
  <|> parse_unit_constant
;;

(****************************Types*parser**************************)

let parse_type_var = check_chunk "\'" *> parse_id >>= fun id -> return (TVar id)

let parse_tuple_type pt =
  sep_by1 (check_chunk "*") pt
  >>= function
  | [] -> pt
  | [ h ] -> return h
  | h :: tl -> return (TTuple (h :: tl))
;;

let parse_list_type pt =
  pt <* check_token "list" >>= fun lst_type -> return (TList lst_type)
;;

let parse_arrow_type pt =
  chainr1 pt (check_chunk "->" *> return (fun ty1 ty2 -> TArrow (ty1, ty2)))
;;

let parse_type =
  fix
  @@ fun parse_type ->
  let pt = pparens parse_type <|> parse_type_var in
  let pt = check_token "int" *> return TInt <|> pt in
  let pt = check_token "string" *> return TString <|> pt in
  let pt = check_token "bool" *> return TBool <|> pt in
  let pt = check_token "unit" *> return TUnit <|> pt in
  let pt = parse_list_type pt <|> pt in
  let pt = parse_tuple_type pt <|> pt in
  let pt = parse_arrow_type pt <|> pt in
  pt
;;

(**************************Pattern*parser************************)

let fcons = check_chunk "::" *> return (fun ptr1 ptr2 -> PCons (ptr1, ptr2))
let parse_cons pp = chainr1 pp fcons
let parse_any = check_token "_" *> return PAny

let parse_tuple pp =
  fix
  @@ fun parse_tuple ->
  pparens @@ sep_by1 (check_chunk ",") (pp <|> parse_tuple)
  >>= function
  | [] -> pp
  | [ h ] -> return h
  | h :: tl -> return (PTuple (h :: tl))
;;

let ptype a b = PType (a, b)
let parse_typed_pattern pp = pparens @@ lift2 ptype pp (check_chunk ":" *> parse_type)

let parse_pattern =
  fix
  @@ fun parse_pattern ->
  let pp = pparens parse_pattern <|> parse_typed_pattern parse_pattern in
  let pp = parse_constant >>= (fun const -> return (PConst const)) <|> pp in
  let pp = parse_id >>= (fun ident -> return (PVar ident)) <|> pp in
  let pp = parse_any <|> pp in
  let pp = parse_tuple pp <|> pp in
  let pp = parse_cons pp <|> pp in
  pp
;;

(**************************Expr*parser************************)

let bin_operation op =
  check_chunk op
  *> return (fun exp1 exp2 -> EApp (EApp (EVar (Id (operator_name op)), exp1), exp2))
;;

let cons_operation = check_chunk "::" *> return (fun exp1 exp2 -> ECons (exp1, exp2))

let unary_operation op pfe =
  check_chunk op *> pfe
  >>= fun exp1 -> return (EApp (EVar (Id (unary_operator_name op)), exp1))
;;

let pfemul pfe = chainl1 pfe (bin_operation "*")
let pfediv pfe = chainl1 pfe (bin_operation "/")
let pfeadd pfe = chainl1 pfe (bin_operation "+")
let pfesub pfe = chainl1 pfe (bin_operation "-")
let pfeeq pfe = chainl1 pfe (bin_operation "=")
let pfereq pfe = chainl1 pfe (bin_operation "==")
let pfeneq pfe = chainl1 pfe (bin_operation "<>")
let pferneq pfe = chainl1 pfe (bin_operation "!=")
let pfeles pfe = chainl1 pfe (bin_operation "<")
let pfeleq pfe = chainl1 pfe (bin_operation "<=")
let pfegre pfe = chainl1 pfe (bin_operation ">")
let pfegeq pfe = chainl1 pfe (bin_operation ">=")
let pfeand pfe = chainr1 pfe (bin_operation "&&")
let pfeor pfe = chainr1 pfe (bin_operation "||")
let pfeunplus pfe = unary_operation "+" pfe
let pfeunminus pfe = unary_operation "-" pfe
let pfecons pfe = chainr1 pfe cons_operation

let parse_binops pfe =
  let pfe = pfemul pfe <|> pfe in
  let pfe = pfediv pfe <|> pfe in
  let pfe = pfeadd pfe <|> pfe in
  let pfe = pfesub pfe <|> pfe in
  let pfe = pfecons pfe <|> pfe in
  let pfe = pfeneq pfe <|> pfe in
  let pfe = pferneq pfe <|> pfe in
  let pfe = pfeeq pfe <|> pfe in
  let pfe = pfereq pfe <|> pfe in
  let pfe = pfeles pfe <|> pfe in
  let pfe = pfeleq pfe <|> pfe in
  let pfe = pfegre pfe <|> pfe in
  let pfe = pfegeq pfe <|> pfe in
  pfe
;;

let parse_application pfe = chainl1 pfe (return (fun exp1 exp2 -> EApp (exp1, exp2)))

let parse_list_constr pfe =
  fix
  @@ fun parse_list_constr ->
  check_chunk "["
  *> (sep_by (check_chunk ";") (pfe <|> parse_list_constr)
      >>=
      let rec helper = function
        | [] -> return (EConst CEmptyList)
        | [ h ] -> return (ECons (h, EConst CEmptyList))
        | h :: tl -> helper tl >>= fun x -> return (ECons (h, x))
      in
      helper)
  <* check_chunk "]"
;;

let parse_tuple pfe =
  sep_by (check_chunk ",") pfe
  >>= function
  | [] -> pfe
  | [ h ] -> return h
  | h :: tl -> return (ETuple (h :: tl))
;;

let eif a b c = EIf (a, b, c)

let parse_if pfexp =
  fix
  @@ fun parse_if ->
  lift3
    eif
    (check_token "if" *> (pfexp <|> parse_if))
    (check_token "then" *> (pfexp <|> parse_if))
    (check_token "else" *> (pfexp <|> parse_if))
;;

let ematch a b = EMatch (a, b)

let parse_match pfe =
  let caseWithout =
    lift2 (fun pat expr -> pat, expr) (parse_pattern <* check_chunk "->") pfe
  in
  let caseWith =
    lift2
      (fun pat expr -> pat, expr)
      (check_chunk "|" *> parse_pattern <* check_chunk "->")
      pfe
  in
  let allCases =
    lift2 (fun first other -> first :: other) (caseWith <|> caseWithout) (many @@ caseWith)
  in
  lift2 ematch (check_token "match" *> pfe <* check_token "with") allCases
;;

let efun a b = EFun (a, b)

let parse_fun pfe =
  check_token "fun" *> lift2 efun (many1 @@ parse_pattern) (check_chunk "->" *> pfe)
;;

let elet a b c = ELet (a, b, c)

let add_pattern_type_to typ var_name = function
  | PType (_, pt) -> TArrow (pt, typ)
  | _ -> TArrow (TVar (Id ("Var" ^ Int.to_string var_name)), typ)
;;

let get_full_type binding_params typ =
  let rec helper acc num = function
    | [] -> return acc
    | [ h ] -> return (add_pattern_type_to acc num h)
    | h :: tl -> helper (add_pattern_type_to acc num h) (num - 1) tl
  in
  helper typ (List.length binding_params - 1) (List.rev binding_params)
;;

let get_binding_expr binding_params binding_expr =
  match binding_params with
  | [] -> binding_expr
  | _ -> EFun (binding_params, binding_expr)
;;

let parse_value_binding_with_type pfexp =
  let* pat = parse_pattern in
  let* binding_params = many @@ parse_pattern in
  let* typ = check_chunk ":" *> parse_type in
  let* full_type = get_full_type binding_params typ in
  let* binding_expr = check_chunk "=" *> pfexp in
  return (ptype pat full_type, get_binding_expr binding_params binding_expr)
;;

let parse_value_binding_without_type pfexp =
  let* pat = parse_pattern in
  let* binding_params = many @@ parse_pattern in
  let* binding_expr = check_chunk "=" *> pfexp in
  return (pat, get_binding_expr binding_params binding_expr)
;;

let parse_value_binding pfexp =
  parse_value_binding_without_type pfexp <|> parse_value_binding_with_type pfexp
;;

let parse_let pfexp =
  check_token "let"
  *> lift3
       elet
       (check_token "rec" *> return Recursive <|> return Nonrecursive)
       (parse_value_binding pfexp)
       (check_token "in" *> pfexp)
;;

let etype a b = EType (a, b)
let parse_typed_expr pfexp = pparens @@ lift2 etype pfexp (check_chunk ":" *> parse_type)

(* doc:https://v2.ocaml.org/manual/expr.html#ss:precedence-and-associativity *)
let parse_expr =
  fix
  @@ fun pfexpr ->
  let pfe = pparens pfexpr <|> parse_list_constr pfexpr <|> parse_typed_expr pfexpr in
  let pfe = parse_id >>= (fun ident -> return (EVar ident)) <|> pfe in
  let pfe = parse_constant >>= (fun const -> return (EConst const)) <|> pfe in
  let pfe = parse_application pfe <|> pfe in
  let pfe = pfeunminus pfe <|> pfeunplus pfe <|> pfe in
  let pfe = parse_binops pfe <|> pfe in
  let pfe = pfeand pfe <|> pfe in
  let pfe = pfeor pfe <|> pfe in
  let pfe = parse_tuple pfe <|> pfe in
  let pfe = parse_if pfexpr <|> pfe in
  let pfe = parse_let pfexpr <|> parse_match pfexpr <|> parse_fun pfexpr <|> pfe in
  pfe
;;

(*********************Structure*Item*Parser*******************)

let parse_let_declaration =
  check_token "let"
  *> lift2
       (fun rec_flag binding_lst -> SILet (rec_flag, binding_lst))
       (check_token "rec" *> return Recursive <|> return Nonrecursive)
       (sep_by1 (check_token "and") (parse_value_binding parse_expr))
;;

let parse_semicolon = many @@ check_chunk ";;"

let parse_structure_item =
  parse_let_declaration
  <|> (parse_expr >>= fun expr -> return (SIExpr expr))
  <* parse_semicolon
;;

(**************************Structure*Parser************************)
let parse_structure = parse_semicolon *> many1 parse_structure_item

(**************************Parsers*****************************)
let id_from_string str = parse_string ~consume:All (parse_id <* spaces) str
let const_from_string str = parse_string ~consume:All (parse_constant <* spaces) str
let type_from_string str = parse_string ~consume:All (parse_type <* spaces) str
let pattern_from_string str = parse_string ~consume:All (parse_pattern <* spaces) str
let expr_from_string str = parse_string ~consume:All (parse_expr <* spaces) str

let structure_item_from_string str =
  parse_string ~consume:All (parse_structure_item <* spaces) str
;;

let structure_from_string str = parse_string ~consume:All (parse_structure <* spaces) str
