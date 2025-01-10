(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Angstrom
open Ast

(* Utilities *)
let is_keyword = function
  | "if"
  | "else"
  | "let"
  | "bool"
  | "string"
  | "int"
  | "rec"
  | "match"
  | "fun"
  | "false"
  | "true"
  | "then"
  | "and"
  | "_"
  | "in" -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_whitespace = function
  | ' ' | '\n' -> true
  | _ -> false
;;

let is_valid_fst_char_ident = function
  | 'a' .. 'z' | '_' -> true
  | _ -> false
;;

let is_valid_fst_char_poly_type = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

let is_identifier_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

let is_op_symb = function
  | "-" | "+" | "/" | "*" | "<" | "<=" | ">=" | "<>" | ">" | "::" | "=" | "==" -> true
  | _ -> false
;;

let is_inf_op_symb = function
  | '-' | '+' | '/' | '*' | '<' | '>' | ':' | '=' -> true
  | _ -> false
;;

let skip_whitespace = skip_while is_whitespace
let between left right exp = left *> exp <* right

let between_parens exp =
  skip_whitespace *> between (Angstrom.char '(') (Angstrom.char ')') exp
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let p_ident_string p_valid_fst_char =
  skip_whitespace
  *>
  let* fst_char = peek_char in
  match fst_char with
  | Some c when p_valid_fst_char c ->
    let* identifier = take_while is_identifier_char in
    if is_keyword identifier
    then fail "Identifier parsing failed: identifier can't be a keyword"
    else return identifier
  | _ -> fail "Identifier parsing failed: first character must start with [a-z]"
;;

let p_infix_ident =
  let* some_str =
    between_parens (skip_whitespace *> take_while1 is_inf_op_symb <* skip_whitespace)
  in
  if some_str |> is_op_symb
  then return (PIdentifier ("( " ^ some_str ^ " )"))
  else fail "Parsed string wasn't an supported operator"
;;

(* Type parsers *)

let p_basic_type : type_name t =
  Angstrom.string "int" *> return TInt
  <|> Angstrom.string "bool" *> return TBool
  <|> Angstrom.string "unit" *> return TUnit
  <|> char '\''
      *> let* typeNameChar = p_ident_string is_valid_fst_char_poly_type in
         return (TPoly typeNameChar)
;;

let p_tuple_type p_type =
  let* fst_component = skip_whitespace *> p_type in
  let* exp_list = many1 (skip_whitespace *> char '*' *> skip_whitespace *> p_type) in
  return (TTuple (fst_component :: exp_list))
;;

let p_function_type p_type =
  chainr1
    p_type
    (skip_whitespace
     *> Angstrom.string "->"
     *> skip_whitespace
     *> return (fun arg ret -> TFunction (arg, ret)))
;;

let rec p_list_type t =
  let* base = t in
  let* _ = skip_whitespace *> Angstrom.string "list" in
  p_list_type (return (TList base)) <|> return (TList base)
;;

let p_type : type_name t =
  fix (fun p_type ->
    let atomic_type = p_basic_type <|> between_parens p_type in
    let list_type = p_list_type atomic_type <|> atomic_type in
    let tuple_type = p_tuple_type list_type <|> list_type in
    p_function_type tuple_type <|> tuple_type)
;;

(* Constant parsers*)
let p_cint =
  let p_number = skip_whitespace *> take_while1 is_digit in
  let p_signed =
    between_parens
      (let* sign = skip_whitespace *> (Angstrom.string "-" <|> Angstrom.string "+") in
       let* num = p_number in
       return (sign ^ num))
  in
  let* number = p_number <|> p_signed in
  return @@ CInt (number |> int_of_string)
;;

let p_cbool =
  let* bool_val =
    skip_whitespace *> (Angstrom.string "true" <|> Angstrom.string "false")
  in
  return (CBool (bool_val |> bool_of_string))
;;

let p_cunit =
  Angstrom.char '(' *> skip_whitespace *> return CUnit
  <* skip_whitespace
  <* Angstrom.char ')'
;;

let p_const_expr =
  let* const = p_cint <|> p_cbool <|> p_cunit in
  return (EConstant const)
;;

let p_const_pattern =
  let* const = p_cint <|> p_cbool <|> p_cunit in
  return (PConstant const)
;;

(* If the else parser *)

let p_if_then_else p_expr =
  let* guard = skip_whitespace *> Angstrom.string "if" *> p_expr in
  let* if_expr = skip_whitespace *> Angstrom.string "then" *> p_expr in
  let* else_expr = skip_whitespace *> Angstrom.string "else" *> p_expr in
  return (EIfThenElse (guard, if_expr, else_expr))
;;

(* Identifiers (Vars) parsers *)
let p_ident =
  let* ident_name = p_ident_string is_valid_fst_char_ident in
  return @@ EIdentifier ident_name
;;

let p_ident_pattern =
  let* ident_name = p_ident_string is_valid_fst_char_ident in
  return @@ PIdentifier ident_name
;;

(* List parsers *)

let p_list_no_constr p_exp =
  let* fst_component = skip_whitespace *> Angstrom.string "[" *> p_exp in
  let* exp_list =
    many (skip_whitespace *> Angstrom.char ';' *> p_exp) <* Angstrom.string "]"
  in
  return (fst_component :: exp_list)
;;

let rec exp_cons_list_builder = function
  | [] -> EConstant CNil
  | h :: tl ->
    EApplication (EIdentifier "( :: )", h)
    |> fun app1 -> EApplication (app1, exp_cons_list_builder tl)
;;

let p_list_not_empty_exp p_exp =
  let* exp_list = p_list_no_constr p_exp in
  return (exp_cons_list_builder exp_list)
;;

let p_pnil =
  skip_whitespace *> Angstrom.string "[" *> Angstrom.string "]" *> return (PConstant CNil)
;;

let p_enil =
  skip_whitespace *> Angstrom.string "[" *> Angstrom.string "]" *> return (EConstant CNil)
;;

let p_list_exp p_exp = p_list_not_empty_exp p_exp <|> p_enil

let p_wild_card_pattern =
  skip_whitespace
  *> Angstrom.string "_"
  *> let* next = peek_char in
     match next with
     | Some c when is_identifier_char c -> fail "a"
     | _ -> return PWildCard
;;

(* Tuple parsers *)

let p_tuple p_exp constructor =
  let* fst_component = skip_whitespace *> Angstrom.string "(" *> p_exp in
  let* exp_list =
    many1 (skip_whitespace *> Angstrom.char ',' *> p_exp) <* Angstrom.string ")"
  in
  fst_component :: exp_list |> constructor |> return
;;

let p_unit constr =
  skip_whitespace
  *> Angstrom.string "("
  *> skip_whitespace
  *> Angstrom.string ")"
  *> return constr
;;

let p_tuple_expr p_exp = p_tuple p_exp (fun x -> ETuple x) <|> p_unit (EConstant CUnit)
let p_tuple_pattern p_exp = p_tuple p_exp (fun x -> PTuple x) <|> p_unit (PConstant CUnit)

let rec pat_cons_list_builder (ls : pattern list) =
  match ls with
  | [] -> PConstant CNil
  | h :: tl -> PCons (h, pat_cons_list_builder tl)
;;

let p_list_not_empty_pattern p_pattern =
  let* pat_list = p_list_no_constr p_pattern in
  return (pat_cons_list_builder pat_list)
;;

let cons_delim_pattern =
  skip_whitespace
  *> Angstrom.string "::"
  *> return (fun operand1 operand2 -> PCons (operand1, operand2))
;;

let p_list_pattern p_pattern = p_list_not_empty_pattern p_pattern <|> p_pnil

(* final pattern parsers*)
let p_cons_pattern p_pattern = chainr1 p_pattern cons_delim_pattern

let p_pattern_with_type p_pat =
  let* pattern = skip_whitespace *> Angstrom.string "(" *> p_pat in
  let* constr =
    skip_whitespace *> Angstrom.string ":" *> skip_whitespace *> p_type
    <* skip_whitespace
    <* Angstrom.string ")"
  in
  return (PConstraint (pattern, constr))
;;

let p_pattern =
  fix (fun p_pattern ->
    let atomic_pat =
      p_ident_pattern
      <|> p_const_pattern
      <|> p_infix_ident
      <|> between_parens p_pattern
      <|> p_list_pattern p_pattern
      <|> p_pattern_with_type p_pattern
      <|> p_tuple_pattern p_pattern
    in
    let w_card_pat = p_wild_card_pattern <|> atomic_pat in
    let cons_pat = p_cons_pattern w_card_pat <|> w_card_pat in
    cons_pat)
;;

let p_patterns = sep_by (take_while1 is_whitespace) p_pattern

let rec build_nested_expr args acc =
  match args with
  | [] -> acc
  | h :: tl -> build_nested_expr tl (EFunction (h, acc))
;;

(* Binary operations parsers & delimiter for chains*)

let binary_operation op func =
  skip_whitespace
  *> Angstrom.string op
  *> return (fun operand1 operand2 ->
    EApplication (func, operand1) |> fun app1 -> EApplication (app1, operand2))
;;

let app_delim = return (fun operand1 operand2 -> EApplication (operand1, operand2))
let add_func = EIdentifier "( + )"
let sub_func = EIdentifier "( - )"
let mul_func = EIdentifier "( * )"
let div_func = EIdentifier "( / )"
let gr_func = EIdentifier "( > )"
let gr_or_eq_func = EIdentifier "( >= )"
let ls_func = EIdentifier "( < )"
let ls_or_eq_func = EIdentifier "( <= )"
let eq_func = EIdentifier "( = )"
let not_eq_func = EIdentifier "( <> )"
let cons_func = EIdentifier "( :: )"
let double_eq_func = EIdentifier "( == )"
let double_eq_expr = binary_operation "==" double_eq_func
let cons_delim_expr = binary_operation "::" cons_func
let not_eq_delim = binary_operation "<>" not_eq_func
let eq_delim = binary_operation "=" eq_func
let gr_delim = binary_operation ">" gr_func
let gr_or_eq_delim = binary_operation ">=" gr_or_eq_func
let ls_delim = binary_operation "<" ls_func
let ls_or_eq_delim = binary_operation "<=" ls_or_eq_func
let add_delim = binary_operation "+" add_func
let sub_delim = binary_operation "-" sub_func
let mul_delim = binary_operation "*" mul_func
let div_delim = binary_operation "/" div_func

(* functions parser *)
let p_function p_expr =
  let rec p_function_helper p_expr_h =
    let* pattern = p_pattern in
    let* fun_or_expr =
      p_function_helper p_expr_h <|> skip_whitespace *> Angstrom.string "->" *> p_expr_h
    in
    return @@ EFunction (pattern, fun_or_expr)
  in
  skip_whitespace *> Angstrom.string "fun" *> p_function_helper p_expr
;;

(* let in parser *)
let p_let_in p_exp =
  skip_whitespace
  *> Angstrom.string "let"
  *> let* flag =
       skip_whitespace *> Angstrom.string "rec " *> return Rec <|> return NotRec
     in
     let* pattern = skip_whitespace *> p_pattern in
     let* args = p_patterns in
     let* let_expr = skip_whitespace *> Angstrom.string "=" *> p_exp in
     let* body_expr = skip_whitespace *> Angstrom.string "in" *> p_exp in
     return
       (ELetIn (flag, pattern, build_nested_expr (List.rev args) let_expr, body_expr))
;;

(* match parser *)
let p_match p_pattern p_expr =
  let p_match_case p_pattern p_expr =
    let* pattern =
      skip_whitespace *> Angstrom.string "|" *> skip_whitespace *> p_pattern
    in
    let* expr = skip_whitespace *> Angstrom.string "->" *> skip_whitespace *> p_expr in
    return (pattern, expr)
  in
  let* sub_pat =
    skip_whitespace *> Angstrom.string "match" *> skip_whitespace *> p_pattern
    <* skip_whitespace
    <* Angstrom.string "with"
  in
  let* fst_case =
    let* pattern =
      skip_whitespace *> option "" (Angstrom.string "|") *> skip_whitespace *> p_pattern
    in
    let* expr = skip_whitespace *> Angstrom.string "->" *> skip_whitespace *> p_expr in
    return (pattern, expr)
  in
  let* cases = many1 (p_match_case p_pattern p_expr) in
  return @@ EMatch (sub_pat, fst_case :: cases)
;;

(* expression parser *)
let p_exp =
  let p_exp_with_type p_exp =
    let* exp = skip_whitespace *> Angstrom.string "(" *> p_exp in
    let* constr =
      skip_whitespace *> Angstrom.string ":" *> skip_whitespace *> p_type
      <* skip_whitespace
      <* Angstrom.string ")"
    in
    return (EConstraint (exp, constr))
  in
  fix (fun p_exp ->
    let atomic_exp =
      p_const_expr
      <|> p_ident
      <|> p_let_in p_exp
      <|> p_match p_pattern p_exp
      <|> p_function p_exp
      <|> between_parens p_exp
      <|> p_list_exp p_exp
      <|> p_exp_with_type p_exp
      <|> p_tuple_expr p_exp
      <|> p_if_then_else p_exp
    in
    let cons_term = chainr1 atomic_exp cons_delim_expr <|> atomic_exp in
    let app_term = chainl1 cons_term app_delim <|> cons_term in
    let high_pr_op_term = chainl1 app_term (mul_delim <|> div_delim) in
    let low_pr_op_term = chainl1 high_pr_op_term (add_delim <|> sub_delim) in
    let eq_term = chainl1 low_pr_op_term (double_eq_expr <|> eq_delim <|> not_eq_delim) in
    let gr_ls_term = chainl1 eq_term (gr_or_eq_delim <|> ls_or_eq_delim) in
    let gr_ls_eq_term = chainl1 gr_ls_term (gr_delim <|> ls_delim) in
    gr_ls_eq_term)
;;

(* let declarations parser*)
let p_let_decl p_exp =
  skip_whitespace
  *> Angstrom.string "let"
  *> skip_whitespace
  *> let* flag =
       skip_whitespace *> Angstrom.string "rec " *> return Rec <|> return NotRec
     in
     let* pattern = skip_whitespace *> p_pattern in
     let* args = p_patterns in
     let* expr = skip_whitespace *> Angstrom.string "=" *> p_exp in
     return @@ DSingleLet (flag, DLet (pattern, build_nested_expr (List.rev args) expr))
;;

let p_mutually_rec_decl =
  let p_mut_rec_decl p_exp =
    skip_whitespace
    *> Angstrom.string "and"
    *> skip_whitespace
    *>
    let* pattern = skip_whitespace *> p_pattern in
    let* args = p_patterns in
    let* expr = skip_whitespace *> Angstrom.string "=" *> p_exp in
    return (DLet (pattern, build_nested_expr (List.rev args) expr))
  in
  let* fst_dcl = p_let_decl p_exp in
  match fst_dcl with
  | DSingleLet (flag, x) ->
    let* other_lets = skip_whitespace *> many1 (p_mut_rec_decl p_exp) in
    return @@ DMutualRecDecl (flag, x :: other_lets)
  | _ -> fail "Error"
;;

let parse p s = parse_string ~consume:All p s

(* takes in code in OCaml and returns its AST*)
let parse_program =
  parse
    (sep_by (take_while1 is_whitespace) (p_mutually_rec_decl <|> p_let_decl p_exp)
     <* option "" (take_while1 is_whitespace))
;;

(* parser testing function *)
let test_parse input =
  match parse_program input with
  | Ok ast -> Format.printf "%a\n" Ast.pp_declarations ast
  | Error message -> Format.printf "Error: %s\n" message
;;
