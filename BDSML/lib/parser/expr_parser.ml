(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Angstrom
open Ast
open Utils

let parse_const =
  let+ const = Const_parser.parse_const in
  Exp_constant const
;;

(** https://ocaml.org/manual/5.2/lex.html#sss:lex-ops-symbols *)
let is_core_operator_char = function
  | '$' | '&' | '*' | '+' | '-' | '/' | '=' | '>' | '@' | '^' | '|' -> true
  | _ -> false
;;

let is_operator_char a =
  is_core_operator_char a
  ||
  match a with
  | '~' | '!' | '?' | '%' | '<' | ':' | '.' -> true
  | _ -> false
;;

let parse_prefix_op =
  let case1 =
    let* first_el = char '?' <|> char '~' in
    let+ other_el = take_while1 is_operator_char in
    Char.escaped first_el ^ other_el
  in
  let case2 =
    let* first_el = char '!' in
    let+ other_el = take_while is_operator_char in
    Char.escaped first_el ^ other_el
  in
  case1 <|> case2
;;

let parse_infix_op prefix =
  let case1 =
    let+ first_el = char '#'
    and+ other_el = take_while1 is_operator_char in
    Char.escaped first_el ^ other_el
  in
  let case2 =
    let+ first_el = satisfy is_core_operator_char <|> char '%' <|> char '<'
    and+ other_el = take_while is_operator_char in
    Char.escaped first_el ^ other_el
  in
  let* check_prefix = peek_string @@ String.length prefix in
  if String.equal check_prefix prefix then case1 <|> case2 else fail ""
;;

let parse_ident =
  let+ ident =
    ws
    *> (parse_lowercase_ident <|> parse_prefix_op <|> remove_parents @@ parse_infix_op "")
  in
  Exp_ident ident
;;

let rec unary_chain (func : string t) arg_parser =
  let* parsed_func = ws *> func in
  let+ arg = unary_chain func arg_parser <|> arg_parser in
  let ident = Exp_ident parsed_func in
  Exp_apply (ident, arg)
;;

let parse_infix_with_prefixes prefixes = choice (List.map parse_infix_op prefixes)

let parse_bop (op : string t) =
  let+ parsed = ws *> op in
  let ident = Exp_ident parsed in
  fun a b -> Exp_apply (ident, Exp_tuple [ a; b ])
;;

let prefix_op parser prev = unary_chain parser prev <|> prev
let infix_left_op parser prev = chainl1 prev (parse_bop parser)
let infix_right_op parser prev = chainr1 prev (parse_bop parser)

let application prev =
  let+ name = parse_ident
  and+ args = many1 prev in
  Exp_apply (name, Exp_tuple args)
;;

let constructor prev =
  let+ ident = parse_capitalized_ident
  and+ arg = prev >>| Option.some <|> return None in
  Exp_construct (ident, arg)
;;

let tuple prev =
  sep_by1 (ws *> char ',') prev
  >>= function
  | _ :: _ :: _ as l -> return @@ Exp_tuple l
  | [ e ] -> return e
  | _ -> fail ""
;;

let let_parser prev =
  let+ _ = ws *> string "let"
  and+ rec_flag = ws *> string "rec" *> return Recursive <|> return Nonrecursive
  and+ bindings =
    let pat_expr =
      let+ pat = ws *> Pattern_parser.parse_pattern
      and+ _ = ws *> char '='
      and+ expr = prev in
      { pat; expr }
    in
    sep_by1 (ws *> string "and") pat_expr
  and+ _ = ws *> string "in"
  and+ expr = prev in
  Exp_let (rec_flag, bindings, expr)
;;

let spec_parser = [ let_parser ]

let choice_pass_prev parsers_list prev =
  choice @@ List.map (fun el -> el prev) parsers_list
;;

(** https://ocaml.org/manual/5.2/expr.html#ss%3Aprecedence-and-associativity
    by priority from higher to lower*)
let priority =
  [ prefix_op @@ parse_prefix_op
  ; infix_left_op @@ parse_infix_op "#"
  ; choice_pass_prev [ application; constructor; Fun.id ]
  ; infix_right_op @@ parse_infix_op "**"
  ; prefix_op @@ choice [ string "-."; string "-" ]
  ; infix_left_op @@ parse_infix_with_prefixes [ "*"; "/"; "%" ]
  ; infix_left_op @@ parse_infix_with_prefixes [ "+"; "-" ]
  ; infix_right_op @@ string "::"
  ; infix_right_op @@ parse_infix_with_prefixes [ "@"; "^" ]
  ; infix_left_op
    @@ choice [ parse_infix_with_prefixes [ "="; "<"; ">"; "|"; "&"; "$" ]; string "!=" ]
  ; infix_right_op @@ choice [ string "&"; string "&&" ]
  ; infix_right_op @@ choice [ string "or"; string "||" ]
  ; tuple
  ; infix_right_op @@ choice [ string "<-"; string ":=" ]
  ; infix_right_op @@ string ";"
  ; choice_pass_prev @@ spec_parser @ [ Fun.id ]
  ]
;;

let parse_expr =
  fix (fun self ->
    let rec parse_priority prev = function
      | h :: tl -> parse_priority (h prev) tl
      | [] -> prev
    in
    let init =
      parse_const
      <|> parse_ident
      <|> remove_parents self
      <|> choice_pass_prev spec_parser self
    in
    parse_priority init priority)
;;
