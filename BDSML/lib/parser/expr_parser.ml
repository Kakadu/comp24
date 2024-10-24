(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Angstrom
open Ast
open Utils

let parse_const =
  let+ const = Const_parser.parse_const in
  Exp_constant const
;;

(** https://ocaml.org/manual/5.2/lex.html#sss:lex-ops-symbols
    '|' moved from core by purpose *)
let is_core_operator_char = function
  | '$' | '&' | '*' | '+' | '-' | '/' | '=' | '>' | '@' | '^' -> true
  | _ -> false
;;

let is_operator_char a =
  is_core_operator_char a
  ||
  match a with
  | '~' | '!' | '?' | '%' | '<' | ':' | '.' | '|' -> true
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
    let+ first_el = char '#' <|> char '|'
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
  sep_by1 (check_char ',') prev
  >>= function
  | _ :: _ :: _ as l -> return @@ Exp_tuple l
  | [ e ] -> return e
  | _ -> fail ""
;;

let let_parser prev =
  let+ _ = check_string "let" <* ws1
  and+ rec_flag = check_string "rec" *> return Recursive <* ws1 <|> return Nonrecursive
  and+ bindings =
    let pat_expr =
      let+ pat = ws *> Pattern_parser.parse_pattern
      and+ _ = check_char '='
      and+ expr = prev in
      { pat; expr }
    in
    sep_by1 (check_string "and" <* ws1) pat_expr
  and+ _ = check_string "in" <* ws1
  and+ expr = prev in
  Exp_let (rec_flag, bindings, expr)
;;

let parse_cases prev =
  let case =
    let+ left = Pattern_parser.parse_pattern
    and+ _ = check_string "->"
    and+ right = prev in
    { left; right }
  in
  sep_by1 (check_char '|') case
;;

let function_parser prev =
  let+ _ = check_string "function"
  and+ _ = check_char '|' <|> ws1 *> return ' '
  and+ cases = parse_cases prev in
  Exp_function cases
;;

let fun_parser prev =
  let+ _ = check_string "fun" <* ws1
  and+ args =
    let arg = Pattern_parser.parse_pattern in
    sep_by1 ws1 arg
  and+ typexpr = option None (Typexpr_parser.parse_typexpr >>| Option.some)
  and+ _ = check_string "->"
  and+ expr = prev in
  let exp_fun = Exp_fun (args, expr) in
  match typexpr with
  | Some typexpr -> Exp_type (exp_fun, typexpr)
  | _ -> exp_fun
;;

let match_parser prev =
  let+ _ = check_string "match" <* ws1
  and+ expr = prev
  and+ _ = check_string "with"
  and+ _ = check_char '|' <|> ws1 *> return ' '
  and+ cases = parse_cases prev in
  Exp_match (expr, cases)
;;

let if_parser prev =
  let+ _ = check_string "if" <* ws1
  and+ expr1 = prev
  and+ _ = check_string "then" <* ws1
  and+ expr2 = prev
  and+ else_res =
    option None
    @@
    let+ _ = check_string "else" <* ws1
    and+ expr3 = prev in
    Some expr3
  in
  Exp_if (expr1, expr2, else_res)
;;

let exp_sequence prev =
  let del = check_string ";" *> return (fun a b -> Exp_sequence (a, b)) in
  chainr1 prev del
;;

let exp_list_cons prev =
  let del =
    check_string "::"
    *> return (fun a b -> Exp_construct ("::", Some (Exp_tuple [ a; b ])))
  in
  chainr1 prev del
;;

let list_parser prev =
  let rec helper = function
    | h :: tl -> Exp_construct ("::", Some (Exp_tuple [ h; helper tl ]))
    | [] -> Exp_construct ("[]", None)
  in
  remove_square_brackets @@ sep_by (check_char ';') prev >>| helper
;;

let typexpr_parser prev =
  remove_parents
  @@
  let+ expr = prev
  and+ typexpr = Typexpr_parser.parse_typexpr in
  Exp_type (expr, typexpr)
;;

let spec_parser = [ let_parser; fun_parser; function_parser; match_parser ]

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
  ; exp_list_cons
  ; infix_right_op @@ parse_infix_with_prefixes [ "@"; "^" ]
  ; infix_left_op
    @@ choice [ parse_infix_with_prefixes [ "="; "<"; ">"; "|"; "&"; "$" ]; string "!=" ]
  ; infix_right_op @@ choice [ string "&"; string "&&" ]
  ; infix_right_op @@ choice [ string "or"; string "||" ]
  ; tuple
  ; infix_right_op @@ choice [ string "<-"; string ":=" ]
  ; choice_pass_prev [ if_parser; Fun.id ]
  ; choice_pass_prev [ list_parser; Fun.id ]
  ; exp_sequence
  ; choice_pass_prev @@ spec_parser @ [ Fun.id ]
  ]
;;

let parse_expr =
  fix (fun self ->
    parse_by_priority priority
    @@ choice
         [ parse_const
         ; parse_ident
         ; remove_parents self
         ; choice_pass_prev spec_parser self
         ; if_parser self
         ; list_parser self
         ; typexpr_parser self
         ])
;;
