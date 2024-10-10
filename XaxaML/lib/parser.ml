(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Angstrom
open Ast

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_low_letter = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_up_letter = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_letter c = is_low_letter c || is_up_letter c

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_keyword = function
  | "else"
  | "false"
  | "fun"
  | "if"
  | "in"
  | "let"
  | "match"
  | "or"
  | "rec"
  | "then"
  | "true"
  | "with"
  | "and" -> true
  | _ -> false
;;

let take_whitespaces = take_while is_whitespace
let take_whitespaces1 = take_while1 is_whitespace
let take_token1 = take_while1 (fun c -> is_letter c || is_digit c || c = '_')

let token s =
  take_whitespaces *> take_token1
  >>= fun res ->
  if res = s then return s else fail @@ Format.sprintf "token %s expected" s
;;

let whitespace1_token s =
  take_whitespaces1 *> take_token1
  >>= fun res ->
  if res = s then return s else fail @@ Format.sprintf "token %s expected" s
;;

let left_bracket = take_whitespaces *> char '('
let right_bracket = take_whitespaces *> char ')'
let parenthesis p = left_bracket *> take_whitespaces *> p <* right_bracket

(* https://ocaml.org/manual/5.2/lex.html#sss:lex-ops-symbols *)
let is_core_operator_char c =
  let list = [ '$'; '&'; '*'; '+'; '-'; '/'; '='; '>'; '@'; '^'; '|' ] in
  List.mem c list
;;

let is_operator_char c =
  is_core_operator_char c || List.mem c [ '~'; '!'; '?'; '%'; '<'; ':'; '.' ]
;;

let take_operator_chars1 = take_while1 (fun c -> is_operator_char c || Char.equal c '#')

let operator str =
  take_whitespaces *> take_operator_chars1
  >>= fun res ->
  if res = str then return str else fail @@ Format.sprintf "operator %s expected" str
;;

let val_name =
  take_token1
  >>= fun s ->
  let c = s.[0] in
  if (not (is_keyword s)) && s <> "_" && (is_low_letter c || c = '_')
  then return s
  else fail "name of value expected"
;;

let operator_name =
  char '(' *> take_whitespaces *> take_operator_chars1 <* take_whitespaces <* char ')'
;;

let ident = val_name <|> operator_name
let expr_ident = take_whitespaces *> ident >>| fun x -> E_ident x

let const_integer =
  take_token1
  >>= fun s ->
  let cons x = C_int x in
  try int_of_string s |> cons |> return with
  | Failure _ -> fail "integer expected"
;;

let const_bool =
  take_token1
  >>= function
  | "false" -> return @@ C_bool false
  | "true" -> return @@ C_bool true
  | _ -> fail "Bool constant expected"
;;

let const_empty_list = char '[' *> take_whitespaces *> char ']' *> return C_empty_list
let const_unit = char '(' *> take_whitespaces *> char ')' *> return C_unit
let parse_const = choice [ const_bool; const_integer; const_empty_list; const_unit ]
let expr_const = take_whitespaces *> parse_const >>| fun c -> E_const c

let list_cons_op =
  take_whitespaces *> operator "::" *> return (fun e1 e2 -> E_cons_list (e1, e2))
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let unary_plus_or_minus expr_item_parser =
  fix (fun cur_expr ->
    take_whitespaces
    *> choice
         [ (operator "+" *> cur_expr >>| fun e -> E_app (E_ident "~+", e))
         ; (operator "-" *> cur_expr >>| fun e -> E_app (E_ident "~-", e))
         ; expr_item_parser
         ])
;;

let if_then_else expr =
  lift3
    (fun e1 e2 e3 -> E_ite (e1, e2, e3))
    (token "if" *> expr)
    (token "then" *> expr)
    (token "else" *> expr)
;;

let parse_tuple expr =
  expr
  >>= (fun first_expr ->
  many1 (take_whitespaces *> char ',' *> expr)
  >>| fun expr_list -> E_tuple (first_expr, expr_list))
  <|> expr
;;

let prim_typ =
  choice [ token "bool"; token "int"; token "unit" ] >>| fun str -> RT_prim str
;;

let type_var_name =
  val_name
  >>= fun s ->
  if String.length s > 0 && is_letter s.[0] then return s else fail "type  name expected"
;;

let type_var = take_whitespaces *> char '\'' *> type_var_name >>| fun str -> RT_var str
let type_list ty = ty <* token "list" >>| (fun t -> RT_list t) <|> ty

let type_tuple ty =
  chainr1
    (ty >>| fun t -> [ t ])
    (operator "*" *> return (fun l r -> List.concat [ l; r ]))
  >>| fun lst ->
  if List.length lst = 1 then List.nth lst 0 else RT_tuple (List.hd lst, List.tl lst)
;;

let one_arrow = operator "->" *> return (fun t1 t2 -> RT_arr (t1, t2))

let typ =
  fix (fun typ_parser ->
    let typ_parser = prim_typ <|> type_var <|> parenthesis typ_parser in
    let typ_parser = type_list typ_parser in
    let typ_parser = type_tuple typ_parser in
    chainr1 typ_parser one_arrow)
  <* take_whitespaces
;;

let typed_expr expr =
  parenthesis
    (expr >>= fun e -> take_whitespaces *> char ':' *> typ >>| fun t -> E_typed (e, t))
;;

let parse_list expr =
  take_whitespaces
  *> char '['
  *> fix (fun cur_parser ->
    choice
      [ (expr
         <* take_whitespaces
         <* char ']'
         >>| fun e -> E_cons_list (e, E_const C_empty_list))
      ; (expr
         <* take_whitespaces
         <* char ';'
         >>= fun e -> cur_parser >>| fun l -> E_cons_list (e, l))
      ])
;;

let parse_any_pat =
  take_whitespaces *> take_token1
  >>= fun s -> if s = "_" then return P_any else fail {|Pattern "any" expected|}
;;

let parse_val_pat = take_whitespaces *> ident >>| fun s -> P_val s
let parse_const_pat = take_whitespaces *> parse_const >>| fun c -> P_const c

let typed_pat pat =
  parenthesis
    (pat >>= fun p -> take_whitespaces *> char ':' *> typ >>| fun t -> P_typed (p, t))
;;

let parse_pat =
  take_whitespaces
  *> fix (fun cur_pat ->
    let parse_base_pat =
      choice [ parse_any_pat; parse_val_pat; parse_const_pat; typed_pat cur_pat ]
    in
    let parse_cons_pat =
      let helper =
        take_whitespaces *> operator "::" *> return (fun p1 p2 -> P_cons_list (p1, p2))
      in
      chainr1 (parenthesis cur_pat <|> parse_base_pat) helper
    in
    let parse_tuple_pat =
      parenthesis cur_pat
      <|> parse_base_pat
      >>= fun first_pat ->
      many1 (take_whitespaces *> char ',' *> (parenthesis cur_pat <|> parse_base_pat))
      >>| fun pat_list -> P_tuple (first_pat, pat_list)
    in
    choice [ parse_tuple_pat; parse_cons_pat; parse_base_pat; parenthesis cur_pat ])
;;

let expr_match expr =
  let case_common =
    parse_pat >>= fun pat -> take_whitespaces *> operator "->" *> expr >>| fun e -> pat, e
  in
  let case0 = take_whitespaces *> case_common in
  let case1 = take_whitespaces *> char '|' *> case_common in
  token "match" *> expr
  >>= fun e ->
  token "with" *> (case0 <|> case1 >>= fun h -> many case1 >>| fun tl -> h :: tl)
  >>| fun l -> E_match (e, l)
;;

let expr_fun expr =
  token "fun" *> take_whitespaces *> many1 parse_pat
  >>= fun args ->
  let first_arg, other_args = List.hd args, List.tl args in
  take_whitespaces *> operator "->" *> expr >>| fun e -> E_fun (first_arg, other_args, e)
;;

let decl_type = option None (char ':' *> take_whitespaces *> typ >>| fun x -> Some x)

let create_fun_type args = function
  | Some ret_typ ->
    Some
      (fst
         (List.fold_right
            (fun arg_pat (cur_typ, num) ->
              match arg_pat with
              | P_typed (_, ty) -> RT_arr (ty, cur_typ), num
              | _ -> RT_arr (RT_var (Int.to_string num), cur_typ), num + 1)
            args
            (ret_typ, 0)))
  | None -> None
;;

let decl_body expr =
  parse_pat
  >>= fun pat ->
  choice
    [ (take_whitespaces *> decl_type
       >>= fun ty -> operator "=" *> expr >>| fun e -> pat, ty, e)
    ; (many1 parse_pat
       >>= fun args ->
       let first_arg, other_args = List.hd args, List.tl args in
       take_whitespaces *> decl_type
       >>= fun ret_typ ->
       operator "=" *> expr
       >>| fun e -> pat, create_fun_type args ret_typ, E_fun (first_arg, other_args, e))
    ]
;;

let non_rec_decl expr =
  token "let" *> decl_body expr >>| fun decl_body -> Non_rec decl_body
;;

let rec_decl expr =
  token "let"
  *> whitespace1_token "rec"
  *> chainr1
       (decl_body expr >>| fun body -> [ body ])
       (token "and" *> return (fun l1 l2 -> List.concat [ l1; l2 ]))
  >>| fun decl_list -> Rec decl_list
;;

let decl expr = non_rec_decl expr <|> rec_decl expr
let let_in expr = lift2 (fun decl e -> E_let (decl, e)) (decl expr <* token "in") expr
let bin_op str = operator str *> return (fun e1 e2 -> E_app (E_app (E_ident str, e1), e2))

let expr =
  take_whitespaces
  *> fix (fun all_expr ->
    let cur_expr =
      choice
        [ expr_const
        ; expr_ident
        ; parenthesis all_expr
        ; parse_list all_expr
        ; typed_expr all_expr
        ]
    in
    let cur_expr = chainl1 cur_expr (return (fun e1 e2 -> E_app (e1, e2))) in
    let cur_expr = unary_plus_or_minus cur_expr in
    let cur_expr = chainl1 cur_expr (bin_op "*" <|> bin_op "/") in
    let cur_expr = chainl1 cur_expr (bin_op "+" <|> bin_op "-") in
    let cur_expr = chainr1 cur_expr list_cons_op in
    let cur_expr =
      chainl1
        cur_expr
        (choice @@ List.map bin_op [ "=="; "="; "<>"; "!="; "<"; ">"; "<="; ">=" ])
    in
    let cur_expr = chainr1 cur_expr (bin_op "&&") in
    let cur_expr = chainr1 cur_expr (bin_op "||") in
    let cur_expr = parse_tuple cur_expr in
    choice
      [ if_then_else all_expr
      ; let_in all_expr
      ; expr_match all_expr
      ; expr_fun all_expr
      ; cur_expr
      ])
  <* take_whitespaces
;;

let toplevel_decl =
  decl expr >>| (fun decl -> Let_decl decl) <* take_whitespaces <* option "" (string ";;")
;;

let expr_top = expr >>| fun e -> Expr e

let program_parser : program t =
  let empty_decl = many (take_whitespaces *> string ";;") in
  many1 (empty_decl *> (toplevel_decl <|> expr_top) <* empty_decl) <* take_whitespaces
;;

let run_parser_program s = parse_string ~consume:All program_parser s
let run_parser_expr s = parse_string ~consume:All expr s
