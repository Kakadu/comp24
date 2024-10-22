(** Copyright 2024-2025, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

let is_space = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false
;;

let parse_space = take_while is_space
let parse_space1 = take_while1 is_space
let parse_token p = parse_space *> p
let parse_token1 p = parse_space1 *> p
let parse_stoken s = parse_token @@ string s
let parse_stoken1 s = parse_token1 @@ string s
let parse_next_stoken p = parse_stoken1 p <* parse_space1
let parse_parens p = parse_stoken "(" *> p <* parse_stoken ")"
let parse_parens_or_non p = parse_parens p <|> p

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_lower = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_upper = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_letter c = is_lower c || is_upper c
let is_ident_char c = is_digit c || is_letter c

let keywords =
  [ "if"
  ; "then"
  ; "else"
  ; "let"
  ; "rec"
  ; "true"
  ; "false"
  ; "match"
  ; "with"
  ; "in"
  ; "fun"
  ; "type"
  ; "int"
  ; "bool"
  ; "when"
  ; "function"
  ; "and"
  ]
;;

let is_keyword s = List.mem s keywords
let ( =?*> ) string parser = parse_stoken string *> parser
let ( =?>>| ) string value = parse_stoken string *> return value
(****************************************************** Consts ******************************************************)

let parse_int =
  parse_token (lift (fun hd -> CInt (int_of_string hd)) (take_while1 is_digit))
;;

let parse_bool =
  choice [ "true" =?>>| true; "false" =?>>| false ]
  >>= fun res_bool -> return @@ CBool res_bool
;;

let parse_unit = "(" =?*> (")" =?>>| CUnit)
let parse_const = choice [ parse_bool; parse_int; parse_unit ]
let parse_const_expr = parse_const >>| econst

(****************************************************** Operators ******************************************************)
let prohibited_ops = [ "|"; "->" ]
let first_unop_strings = [ "?"; "~"; "!" ]

let suffix_unop_strings =
  [ "$"; "&"; "*"; "+"; "-"; "/"; "="; ">"; "@"; "^"; "|"; "%"; "<" ]
;;

let base_unops = [ "-"; "+" ]

let first_binop_strings =
  [ "$"; "&"; "*"; "+"; "-"; "/"; "="; ">"; "@"; "^"; "|"; "%"; "<"; "#" ]
;;

let suffix_binop_strings =
  [ "$"
  ; "&"
  ; "*"
  ; "+"
  ; "-"
  ; "/"
  ; "="
  ; ">"
  ; "@"
  ; "^"
  ; "|"
  ; "%"
  ; "<"
  ; "!"
  ; "."
  ; ":"
  ; "?"
  ; "~"
  ]
;;

let base_binops = [ "+"; "-"; "*"; "/"; "<="; "<"; ">="; ">"; "=="; "!="; "&&"; "||" ]

let parse_op first suffix base_ops =
  let rec helper = function
    | hd :: tl -> hd =?*> return hd <|> helper tl
    | [] -> fail "Can't parse operator"
  in
  choice
    [ (let* first_string = helper first in
       let* suffix_string_list = many (helper suffix) in
       let res_string = String.concat "" (first_string :: suffix_string_list) in
       if List.mem res_string prohibited_ops
       then fail ("Can't use " ^ res_string ^ " as custom operator")
       else return res_string)
    ; helper base_ops
    ]
;;

let parse_unary_op = parse_op first_unop_strings suffix_unop_strings base_unops
let parse_binary_op = parse_op first_binop_strings suffix_binop_strings base_binops
let parse_op = parse_binary_op >>| iobinop <|> (parse_unary_op >>| iounop)

type priority_group =
  { group : string list
  ; left_associative : bool
  }

let first_priority_group =
  { group = [ "$"; "&"; "<"; "="; ">"; "|" ]; left_associative = true }
;;

let second_priority_group = { group = [ "@"; "^" ]; left_associative = false }
let third_priority_group = { group = [ "+"; "-" ]; left_associative = true }
let fourth_priority_group = { group = [ "%"; "*"; "/" ]; left_associative = true }
let fifth_priority_group = { group = [ "#" ]; left_associative = true }

(****************************************************** Tuple ******************************************************)

let parse_tuple ?(sep = ",") parser wrap =
  let* list_res = sep_by1 (parse_stoken sep) parser in
  match list_res with
  | [ _ ] -> fail "Tuple arity must be greater than 1"
  | _ -> return @@ wrap list_res
;;

let parse_tuple_expr parse_expr = parse_tuple parse_expr etuple

(****************************************************** Branching ******************************************************)

let parse_branching parse_expr =
  lift3
    eif
    ("if" =?*> parse_token1 parse_expr)
    ("then" =?*> parse_token1 parse_expr)
    ("else" =?*> parse_token1 parse_expr)
;;

(****************************************************** Application, unary, binary ops ******************************************************)

let chainl1 parse_e op =
  let rec go acc = lift2 (fun f x -> f acc x) op parse_e >>= go <|> return acc in
  parse_e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let binop_binder group =
  let* bin_op = parse_binary_op in
  let rec helper = function
    | group_string :: tl ->
      if String.starts_with ~prefix:group_string bin_op then return () else helper tl
    | [] -> fail "There is no matching operator"
  in
  let ebinop_helper x y = eapp (eapp (eid (iobinop bin_op)) x) y in
  helper group *> return ebinop_helper
;;

let get_chain e priority_group =
  let binop_binder = binop_binder priority_group.group in
  let chain =
    match priority_group.left_associative with
    | true -> chainl1
    | false -> chainr1
  in
  chain e binop_binder
;;

let rec parse_un_op_app parse_expr =
  let* unop = parse_token parse_unary_op >>| iounop in
  let* expr =
    choice
      [ parse_expr
      ; parse_parens (parse_un_op_app parse_expr)
      ; parse_space1 *> parse_un_op_app parse_expr
      ]
  in
  return @@ eapp (eid unop) expr
;;

let parse_bin_op_app parse_expr =
  fix (fun parse_bin_op_app ->
    let parse_bin_op parse_expr =
      let term = get_chain parse_expr fifth_priority_group in
      let term = get_chain term fourth_priority_group in
      let term = get_chain term third_priority_group in
      let term = get_chain term second_priority_group in
      get_chain (choice [ term; parse_bin_op_app ]) first_priority_group
    in
    let parse_app parse_expr =
      let term = chainl1 parse_expr (return eapp) in
      let term = choice [ parse_un_op_app term; term ] in
      let cons = "::" =?*> return elist in
      chainr1 term cons
    in
    parse_bin_op (parse_app parse_expr))
;;

(****************************************************** List ******************************************************)

let parse_list_semicolon parser wrap init =
  let* expr_list =
    choice
      [ "[" =?*> parse_stoken "]" *> return []
      ; "[" =?*> sep_by (parse_stoken ";") parser <* parse_stoken "]"
      ]
  in
  return (List.fold_right wrap expr_list init)
;;

let parse_list_expr parse_expr = parse_list_semicolon parse_expr elist (econst CNil)

(****************************************************** Patterns, types ******************************************************)
let parse_letters =
  lift2
    (fun hd tl -> String.make 1 hd ^ tl)
    (satisfy (fun ch -> ch = '_' || is_lower ch))
    (take_while (fun ch -> ch = '_' || is_ident_char ch))
;;

let parse_identifier =
  choice
    [ (parse_token parse_letters
       >>= fun ident ->
       if is_keyword ident then fail @@ ident ^ "? invalid syntax" else return ident)
    ; parse_parens (parse_binary_op <|> parse_unary_op)
    ]
;;

let parse_identifier_expr =
  let is_operator s =
    Base.String.exists ~f:(fun c -> not (is_ident_char c || c == '_')) s
  in
  let is_unop s =
    let rec helper = function
      | hd :: tl -> String.starts_with ~prefix:hd s || helper tl
      | [] -> false
    in
    helper first_unop_strings
  in
  let* ident = parse_identifier in
  return
    (eid
     @@
     match is_operator ident with
     | false -> ioident ident
     | true ->
       (match is_unop ident with
        | true -> iounop ident
        | false -> iobinop ident))
;;

let parse_ground_type =
  choice [ "int" =?>>| tint; "bool" =?>>| tbool; "unit" =?>>| tunit ]
;;

let parse_generic_type = "'" =?*> parse_letters >>| tvar
let parse_base_types = choice [ parse_generic_type; parse_ground_type ]
let parse_tuple_type parse_type = parse_tuple ~sep:"*" parse_type ttuple
let parse_arrow_type parse_type = lift2 tarrow parse_type ("->" =?*> parse_type)

let parse_list_type parse_type =
  let* typ = parse_type in
  "list" =?*> return @@ tlist typ
;;

let parse_type =
  fix (fun parse_type ->
    choice
      [ parse_parens @@ parse_arrow_type parse_type
      ; parse_parens @@ parse_tuple_type parse_type
      ; parse_parens @@ parse_list_type parse_type
      ; parse_parens_or_non parse_base_types
      ])
;;

let parse_get_explicit_typ =
  (let* parsed_typ = ":" =?*> parse_type in
   return (Some parsed_typ))
  <|> return None
;;

let parse_pat_typed parse_pat =
  let parse_pattern_typed parse_pat =
    lift2 (fun pat typ -> p_typed ~typ pat) parse_pat parse_get_explicit_typ
  in
  let parse_pat_not_typed parse_pat = lift p_typed parse_pat in
  choice [ parse_parens (parse_pattern_typed parse_pat); parse_pat_not_typed parse_pat ]
;;

let parse_ident_pat = parse_identifier >>| pid

let parse_list_pat_semicolon parse_pat =
  parse_list_semicolon
    (parse_pat_typed parse_pat)
    (fun hd tl -> p_typed @@ plist hd tl)
    (p_typed (pconst CNil))
  >>| fst
;;

let parse_list_pat_colons parse_pat =
  chainr1
    (parse_pat_typed parse_pat)
    ("::" =?*> return (fun hd tl -> p_typed (plist hd tl)))
  >>| fst
;;

let parse_tuple_pat parse_pat = parse_tuple (parse_pat_typed parse_pat) ptuple
let parse_base_pats = choice [ parse_ident_pat; parse_const >>| pconst ]

let parse_lrf_pats parse_pat =
  (* lrf === left-recursion free *)
  fix (fun parse_pat_fix -> choice [ parse_pat; parse_list_pat_semicolon parse_pat_fix ])
;;

let parse_pattern_typed =
  parse_pat_typed
  @@ fix (fun parse_pattern_fix ->
    choice
      [ parse_lrf_pats parse_base_pats
      ; parse_parens (parse_list_pat_colons parse_pattern_fix)
      ; parse_parens (parse_lrf_pats parse_pattern_fix)
      ; parse_parens (parse_tuple_pat parse_pattern_fix)
      ])
;;

(****************************************************** Functions ******************************************************)
let parse_params = sep_by parse_space parse_pattern_typed

let parse_fun_decl parse_expr =
  lift3
    (fun params typ expr -> List.fold_right efun params expr, typ)
    parse_params
    parse_get_explicit_typ
    ("=" =?*> parse_expr)
;;

let parse_fun_anon_expr parse_expr =
  "fun"
  =?*> parse_space1
       *> lift2
            (fun params expr -> List.fold_right efun params expr)
            parse_params
            ("->" =?*> parse_expr)
;;

(****************************************************** Pattern matching ******************************************************)

let parse_match parse_expr =
  "match"
  =?*>
  let parse_case =
    lift2 (fun case value -> case, value) parse_pattern_typed ("->" =?*> parse_expr)
  in
  lift2
    ematch
    (parse_token1 parse_expr
     <* (parse_stoken "with" <* choice [ parse_stoken1 "|"; parse_space ]))
    (sep_by1 (parse_stoken "|") parse_case)
;;

(****************************************************** Decl, expression parsing ******************************************************)
let parse_rec_flag = parse_next_stoken "rec" *> return Recursive <|> return Not_recursive

let parse_let_body parse_expr =
  lift2
    (fun ident (expr, typ) -> ident, expr, typ)
    (parse_token parse_identifier)
    (parse_fun_decl parse_expr)
;;

let parse_closure parse_decl parse_expr =
  lift2 (fun decl expr -> eclsr decl expr) parse_decl ("in" =?*> parse_expr)
;;

let parse_let_decl parse_expr =
  let* let_decl =
    "let"
    =?*> lift2
           (fun rec_flag (ident, expr, typ) -> rec_flag, ident, expr, typ)
           parse_rec_flag
           (parse_let_body parse_expr)
  in
  let* let_decls_mut = many ("and" =?*> parse_let_body parse_expr) in
  match let_decl, let_decls_mut with
  | (rec_flag, ident, expr, typ), [] -> return @@ dlet rec_flag ident expr typ
  | (rec_flag, ident, expr, typ), decls ->
    return (dletmut rec_flag ((ident, expr, typ) :: decls))
;;

let parse_expr =
  fix (fun parse_expr_fix ->
    let parse_expr_inner =
      choice
        [ parse_const_expr
        ; parse_identifier_expr
        ; parse_parens parse_expr_fix
        ; parse_fun_anon_expr parse_expr_fix
        ; parse_closure (parse_let_decl parse_expr_fix) parse_expr_fix
        ; parse_branching parse_expr_fix
        ; parse_match parse_expr_fix
        ; parse_list_expr parse_expr_fix
        ]
    in
    let parse_expr_inner = parse_bin_op_app parse_expr_inner in
    let parse_expr_inner =
      choice [ parse_tuple_expr parse_expr_inner; parse_expr_inner ]
    in
    choice ~failure_msg:"Can't parse expr" [ parse_expr_inner ])
;;

let parse_decls =
  lift prog (many (parse_let_decl parse_expr <* (parse_stoken ";;" <|> parse_space)))
;;

let parse_program s =
  match Angstrom.parse_string ~consume:Consume.All parse_decls s with
  | Ok v -> Ok v
  | Error msg ->
    (match msg with
     | ": end_of_input" -> Error "Syntax error"
     | _ -> Error msg)
;;
