(** Copyright 2024-2025, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open AstLib.Ast

let ( let+ ) = ( >>| )
let ( let* ) = ( >>= )

let is_space = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false
;;

let debug = false
let print_endline s = if debug then print_endline s else ()
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

open Common.Ops

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

let parse_any_op =
  parse_op
    (first_unop_strings @ first_binop_strings)
    (suffix_unop_strings @ suffix_binop_strings)
    base_binops
;;

let convert_op_to_ident = function
  | op when op = "+" || op = "-" -> ident_of_base_op (if op = "+" then Plus else Minus)
  | op -> op |> ident_op |> ident_of_definable
;;

let parse_op =
  let parse_bin_or_un_op = parse_binary_op <|> parse_unary_op in
  let parse_base_op = parse_bin_or_un_op >>| convert_op_to_ident in
  parse_base_op
;;

let parse_op = parse_binary_op <|> parse_unary_op

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
  | [ _ ] | [] -> fail "Tuple arity must be greater than 1"
  | e1 :: e2 :: tl -> return (wrap e1 e2 tl)
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

(****************************************************** Chains ******************************************************)

let chainl1 parse_e op =
  let rec go acc = lift2 (fun f x -> f acc x) op parse_e >>= go <|> return acc in
  parse_e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

(****************************************************** Patterns, types ******************************************************)
let parse_letters =
  lift2
    (fun hd tl -> String.make 1 hd ^ tl)
    (satisfy (fun ch -> ch = '_' || is_lower ch))
    (take_while (fun ch -> ch = '_' || is_ident_char ch))
;;

let parse_identifier_letters =
  parse_token parse_letters
  >>= fun ident ->
  if is_keyword ident then fail @@ ident ^ "? invalid syntax" else return ident
;;

let parse_identifier_definable =
  choice
    [ parse_identifier_letters >>| ident_letters; parse_parens parse_any_op >>| ident_op ]
;;

let parse_identifier_expr = parse_identifier_definable >>| ident_of_definable >>| eid

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

let parse_pattern_typed parse_pat =
  let parse_pattern_typed parse_pat =
    lift2 (fun pat typ -> p_typed ~typ pat) parse_pat parse_get_explicit_typ
  in
  let parse_pat_not_typed parse_pat = lift p_typed parse_pat in
  choice [ parse_parens (parse_pattern_typed parse_pat); parse_pat_not_typed parse_pat ]
;;

let parse_ident_pat = parse_identifier_letters >>| pid

let parse_list_pat_semicolon parse_pat =
  parse_list_semicolon
    (parse_pattern_typed parse_pat)
    (fun hd tl -> p_typed @@ plist hd tl)
    (p_typed (pconst CNil))
  >>| fst
;;

let parse_list_pat_colons parse_pat =
  chainr1
    (parse_pattern_typed parse_pat)
    ("::" =?*> return (fun hd tl -> p_typed (plist hd tl)))
  >>| fst
;;

let parse_tuple_pat parse_pat = parse_tuple (parse_pattern_typed parse_pat) ptuple
let parse_base_pats = choice [ parse_ident_pat; parse_const >>| pconst ]

let parse_lrf_pats parse_pat =
  (* lrf === left-recursion free *)
  fix (fun parse_pat_fix -> choice [ parse_pat; parse_list_pat_semicolon parse_pat_fix ])
;;

let parse_pattern =
  fix (fun parse_pattern_fix ->
    choice
      [ parse_lrf_pats parse_base_pats
      ; parse_parens (parse_list_pat_colons parse_pattern_fix)
      ; parse_parens (parse_lrf_pats parse_pattern_fix)
      ; parse_parens (parse_tuple_pat parse_pattern_fix)
      ])
;;

let parse_pattern_typed = parse_pattern_typed parse_pattern

let parse_expr_typed parse_expr =
  let _ = print_endline "parse_expr_typed" in
  let let_and_expr_delim = ":" in
  let parse_expr_typed is_in_parens =
    lift2
      (fun expr typ -> expr, typ)
      parse_expr
      (parse_space
       *>
       let* is_there_delim_ahead =
         if is_in_parens
         then
           choice
             [ (let* next = peek_string 1 in
                return (next = let_and_expr_delim))
             ; return false
             ]
         else return false
       in
       match is_there_delim_ahead && is_in_parens with
       | true ->
         let* typ = let_and_expr_delim =?*> parse_type in
         return (Some typ)
       | false -> return None)
  in
  parse_expr_typed false <|> parse_parens (parse_expr_typed true)
;;

let parse_get_rid_of_typ parse = parse >>| fst

(****************************************************** Tuple ******************************************************)

let parse_tuple_expr parse_expr = parse_tuple (parse_expr_typed parse_expr) etuple

(****************************************************** Branching ******************************************************)

let parse_branching parse_expr =
  let parse_expr_typed = parse_expr_typed parse_expr in
  lift3
    eif
    ("if" =?*> parse_token1 parse_expr_typed)
    ("then" =?*> parse_token1 parse_expr_typed)
    ("else" =?*> parse_token1 parse_expr_typed)
;;

(****************************************************** Application, unary, binary ops ******************************************************)

let binop_binder group =
  let* bin_op_string = parse_binary_op in
  let rec helper = function
    | group_string :: tl ->
      if String.starts_with ~prefix:group_string bin_op_string
      then return ()
      else helper tl
    | [] -> fail "There is no matching operator"
  in
  let ebinop_helper x y =
    e_typed
    @@ eapp
         (e_typed
            (eapp (e_typed (eid (bin_op_string |> ident_op |> ident_of_definable))) x))
         y
  in
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

let rec parse_un_op_app (parse_expr : expr t) =
  let* unop = parse_token parse_unary_op >>| convert_op_to_ident in
  let* expr_typed =
    choice
      [ parse_expr; parse_parens parse_expr; parse_space1 *> parse_un_op_app parse_expr ]
  in
  return (eapp (e_typed (eid unop)) (e_typed expr_typed))
;;

let parse_bin_op_app parse_expr =
  fix (fun parse_bin_op_app ->
    let _ = print_endline "parse_bin_op_app" in
    let parse_bin_op parse_expr =
      let _ = print_endline "parse_bin_op" in
      let term = get_chain (parse_expr_typed parse_expr) fifth_priority_group in
      let term = get_chain term fourth_priority_group in
      let term = get_chain term third_priority_group in
      let term = get_chain term second_priority_group in
      parse_get_rid_of_typ
      @@ get_chain
           (choice [ term; parse_parens (parse_expr_typed parse_bin_op_app) ])
           first_priority_group
    in
    let parse_app parse_expr =
      let _ = print_endline "parse_app" in
      let term =
        parse_get_rid_of_typ
        @@ chainl1 (parse_expr_typed parse_expr) (return @@ fun x y -> e_typed (eapp x y))
      in
      let term = choice [ parse_un_op_app term; term ] in
      let cons = "::" =?*> return (fun x y -> e_typed (elist x y)) in
      parse_get_rid_of_typ @@ chainr1 (parse_expr_typed term) cons
    in
    let parse_bin_op_parens =
      let _ = print_endline "parse_bin_op_parens" in
      let* op = parse_parens parse_op in
      let parse_expr =
        parse_expr_typed
          (choice
             [ parse_const_expr; parse_identifier_expr; parse_parens parse_bin_op_app ])
      in
      lift2
        (fun e1 e2 ->
          eapp
            (e_typed (eapp (e_typed (eid (op |> ident_op |> ident_of_definable))) e1))
            e2)
        parse_expr
        parse_expr
    in
    parse_bin_op @@ choice [ parse_bin_op_parens; parse_app parse_expr ])
;;

(****************************************************** Functions ******************************************************)

let parse_list_expr parse_expr =
  parse_get_rid_of_typ
  @@ parse_list_semicolon
       (parse_expr_typed parse_expr)
       (fun x y -> e_typed @@ elist x y)
       (e_typed @@ econst CNil)
;;

(****************************************************** Functions ******************************************************)
let parse_params = sep_by parse_space parse_pattern_typed

let parse_fun_decl parse_expr =
  let fun_decl_delim = "=" in
  lift3
    (fun params decl_typ expr_typed ->
      let right_side, _ =
        (* TODO: handle case with empty params *)
        List.fold_right (fun x y -> e_typed @@ efun x y) params expr_typed
      in
      decl_typ, right_side)
    parse_params
    (parse_get_explicit_typ <* parse_stoken fun_decl_delim)
    (parse_expr_typed parse_expr)
;;

let parse_fun_anon_expr parse_expr =
  let _ = print_endline "parse_fun_anon_expr" in
  let fun_prefix = "fun" in
  let fun_delim = "->" in
  fun_prefix
  =?*> parse_space1
       *> lift2
            (fun params expr_typed ->
              let right_side, _ =
                List.fold_right (fun x y -> e_typed @@ efun x y) params expr_typed
              in
              right_side)
            (parse_params <* parse_stoken fun_delim)
            (parse_expr_typed parse_expr)
;;

(****************************************************** Pattern matching ******************************************************)

let parse_match parse_expr =
  let _ = print_endline "parse_match" in
  let parse_expr_typed = parse_expr_typed parse_expr in
  "match"
  =?*>
  let parse_case =
    lift2 (fun case value -> case, value) parse_pattern_typed ("->" =?*> parse_expr_typed)
  in
  let* expr =
    parse_token1 parse_expr_typed
    <* (parse_stoken "with" <* choice [ parse_stoken1 "|"; parse_space ])
  in
  let* cases = sep_by1 (parse_stoken "|") parse_case in
  match cases with
  | [] -> fail "Pattern matching with 0 patterns?!"
  | hd :: tl -> return @@ ematch expr hd tl
;;

(****************************************************** Decl, expression parsing ******************************************************)
let parse_rec_flag = parse_next_stoken "rec" *> return Recursive <|> return Not_recursive

let parse_let_body parse_expr =
  let _ = print_endline "parse_let_body" in
  lift2
    (fun pat (pat_typ, expr) -> pat, pat_typ, expr)
    (parse_token
       (choice
          [ parse_parens_or_non parse_pattern >>| pop_pat
          ; parse_parens (parse_binary_op <|> parse_unary_op) >>| pop_op
          ]))
    (parse_fun_decl parse_expr)
;;

let parse_closure parse_decl parse_expr =
  lift2 eclsr parse_decl ("in" =?*> parse_expr_typed parse_expr)
;;

let parse_let_decl parse_expr =
  let parse_let_body_typed =
    lift
      (fun ((pat, pat_typ, expr), expr_typ) -> (pat, pat_typ), (expr, expr_typ))
      (parse_expr_typed (parse_let_body parse_expr))
  in
  let* let_decl =
    "let"
    =?*> lift2
           (fun rec_flag let_body -> rec_flag, let_body)
           parse_rec_flag
           parse_let_body_typed
  in
  let* let_decls_mut = many ("and" =?*> parse_let_body_typed) in
  match let_decl, let_decls_mut with
  | (rec_flag, (pat_or_op_typed, expr_typed)), [] ->
    return @@ dlet rec_flag (pat_or_op_typed, expr_typed)
  | ( (rec_flag, (pat_or_op_typed1, expr_typed1))
    , (pat_or_op_typed2, expr_typed2) :: tl_decls ) ->
    return
      (dletmut
         rec_flag
         (pat_or_op_typed1, expr_typed1)
         (pat_or_op_typed2, expr_typed2)
         tl_decls)
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
  lift
    prog
    (many
       (parse_let_decl parse_expr
        <* choice [ parse_stoken ";;"; peek_string 0 ]
        <* parse_space))
;;

let parse_program s =
  match Angstrom.parse_string ~consume:Consume.All parse_decls s with
  | Ok v -> Ok v
  | Error msg ->
    (match msg with
     | ": end_of_input" -> Error "Syntax error"
     | _ -> Error msg)
;;
