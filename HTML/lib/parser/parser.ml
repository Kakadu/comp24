(** Copyright 2024-2025, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open AstLib.Ast

let is_space = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false
;;

let pe_space = take_while is_space
let pe_space1 = take_while1 is_space
let pe_token p = pe_space *> p
let pe_token1 p = pe_space1 *> p
let pe_stoken s = pe_token @@ string s
let pe_stoken1 s = pe_token1 @@ string s
let pe_next_stoken p = pe_stoken1 p <* pe_space1
let pe_parens p = pe_stoken "(" *> p <* pe_stoken ")"
let pe_parens_or_non p = pe_parens p <|> p

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
let ( =?*> ) string pe = pe_stoken string *> pe
let ( =?>>| ) string value = pe_stoken string *> return value

(****************************************************** Consts ******************************************************)

let pe_int = pe_token (lift (fun hd -> CInt (int_of_string hd)) (take_while1 is_digit))

let pe_bool =
  choice [ "true" =?>>| true; "false" =?>>| false ]
  >>= fun res_bool -> return @@ CBool res_bool
;;

let pe_unit = "(" =?*> (")" =?>>| CUnit)
let pe_const = choice [ pe_bool; pe_int; pe_unit ]
let pe_const_expr = pe_const >>| econst

(****************************************************** Operators ******************************************************)

open Common.Ops

let pe_op first suffix base_ops =
  let rec helper = function
    | hd :: tl -> hd =?*> return hd <|> helper tl
    | [] -> fail "Can't pe operator"
  in
  choice
    [ (let* first_string = helper first in
       let* suffix_string_list = many (helper suffix) in
       let res_string = String.concat "" (first_string :: suffix_string_list) in
       if List.mem res_string prohibited_ops
       then
         fail
           (let strings = [ "Can't use "; res_string; " as custom operator" ] in
            String.concat "" strings)
       else return res_string)
    ; helper base_ops
    ]
;;

let pe_unary_op = pe_op first_unop_strings suffix_unop_strings base_unops
let pe_binary_op = pe_op first_binop_strings suffix_binop_strings base_binops

let pe_any_op =
  pe_op
    (first_unop_strings @ first_binop_strings)
    (suffix_unop_strings @ suffix_binop_strings)
    base_binops
;;

let convert_op_to_ident = function
  | op when op = "+" || op = "-" -> ident_of_base_op (if op = "+" then Plus else Minus)
  | op -> op |> ident_op |> ident_of_definable
;;

let pe_op = pe_binary_op <|> pe_unary_op

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

let pe_assert_token_is_not_ahead token =
  pe_space
  *>
  let* next = pe_space *> choice [ peek_string (String.length token); return "" ] in
  return (next = token)
;;

(****************************************************** Tuple ******************************************************)

let pe_tuple ?(delim = ",") pe wrap =
  let* list_res = sep_by1 (pe_stoken delim) pe in
  match list_res with
  | v1 :: v2 :: tl -> return (wrap v1 v2 tl)
  | _ -> fail "Tuple arity must be greater than 1"
;;

let pe_value_or_tuple ?(delim = ",") pe_tuple parser =
  (* speed up hack moment *)
  choice
    [ (let* v = parser in
       let* is_delim_ahead = pe_assert_token_is_not_ahead delim in
       if is_delim_ahead then fail "Rollback" else return v)
    ; pe_tuple parser
    ]
;;

(****************************************************** List ******************************************************)

let pe_list_semicolon pe wrap init =
  let* expr_list =
    choice
      [ "[" =?*> pe_stoken "]" *> return []
      ; "[" =?*> sep_by (pe_stoken ";") pe <* pe_stoken "]"
      ]
  in
  return (List.fold_right wrap expr_list init)
;;

(****************************************************** Chains ******************************************************)

let chainl1 pe op =
  let rec go acc = lift2 (fun f x -> f acc x) op pe >>= go <|> return acc in
  pe >>= go
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

(****************************************************** Patterns, types ******************************************************)
let pe_letters =
  lift2
    (fun hd tl -> String.make 1 hd ^ tl)
    (satisfy (fun ch -> ch = '_' || is_lower ch))
    (take_while (fun ch -> ch = '_' || is_ident_char ch))
;;

let pe_identifier_letters =
  pe_token pe_letters
  >>= fun ident ->
  if is_keyword ident then fail @@ ident ^ "? invalid syntax" else return ident
;;

let pe_identifier_definable =
  choice [ pe_identifier_letters >>| ident_letters; pe_parens pe_any_op >>| ident_op ]
;;

let pe_identifier_expr = pe_identifier_definable >>| ident_of_definable >>| eid
let pe_base_expr = choice [ pe_const_expr; pe_identifier_expr ]
let pe_ground_type = choice [ "int" =?>>| tint; "bool" =?>>| tbool; "unit" =?>>| tunit ]
let pe_generic_type = "'" =?*> pe_letters >>| tvar
let pe_base_types = choice [ pe_generic_type; pe_ground_type ]
let pe_tuple_type pe_type = pe_tuple ~delim:"*" pe_type ttuple
let pe_value_or_tuple_type = pe_value_or_tuple ~delim:"*" pe_tuple_type
let pe_arrow_type pe_type = lift2 tarrow pe_type ("->" =?*> pe_type)

let pe_list_type pe_type =
  let* typ = pe_type in
  "list" =?*> return @@ tlist typ
;;

let pe_type =
  fix (fun pe_type ->
    choice
      [ pe_parens @@ pe_arrow_type pe_type
      ; pe_parens @@ pe_value_or_tuple_type pe_type
      ; pe_parens @@ pe_list_type pe_type
      ; pe_parens_or_non pe_base_types
      ])
;;

let pe_get_explicit_typ =
  let delim = ":" in
  let* is_there_delim_ahead =
    choice [ pe_assert_token_is_not_ahead delim; return false ]
  in
  if is_there_delim_ahead
  then
    choice
      [ (let* typ = delim =?*> pe_type in
         return (Some typ))
      ; return None
      ]
  else return None
;;

let pe_typed cons pe =
  let pe_typed = lift2 cons pe pe_get_explicit_typ in
  pe_typed <|> pe_parens pe_typed
;;

let cons_pat_typed res typ = p_typed res ~typ
let pe_typed_pat = pe_typed cons_pat_typed

let rollback_not_list pe =
  let* pat = pe in
  match pat with
  | PList (_, _) | PConst CNil -> return pat
  | _ -> fail "Rollback"
;;

let pe_list_pat_semicolon pe_pat =
  rollback_not_list
  @@ pe_list_semicolon (pe_typed_pat pe_pat) (fun hd tl -> plist hd tl) (pconst CNil)
;;

let delim_list_pat_colons = "::"

let pe_list_pat_colons pe_pat =
  rollback_not_list
  @@ chainr1
       (pe_typed_pat pe_pat)
       (delim_list_pat_colons =?*> return (fun hd tl -> p_typed (plist hd tl)))
;;

let pe_base_pat = choice [ pe_identifier_letters >>| pid; pe_const >>| pconst ]

let pe_lrf_pats pe_pat =
  (* lrf === left-recursion free *)
  choice [ pe_list_pat_semicolon pe_pat; pe_base_pat; pe_pat ]
;;

let pe_tuple_pat pe_pat = pe_tuple (pe_typed_pat pe_pat) ptuple
let pe_value_or_tuple_pat = pe_value_or_tuple pe_tuple_pat

let pe_pattern =
  fix (fun pe_pattern_fix ->
    let term =
      choice
        [ pe_lrf_pats pe_base_pat
        ; pe_parens (pe_list_pat_colons pe_pattern_fix)
        ; pe_parens (pe_lrf_pats pe_pattern_fix)
        ; pe_parens (pe_tuple_pat pe_pattern_fix)
        ]
    in
    choice [ pe_list_pat_colons term; pe_tuple_pat term; term ])
;;

let pe_pattern_typed = pe_typed cons_pat_typed pe_pattern

(****************************************************** Tuple ******************************************************)
let cons_expr_typed res typ = e_typed res ~typ
let pe_typed_expr = pe_typed cons_expr_typed
let pe_tuple_expr pe_expr = pe_tuple (pe_typed_expr pe_expr) etuple
let pe_value_or_tuple_expr = pe_value_or_tuple pe_tuple_expr

(****************************************************** Branching ******************************************************)

let pe_branching pe_expr =
  let pe_typed = pe_typed_expr pe_expr in
  lift3
    eif
    ("if" =?*> pe_token1 pe_typed)
    ("then" =?*> pe_token1 pe_typed)
    ("else" =?*> pe_token1 pe_typed)
;;

(****************************************************** Application, unary, binary ops ******************************************************)

let get_typed_ebinop_applier op =
  let cast_str_ident_op op = eid (op |> ident_op |> ident_of_definable) in
  let typed_ebinop_applier x y =
    e_typed @@ eapp (e_typed (eapp (e_typed (cast_str_ident_op op)) x)) y
  in
  typed_ebinop_applier
;;

let binop_binder group =
  let* op = pe_binary_op in
  let rec helper bin_op_string = function
    | group_string :: _ when String.starts_with ~prefix:group_string bin_op_string ->
      return ()
    | _ :: tl -> helper bin_op_string tl
    | [] -> fail "There is no matching operator"
  in
  helper op group *> return (get_typed_ebinop_applier op)
;;

let get_chain e priority_group =
  let binop_binder = binop_binder priority_group.group in
  let chain = if priority_group.left_associative then chainl1 else chainr1 in
  chain e binop_binder
;;

let pe_typed_if_in_parens pe = pe_parens (pe_typed_expr pe) <|> pe

let rollback_not_app pe =
  let* e = pe in
  match e with
  | EConstraint (_, _) -> fail "Rollback"
  | _ -> return e
;;

let pe_bin_op_app pe_expr =
  let delim_list_expr_colons = "::" in
  fix (fun pe_bin_op_app ->
    let pe_bin_op pe_expr =
      let term = get_chain pe_expr fifth_priority_group in
      let term = get_chain term fourth_priority_group in
      let term = get_chain term third_priority_group in
      let term = get_chain term second_priority_group in
      get_chain term first_priority_group
    in
    let rec pe_un_op_app pe_expr =
      let* unop = pe_token pe_unary_op >>| convert_op_to_ident in
      let* expr_typed = choice [ pe_space1 *> pe_un_op_app pe_expr; pe_expr ] in
      return @@ e_typed @@ eapp (e_typed (eid unop)) expr_typed
    in
    let pe_app pe_expr =
      let term = chainl1 pe_expr (return (fun x y -> eapp (e_typed x) (e_typed y))) in
      let term =
        chainl1 (pe_typed_if_in_parens term) (return (fun x y -> e_typed @@ eapp x y))
      in
      let term = choice [ pe_un_op_app term; term ] in
      let cons = delim_list_expr_colons =?*> return (fun x y -> e_typed @@ elist x y) in
      chainr1 term cons
    in
    let pe_bin_op_parens =
      let* op = pe_parens pe_op in
      let pe_expr = pe_typed_if_in_parens pe_bin_op_app in
      lift2 (get_typed_ebinop_applier op) pe_expr pe_expr
    in
    rollback_not_app @@ pe_bin_op (choice [ pe_bin_op_parens; pe_app pe_expr ]))
;;

(****************************************************** List ******************************************************)

let pe_list_expr_semicolon pe_expr =
  pe_list_semicolon (pe_typed_expr pe_expr) elist (econst CNil)
;;

(****************************************************** Functions ******************************************************)
let pe_params = sep_by pe_space pe_pattern_typed

let pe_fun_let_decl (pe_expr : expr t) =
  let delim_fun_decl = "=" in
  lift3
    (fun params decl_typ expr ->
      match expr with
      | EConstraint (expr, typ) ->
        let right_side = List.fold_right efun params expr in
        decl_typ, e_typed right_side ~typ:(Some typ)
      | expr ->
        let right_side = List.fold_right efun params expr in
        decl_typ, right_side)
    pe_params
    (pe_get_explicit_typ <* pe_stoken delim_fun_decl)
    (pe_typed_expr pe_expr)
;;

let pe_fun_anon_expr pe_expr =
  let prefix_fun = "fun" in
  let delim_fun = "->" in
  prefix_fun
  =?*> pe_space1
       *> lift2
            (fun params expr ->
              let right_side = List.fold_right efun params expr in
              right_side)
            (pe_params <* pe_stoken delim_fun)
            (pe_typed_expr pe_expr)
;;

(****************************************************** Pattern matching ******************************************************)

let pe_match pe_expr =
  let pe_typed = pe_typed_expr pe_expr in
  "match"
  =?*>
  let pe_case =
    lift2 (fun case value -> case, value) pe_pattern_typed ("->" =?*> pe_typed)
  in
  let* expr =
    pe_token1 pe_typed <* (pe_stoken "with" <* choice [ pe_stoken1 "|"; pe_space ])
  in
  let* cases = sep_by1 (pe_stoken "|") pe_case in
  match cases with
  | [] -> fail "Pattern matching with 0 patterns?!"
  | hd :: tl -> return @@ ematch expr hd tl
;;

(****************************************************** Decl, expression parsing ******************************************************)
let pe_rec_flag = pe_next_stoken "rec" *> return Recursive <|> return Not_recursive

let pe_let_body pe_expr =
  lift2
    (fun pat (pat_typ, expr) -> pop_typed ~typ:pat_typ pat, expr)
    (pe_token
       (choice
          [ pe_parens_or_non pe_pattern >>| pop_pat
          ; pe_parens (pe_binary_op <|> pe_unary_op) >>| pop_op
          ]))
    (pe_fun_let_decl pe_expr)
;;

let pe_closure pe_decl pe_expr = lift2 eclsr pe_decl ("in" =?*> pe_typed_expr pe_expr)

let pe_let_decl pe_expr =
  let pe_let_body_typed = lift (fun (pat, expr) -> pat, expr) (pe_let_body pe_expr) in
  let* let_decl =
    "let"
    =?*> lift2 (fun rec_flag let_body -> rec_flag, let_body) pe_rec_flag pe_let_body_typed
  in
  let* let_decls_mut = many ("and" =?*> pe_let_body_typed) in
  match let_decl, let_decls_mut with
  | (rec_flag, (pat_or_op, expr)), [] -> return @@ dlet rec_flag (pat_or_op, expr)
  | ( (rec_flag, (pat_or_op_typed1, expr_typed1))
    , (pat_or_op_typed2, expr_typed2) :: tl_decls ) ->
    return
      (dletmut
         rec_flag
         (pat_or_op_typed1, expr_typed1)
         (pat_or_op_typed2, expr_typed2)
         tl_decls)
;;

let pe_expr =
  fix (fun pe_expr_fix ->
    let pe_expr_inner =
      choice
        [ pe_base_expr
        ; pe_parens pe_expr_fix
        ; pe_fun_anon_expr pe_expr_fix
        ; pe_closure (pe_let_decl pe_expr_fix) pe_expr_fix
        ; pe_branching pe_expr_fix
        ; pe_match pe_expr_fix
        ; pe_list_expr_semicolon pe_expr_fix
        ]
    in
    let pe_expr_inner = choice [ pe_bin_op_app pe_expr_inner; pe_expr_inner ] in
    let pe_expr_inner = pe_value_or_tuple_expr pe_expr_inner in
    choice ~failure_msg:"Can't parse expr" [ pe_expr_inner ])
;;

let pe_decls =
  lift
    prog
    (many (pe_let_decl pe_expr <* choice [ pe_stoken ";;"; peek_string 0 ]) <* pe_space)
;;

let parse_program s =
  match Angstrom.parse_string ~consume:Consume.All pe_decls s with
  | Ok v -> Ok v
  | Error msg ->
    (match msg with
     | ": end_of_input" -> Error "Syntax error"
     | _ -> Error msg)
;;
