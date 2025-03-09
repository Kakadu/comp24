(** Copyright 2024-2025, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Pp_utils
open Format

let pp_const fmt = function
  | CInt a -> fprintf fmt "%d" a
  | CBool a -> fprintf fmt "%b" a
  | CNil -> fprintf fmt "[]"
  | CUnit -> fprintf fmt "()"
;;

let pp_ground_typ = pp_ground

let pp_ident_definable fmt = function
  | IdentLetters s -> fprintf fmt "%s" s
  | IdentOp s -> fprintf fmt "( %s )" s
;;

let pp_ident fmt = function
  | IdentOfDefinable id -> pp_ident_definable fmt id
  | IdentOfBaseOp bop ->
    fprintf
      fmt
      "%s"
      ((function
         | Plus -> "+"
         | Minus -> "-")
         bop)
;;

let rec pp_typ fmt typ =
  let arrow = function
    | TArr _ -> format_of_string "(%a)"
    | _ -> format_of_string "%a"
  in
  match typ with
  | TGround x ->
    (match x with
     | GInt -> fprintf fmt "int"
     | GBool -> fprintf fmt "bool"
     | GUnit -> fprintf fmt "Unit")
  | TVar a -> fprintf fmt "'%s" a
  | TTuple (v1, v2, value_list) ->
    pp_tuple
      (function
        | TTuple _ -> true
        | _ -> false)
      pp_typ
      " * "
      fmt
      (v1 :: v2 :: value_list)
  | TList typ ->
    fprintf
      fmt
      ((match typ with
        | TGround _ | TVar _ | TList _ -> "%a"
        | _ -> "(%a)")
       ^^ " list")
      pp_typ
      typ
  | TArr (typ_left, typ_right) ->
    fprintf fmt (arrow typ_left ^^ " -> %a") pp_typ typ_left pp_typ typ_right
;;

let pp_typed fmt pp_v (v, typ) = fprintf fmt "(%a : %a)" pp_v v pp_typ typ

let rec pp_pattern fmt = function
  | PId id -> fprintf fmt "%s" id
  | PConst c -> fprintf fmt "%a" pp_const c
  | PTuple (p1, p2, tuple) ->
    fprintf
      fmt
      "(%a)"
      (pp_tuple
         (function
           | PList _ -> true
           | _ -> false)
         pp_pattern
         ", ")
      (p1 :: p2 :: tuple)
  | PList (left, right) -> fprintf fmt "%a :: %a" pp_pattern left pp_pattern right
  | PConstraint (p, typ) -> pp_typed fmt pp_pattern (p, typ)
;;

let rec pp_pattern_or_op fmt = function
  | POpPat pat -> fprintf fmt "%a" pp_pattern pat
  | POpOp op -> fprintf fmt "( %s )" op
  | POrOpConstraint (pat, typ) -> pp_typed fmt pp_pattern_or_op (pat, typ)
;;

let pp_rec_flag fmt = function
  | Recursive -> fprintf fmt " rec"
  | Not_recursive -> fprintf fmt ""
;;

let rec pp_expr fmt = function
  | EConst c -> pp_const fmt c
  | EId s -> pp_ident fmt s
  | EFun (pattern, expr) -> fprintf fmt "(fun %a -> %a)" pp_pattern pattern pp_expr expr
  | EApp (EApp (EId (IdentOfDefinable (IdentOp bin_op)), op1), op2) ->
    (* bin op pattern *)
    fprintf fmt "(%a %s %a)" pp_expr op1 bin_op pp_expr op2
  | EApp (e1, EApp (EApp (EId (IdentOfDefinable (IdentOp bin_op)), op1), op2)) ->
    fprintf fmt "%a (%a %s %a)" pp_expr e1 pp_expr op1 bin_op pp_expr op2
  | EApp (e1, e2) -> fprintf fmt "(%a %a)" pp_expr e1 pp_expr e2
  | EIf (e_if, e_th, e_el) ->
    fprintf fmt "if %a then %a else %a" pp_expr e_if pp_expr e_th pp_expr e_el
  | EList (hd, tl) -> fprintf fmt "%a :: %a" pp_expr hd pp_expr tl
  | ETuple (e1, e2, es) ->
    fprintf
      fmt
      "(%a)"
      (pp_tuple
         (function
           | ETuple _ | EList (_, _) -> true
           | _ -> false)
         pp_expr
         ", ")
      (e1 :: e2 :: es)
  | EClsr (decl, expr) -> fprintf fmt "%a\nin %a" pp_decl decl pp_expr expr
  | EMatch (expr, case1, cases) ->
    let pp_case fmt (pat_typed, expr) =
      fprintf fmt "| %a -> %a" pp_pattern pat_typed pp_expr expr
    in
    let pp_cases = pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "\n") pp_case in
    fprintf fmt "match %a with\n%a" pp_expr expr pp_cases (case1 :: cases)
  | EConstraint (e, typ) -> pp_typed fmt pp_expr (e, typ)

and pp_let_expr fmt expr =
  let rec extract_fun_pats acc = function
    | EFun (pat_typed, expr) -> extract_fun_pats (pat_typed :: acc) expr
    | e -> List.rev acc, e
  in
  let pats, expr_fun_body = extract_fun_pats [] expr in
  let is_pat_with_parens = function
    | PConst _ | PId _ | PTuple _ | PConstraint _ -> false
    | _ -> true
  in
  let pp_pattern_typed fmt pat =
    let to_print = format_of_string @@ if is_pat_with_parens pat then "(%a)" else "%a" in
    fprintf fmt to_print pp_pattern pat
  in
  let pp_pats_typed =
    pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt " ") pp_pattern_typed
  in
  let to_print =
    format_of_string (if List.length pats > 0 then "%a = %a" else "%a= %a")
  in
  match expr with
  | EConstraint (_, _) -> fprintf fmt to_print pp_pats_typed pats pp_expr expr
  | _ -> fprintf fmt to_print pp_pats_typed pats pp_expr expr_fun_body

and pp_decl fmt =
  let pp_let_body fmt (pat_or_op, expr) =
    fprintf fmt "%a %a" pp_pattern_or_op pat_or_op pp_let_expr expr
  in
  function
  | DLet (rec_flag, (pat_or_op, expr)) ->
    fprintf fmt "let%a %a" pp_rec_flag rec_flag pp_let_body (pat_or_op, expr)
  | DLetMut (rec_flag, decl1, decl2, decls) ->
    let pp_decls =
      pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "\nand ") pp_let_body
    in
    fprintf fmt "let%a %a" pp_rec_flag rec_flag pp_decls (decl1 :: decl2 :: decls)
;;

let pp_prog = pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt ";;\n") pp_decl
