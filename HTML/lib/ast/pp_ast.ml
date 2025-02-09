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

let rec pp_pattern fmt = function
  | PId id -> fprintf fmt "%s" id
  | PConst c -> fprintf fmt "%a" pp_const c
  | PTuple (p1, p2, tuple) ->
    fprintf
      fmt
      "(%a)"
      (pp_tuple
         (function
           | PList _, _ -> true
           | _ -> false)
         pp_pattern_typed
         ", ")
      (p1 :: p2 :: tuple)
  | PList (left, right) ->
    fprintf fmt "%a :: %a" pp_pattern_typed left pp_pattern_typed right

and pp_pattern_typed fmt = function
  | pattern, Some typ -> fprintf fmt "(%a : %a)" pp_pattern pattern pp_typ typ
  | pattern, None -> fprintf fmt "%a" pp_pattern pattern
;;

let pp_pattern_or_op fmt = function
  | POpPat pat -> fprintf fmt "%a" pp_pattern pat
  | POpOp op -> fprintf fmt "( %s )" op
;;

let pp_pattern_or_op_typed fmt = function
  | pattern_or_op, Some typ ->
    fprintf fmt "(%a : %a)" pp_pattern_or_op pattern_or_op pp_typ typ
  | pattern_or_op, None -> fprintf fmt "%a" pp_pattern_or_op pattern_or_op
;;

let pp_rec_flag fmt = function
  | Recursive -> fprintf fmt " rec"
  | Not_recursive -> fprintf fmt ""
;;

let rec pp_expr fmt = function
  | EConst c -> pp_const fmt c
  | EId s -> pp_ident fmt s
  | EFun (pattern_typed, expr) ->
    fprintf fmt "(fun %a -> %a)" pp_pattern_typed pattern_typed pp_expr_typed expr
  | EApp ((EApp ((EId (IdentOfDefinable (IdentOp bin_op)), None), op1), None), op2) ->
    (* bin op pattern *)
    fprintf fmt "(%a %s %a)" pp_expr_typed op1 bin_op pp_expr_typed op2
  | EApp
      ( e1
      , ( EApp ((EApp ((EId (IdentOfDefinable (IdentOp bin_op)), None), op1), None), op2)
        , None ) ) ->
    fprintf
      fmt
      "%a (%a %s %a)"
      pp_expr_typed
      e1
      pp_expr_typed
      op1
      bin_op
      pp_expr_typed
      op2
  | EApp (e1, e2) -> fprintf fmt "(%a %a)" pp_expr_typed e1 pp_expr_typed e2
  | EIf (e_if, e_th, e_el) ->
    fprintf
      fmt
      "if %a then %a else %a"
      pp_expr_typed
      e_if
      pp_expr_typed
      e_th
      pp_expr_typed
      e_el
  | EList (hd, tl) -> fprintf fmt "%a :: %a" pp_expr_typed hd pp_expr_typed tl
  | ETuple (e1, e2, es) ->
    fprintf
      fmt
      "(%a)"
      (pp_tuple
         (fun (expr, _) ->
           match expr with
           | ETuple _ | EList (_, _) -> true
           | _ -> false)
         pp_expr_typed
         ", ")
      (e1 :: e2 :: es)
  | EClsr (decl, expr) -> fprintf fmt "%a\nin %a" pp_decl decl pp_expr_typed expr
  | EMatch (expr, case1, cases) ->
    let pp_case fmt (pat_typed, expr) =
      fprintf fmt "| %a -> %a" pp_pattern_typed pat_typed pp_expr_typed expr
    in
    let pp_cases = pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "\n") pp_case in
    fprintf fmt "match %a with\n%a" pp_expr_typed expr pp_cases (case1 :: cases)

and pp_expr_typed fmt = function
  | expr, Some typ -> fprintf fmt "(%a : %a)" pp_expr expr pp_typ typ
  | expr, None -> fprintf fmt "%a" pp_expr expr

and pp_decl fmt =
  let pp_let_body fmt (pat_or_op_typed, expr_typed) =
    fprintf fmt "%a = %a" pp_pattern_or_op_typed pat_or_op_typed pp_expr_typed expr_typed
  in
  function
  | DLet (rec_flag, (pat_or_op_typed, expr_typed)) ->
    fprintf fmt "let%a %a" pp_rec_flag rec_flag pp_let_body (pat_or_op_typed, expr_typed)
  | DLetMut (rec_flag, decl1, decl2, decls) ->
    let pp_decls =
      pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "\nand ") pp_let_body
    in
    fprintf fmt "let%a %a" pp_rec_flag rec_flag pp_decls (decl1 :: decl2 :: decls)
;;

let pp_prog = pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt ";;\n") pp_decl
