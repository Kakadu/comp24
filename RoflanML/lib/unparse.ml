(** Copyright 2025, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Format

let unparse_const ppf c =
  match c with
  | CInt i -> fprintf ppf "%d" i
  | CBool b -> fprintf ppf "%s" (if b then "true" else "false")
  | CUnit -> fprintf ppf "()"
;;

let pp_list_with sep pp_elem ppf lst =
  List.iteri
    (fun i x ->
      if i <> 0 then fprintf ppf "%s" sep;
      pp_elem ppf x)
    lst
;;

let rec unparse_pattern ppf pat =
  match pat with
  | PWild -> fprintf ppf "_"
  | PEmpty -> fprintf ppf "[]"
  | PConst c -> unparse_const ppf c
  | PVar id -> fprintf ppf "%s" id
  | PCons (p1, p2, ps) ->
    fprintf ppf "(";
    unparse_pattern ppf p1;
    fprintf ppf " :: ";
    unparse_pattern ppf p2;
    List.iter
      (fun p ->
        fprintf ppf " :: ";
        unparse_pattern ppf p)
      ps;
    fprintf ppf ")"
  | POr (p1, p2, ps) ->
    fprintf ppf "(";
    unparse_pattern ppf p1;
    fprintf ppf " | ";
    unparse_pattern ppf p2;
    List.iter
      (fun p ->
        fprintf ppf " | ";
        unparse_pattern ppf p)
      ps;
    fprintf ppf ")"
  | PTuple (p1, p2, ps) ->
    fprintf ppf "(";
    unparse_pattern ppf p1;
    fprintf ppf ", ";
    unparse_pattern ppf p2;
    List.iter
      (fun p ->
        fprintf ppf ", ";
        unparse_pattern ppf p)
      ps;
    fprintf ppf ")"
;;

let unparse_is_rec ppf rec_flag = fprintf ppf "%s" (if rec_flag = Rec then "rec " else "")

let rec unparse_type ppf t =
  match t with
  | TInt -> fprintf ppf "int"
  | TBool -> fprintf ppf "bool"
  | TUnit -> fprintf ppf "unit"
  | TFun (t1, t2) -> fprintf ppf "(%a -> %a)" unparse_type t1 unparse_type t2
  | TList t' -> fprintf ppf "(%a list)" unparse_type t'
  | TTuple (t1, t2, ts) ->
    fprintf ppf "(";
    unparse_type ppf t1;
    fprintf ppf " * ";
    unparse_type ppf t2;
    List.iter
      (fun t_elem ->
        fprintf ppf " * ";
        unparse_type ppf t_elem)
      ts;
    fprintf ppf ")"
;;

let print_arg ppf (id, ty_opt) =
  match ty_opt with
  | None -> fprintf ppf "%s" id
  | Some ty -> fprintf ppf "(%s : %a)" id unparse_type ty
;;

let rec unparse_expr ?(top_level = false) ppf exp =
  match exp with
  | EConst c -> unparse_const ppf c
  | EVar id -> fprintf ppf "%s" id
  | EFun (arg, e) ->
    if top_level
    then fprintf ppf "fun %a -> %a" print_arg arg (unparse_expr ~top_level:true) e
    else fprintf ppf "(fun %a -> %a)" print_arg arg (unparse_expr ~top_level:false) e
  | EApp (e1, e2) ->
    fprintf
      ppf
      "(%a %a)"
      (unparse_expr ~top_level:false)
      e1
      (unparse_expr ~top_level:false)
      e2
  | EBranch (e_cond, e_then, e_else) ->
    if top_level
    then
      fprintf
        ppf
        "if %a then %a else %a"
        (unparse_expr ~top_level:true)
        e_cond
        (unparse_expr ~top_level:true)
        e_then
        (unparse_expr ~top_level:true)
        e_else
    else
      fprintf
        ppf
        "(if %a then %a else %a)"
        (unparse_expr ~top_level:false)
        e_cond
        (unparse_expr ~top_level:false)
        e_then
        (unparse_expr ~top_level:false)
        e_else
  | ELetIn (r, id, e_val, e_body) ->
    if top_level
    then
      fprintf
        ppf
        "let %a%s = %a in %a"
        unparse_is_rec
        r
        id
        (unparse_expr ~top_level:true)
        e_val
        (unparse_expr ~top_level:true)
        e_body
    else
      fprintf
        ppf
        "(let %a%s = %a in %a)"
        unparse_is_rec
        r
        id
        (unparse_expr ~top_level:false)
        e_val
        (unparse_expr ~top_level:false)
        e_body
  | ETuple (e1, e2, es) ->
    fprintf ppf "(";
    unparse_expr ~top_level:false ppf e1;
    fprintf ppf ", ";
    unparse_expr ~top_level:false ppf e2;
    List.iter
      (fun e ->
        fprintf ppf ", ";
        unparse_expr ~top_level:false ppf e)
      es;
    fprintf ppf ")"
  | EList es -> fprintf ppf "[%a]" (pp_list_with "; " (unparse_expr ~top_level:false)) es
  | EMatch (e, cases) ->
    fprintf ppf "(match %a with" (unparse_expr ~top_level:false) e;
    List.iter
      (fun (p, e_case) ->
        fprintf ppf " | %a -> %a" unparse_pattern p (unparse_expr ~top_level:false) e_case)
      cases;
    fprintf ppf ")"
;;

let unparse_decl ppf d =
  match d with
  | DLet (r, id, e) ->
    fprintf ppf "let %a%s = %a" unparse_is_rec r id (unparse_expr ~top_level:true) e
  | DMutualLet (r, binds) ->
    (match binds with
     | [] -> () (* этого случая не должно быть *)
     | (id, e) :: rest ->
       fprintf ppf "let %a%s = %a" unparse_is_rec r id (unparse_expr ~top_level:true) e;
       List.iter
         (fun (id, e) -> fprintf ppf " and %s = %a" id (unparse_expr ~top_level:true) e)
         rest)
;;

let unparse_program prog =
  asprintf
    "%a"
    (fun ppf prog ->
      List.iter
        (fun d ->
          unparse_decl ppf d;
          fprintf ppf "\n")
        prog)
    prog
;;
