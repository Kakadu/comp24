(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast

let frestore_constant ppf c =
  let fprintf x = Format.fprintf ppf x in
  match c with
  | CInt i -> fprintf "%d" i
  | CBool false -> fprintf "false"
  | CBool true -> fprintf "true"
  | CNil -> fprintf "[]"
  | CUnit -> fprintf "()"
;;

let pp_list_with_commas pp_elem ppf lst =
  List.iteri
    (fun i pat ->
      if i <> 0 then Format.fprintf ppf ", " else ();
      pp_elem ppf pat)
    lst
;;

let rec frestore_pattern ppf pat =
  let fprintf x = Format.fprintf ppf x in
  match pat with
  | PWildCard -> fprintf "_"
  | PCons (h_pat, t_pat) ->
    fprintf "(%a :: %a)" frestore_pattern h_pat frestore_pattern t_pat
  | PIdentifier x -> fprintf "%s" x
  | PTuple lst -> fprintf "(%a)" (pp_list_with_commas frestore_pattern) lst
  | PConstant c -> frestore_constant ppf c
  | PConstraint (pat, tp) -> fprintf "(%a : %a)" frestore_pattern pat pp_type_name tp
;;

let frestore_rec_flag ppf = function
  | Rec -> Format.fprintf ppf "rec"
  | NotRec -> ()
;;

let rec frestore_expr ppf exp =
  let fprintf x = Format.fprintf ppf x in
  match exp with
  | EConstant c -> frestore_constant ppf c
  | EIdentifier s -> fprintf "%s" s
  | EFunction (pat, exp) ->
    fprintf "(fun %a -> %a)" frestore_pattern pat frestore_expr exp
  | EApplication (exp1, exp2) -> fprintf "(%a %a)" frestore_expr exp1 frestore_expr exp2
  | EIfThenElse (exp_cond, exp_then, exp_else) ->
    fprintf
      "(if %a then %a else %a)"
      frestore_expr
      exp_cond
      frestore_expr
      exp_then
      frestore_expr
      exp_else
  | ELetIn (rec_f, pat, exp_val, exp_body) ->
    fprintf
      "(let %a %a = %a in %a)"
      frestore_rec_flag
      rec_f
      frestore_pattern
      pat
      frestore_expr
      exp_val
      frestore_expr
      exp_body
  | ETuple lst -> fprintf "(%a)" (pp_list_with_commas frestore_expr) lst
  | EMatch (pat_head, pat_exp_lst) ->
    fprintf "(match %a with" frestore_expr pat_head;
    List.iter
      (fun (pat, exp) -> fprintf "\n| %a -> %a" frestore_pattern pat frestore_expr exp)
      pat_exp_lst;
    fprintf ")"
  | EConstraint (exp, tp) -> fprintf "(%a : %a)" frestore_expr exp pp_type_name tp
;;

let frestore_single_let ppf (DLet (pat, exp)) =
  Format.fprintf ppf "%a = %a" frestore_pattern pat frestore_expr exp
;;

let frestore_let_declaration ppf decl =
  let fprintf x = Format.fprintf ppf x in
  match decl with
  | DSingleLet (rec_f, slet) ->
    fprintf "let %a %a\n" frestore_rec_flag rec_f frestore_single_let slet
  | DMutualRecDecl (rec_f, slet_lst) ->
    fprintf "let %a " frestore_rec_flag rec_f;
    List.iteri
      (fun i slet ->
        if i <> 0 then fprintf " and " else ();
        frestore_single_let ppf slet)
      slet_lst;
    fprintf "\n"
;;

let frestore_declarations ppf decls = List.iter (frestore_let_declaration ppf) decls
let restore_declarations decls = Format.asprintf "%a" frestore_declarations decls
