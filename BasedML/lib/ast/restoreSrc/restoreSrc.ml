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

let rec frestore_pattern ppf pat =
  let rec_call = frestore_pattern ppf in
  let fprintf x = Format.fprintf ppf x in
  match pat with
  | PWildCard -> fprintf "_"
  | PCons (h_pat, t_pat) ->
    fprintf "(";
    rec_call h_pat;
    fprintf " :: ";
    rec_call t_pat;
    fprintf ")"
  | PIdentifier x -> fprintf "%s" x
  | PTuple lst ->
    fprintf "(";
    List.iteri
      (fun i pat ->
        if i != 0 then fprintf ", " else ();
        rec_call pat)
      lst;
    fprintf ")"
  | PConstant c -> frestore_constant ppf c
  | PConstraint (pat, tp) ->
    fprintf "(";
    rec_call pat;
    fprintf " : ";
    pp_type_name ppf tp;
    fprintf ")"
;;

let frestore_rec_flag ppf = function
  | Rec -> Format.fprintf ppf "rec"
  | NotRec -> ()
;;

let rec frestore_expr ppf exp =
  let rec_call = frestore_expr ppf in
  let fprintf x = Format.fprintf ppf x in
  match exp with
  | EConstant c -> frestore_constant ppf c
  | EIdentifier s -> fprintf "%s" s
  | EFunction (pat, exp) ->
    fprintf "(fun ";
    frestore_pattern ppf pat;
    fprintf " -> ";
    rec_call exp;
    fprintf ")"
  | EApplication (exp1, exp2) ->
    fprintf "(";
    rec_call exp1;
    fprintf " ";
    rec_call exp2;
    fprintf ")"
  | EIfThenElse (exp_cond, exp_then, exp_else) ->
    fprintf "(if (";
    rec_call exp_cond;
    fprintf ") then (";
    rec_call exp_then;
    fprintf ") else (";
    rec_call exp_else;
    fprintf "))"
  | ELetIn (rec_f, pat, exp_val, exp_body) ->
    fprintf "(let ";
    frestore_rec_flag ppf rec_f;
    fprintf " ";
    frestore_pattern ppf pat;
    fprintf " = ";
    rec_call exp_val;
    fprintf " in ";
    rec_call exp_body;
    fprintf ")"
  | ETuple lst ->
    fprintf "(";
    List.iteri
      (fun i exp ->
        if i <> 0 then fprintf ", " else ();
        rec_call exp)
      lst;
    fprintf ")"
  | EMatch (pat_head, pat_exp_lst) ->
    fprintf "(match ";
    frestore_pattern ppf pat_head;
    fprintf " with ";
    List.iter
      (fun (pat, exp) ->
        fprintf "\n| ";
        frestore_pattern ppf pat;
        fprintf " -> ";
        rec_call exp)
      pat_exp_lst;
    fprintf ")"
  | EConstraint (exp, tp) ->
    fprintf "(";
    rec_call exp;
    fprintf " : ";
    pp_type_name ppf tp;
    fprintf ")"
;;

let frestore_single_let ppf (DLet (pat, exp)) =
  frestore_pattern ppf pat;
  Format.fprintf ppf " = ";
  frestore_expr ppf exp
;;

let frestore_let_declaration ppf decl =
  let fprintf x = Format.fprintf ppf x in
  match decl with
  | DSingleLet (rec_f, slet) ->
    fprintf "let ";
    frestore_rec_flag ppf rec_f;
    fprintf " ";
    frestore_single_let ppf slet;
    fprintf ";;\n"
  | DMutualRecDecl (rec_f, slet_lst) ->
    fprintf "let ";
    frestore_rec_flag ppf rec_f;
    fprintf " ";
    List.iteri
      (fun i slet ->
        if i != 0 then fprintf " and " else ();
        frestore_single_let ppf slet)
      slet_lst;
    fprintf ";;\n"
;;

let frestore_declarations ppf decls = List.iter (frestore_let_declaration ppf) decls
let restore_declarations decls = Format.asprintf "%a" frestore_declarations decls
