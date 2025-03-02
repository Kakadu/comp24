(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Llast
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

let rec frestore_llexpr ppf exp =
  let rec_call = frestore_llexpr ppf in
  let fprintf x = Format.fprintf ppf x in
  match exp with
  | LLConstant c -> frestore_constant ppf c
  | LLIdentifier s -> fprintf "%s" s
  | LLApplication (exp1, exp2) ->
    fprintf "(";
    rec_call exp1;
    fprintf " ";
    rec_call exp2;
    fprintf ")"
  | LLIfThenElse (exp_cond, exp_then, exp_else) ->
    fprintf "(if (";
    rec_call exp_cond;
    fprintf ") then ";
    rec_call exp_then;
    fprintf " else (";
    rec_call exp_else;
    fprintf "))"
  | LLLetIn (rec_f, pat, exp_val, exp_body) ->
    fprintf "(let ";
    frestore_rec_flag ppf rec_f;
    fprintf " ";
    frestore_pattern ppf pat;
    fprintf " = ";
    rec_call exp_val;
    fprintf " in ";
    rec_call exp_body;
    fprintf ")"
  | LLTuple lst ->
    fprintf "(";
    List.iteri
      (fun i exp ->
        if i != 0 then fprintf ", " else ();
        rec_call exp)
      lst;
    fprintf ")"
  | LLMatch (pat_head, pat_exp_lst) ->
    fprintf "(match ";
    frestore_llexpr ppf pat_head;
    fprintf " with ";
    List.iter
      (fun (pat, exp) ->
        fprintf "\n| ";
        frestore_pattern ppf pat;
        fprintf " -> ";
        rec_call exp)
      pat_exp_lst;
    fprintf ")"
  | LLConstraint (exp, tp) ->
    fprintf "(";
    rec_call exp;
    fprintf " : ";
    pp_type_name ppf tp;
    fprintf ")"
;;

let pp_lllet_declaration fmt = function
  | LLDSingleLet (rec_flag, LLLet (pat, patterns, l)) ->
    Format.fprintf
      fmt
      "let %a %a %a = %a"
      frestore_rec_flag
      rec_flag
      frestore_pattern
      pat
      (fun fmt -> List.iter (fun pat -> Format.fprintf fmt "%a " frestore_pattern pat))
      patterns
      frestore_llexpr
      l
  | LLDMutualRecDecl (rec_flag, bindings) ->
    Format.fprintf fmt "let ";
    frestore_rec_flag fmt rec_flag;
    Format.fprintf fmt " ";
    List.iteri
      (fun i binding ->
        if i != 0 then Format.fprintf fmt " and ";
        match binding with
        | LLLet (pat, patterns, exp) ->
          Format.fprintf fmt " ";
          frestore_pattern fmt pat;
          (fun fmt ->
            List.iter (fun pat -> Format.fprintf fmt " %a " frestore_pattern pat))
            fmt
            patterns;
          Format.fprintf fmt " = %a " frestore_llexpr exp)
      bindings
;;

let pp_llprogram fmt program =
  List.iter
    (fun decl ->
      pp_lllet_declaration fmt decl;
      Format.fprintf fmt "\n")
    program
;;
