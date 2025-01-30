(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Anf_ast
open Format

let rec pp_type_name ppf tp =
  let rec_call tp = pp_type_name ppf tp in
  let fprintf x = Format.fprintf ppf x in
  match tp with
  | TUnit -> fprintf "unit"
  | TInt -> fprintf "int"
  | TBool -> fprintf "bool"
  | TPoly name -> fprintf "'%s" name
  | TTuple lst ->
    fprintf "(";
    List.iteri
      (fun i tp ->
         if i <> 0 then fprintf " * " else ();
         rec_call tp)
      lst;
    fprintf ")"
  | TFunction (tp_arg, tp_ret) ->
    fprintf "(%a -> %a)" pp_type_name tp_arg pp_type_name tp_ret
  | TList tp -> fprintf "(%a list)" pp_type_name tp
;;

let frestore_constant ppf c =
  let fprintf x = Format.fprintf ppf x in
  match c with
  | CInt i -> fprintf "%d" i
  | CBool false -> fprintf "false"
  | CBool true -> fprintf "true"
  | CNil -> fprintf "[]"
  | CUnit -> fprintf "()"
;;

let frestore_imm ppf c =
  let fprintf x = Format.fprintf ppf x in
  match c with
  | ImmInt i -> fprintf "%d" i
  | ImmBool false -> fprintf "false"
  | ImmBool true -> fprintf "true"
  | ImmNil -> fprintf "[]"
  | ImmIdentifier id -> fprintf "%s" id
  | ImmUnit -> fprintf "()"
;;

let pp_list ppf pp sep lst =
  let rec aux = function
    | [] -> ()
    | [ x ] -> pp ppf x
    | x :: xs ->
      pp ppf x;
      fprintf ppf "%s" sep;
      aux xs
  in
  aux lst
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

let rec restore_cexpr ppf = function
  | CImmExpr imm -> fprintf ppf "%a" frestore_imm imm
  | CTuple tup -> fprintf ppf "(%a)" (fun ppf -> pp_list ppf frestore_imm ", ") tup
  | CIfThenElse (cond, then_branch, else_branch) ->
    fprintf
      ppf
      "if %a then %a else %a"
      frestore_imm
      cond
      pp_aexpr
      then_branch
      pp_aexpr
      else_branch
  | CMatch (pat_head, pat_exp_lst) ->
    fprintf
      ppf
      "match %a with\n%a"
      frestore_pattern
      pat_head
      (fun ppf ->
         pp_list
           ppf
           (fun ppf (pat, ae) ->
              fprintf ppf "| %a -> %a" frestore_pattern pat pp_aexpr ae)
           "\n")
      pat_exp_lst
  | CApplication (left, rigth) -> fprintf ppf "%a %a" frestore_imm left frestore_imm rigth
  | CConstraint (imm, typ) -> fprintf ppf "(%a : %a)" frestore_imm imm pp_type_name typ

and pp_aexpr ppf = function
  | ACExpr cexp -> fprintf ppf "%a" restore_cexpr cexp
  | ALetIn (pat, outer, inner) ->
    fprintf
      ppf
      "let %a = %a in\n %a"
      frestore_pattern
      pat
      restore_cexpr
      outer
      pp_aexpr
      inner
;;

let frestore_rec_flag ppf = function
  | Rec -> Format.fprintf ppf "rec"
  | NotRec -> ()
;;

let restore_anf_decl fmt = function
  | ADSingleLet (rec_flag, ALet (pat, patterns, body)) ->
    Format.fprintf
      fmt
      "let %a %a %a = %a"
      frestore_rec_flag
      rec_flag
      frestore_pattern
      pat
      (fun fmt -> List.iter (fun pat -> Format.fprintf fmt "%a " frestore_pattern pat))
      patterns
      pp_aexpr
      body
  | ADMutualRecDecl (rec_flag, bindings) ->
    Format.fprintf fmt "let ";
    frestore_rec_flag fmt rec_flag;
    Format.fprintf fmt " ";
    List.iteri
      (fun i binding ->
         if i != 0 then Format.fprintf fmt " and ";
         match binding with
         | ALet (pat, patterns, exp) ->
           Format.fprintf fmt " ";
           frestore_pattern fmt pat;
           (fun fmt ->
              List.iter (fun pat -> Format.fprintf fmt " %a " frestore_pattern pat))
             fmt
             patterns;
           Format.fprintf fmt " = %a " pp_aexpr exp)
      bindings
;;

let restore_program formatter declarations =
  fprintf
    formatter
    "%a"
    (fun formatter -> pp_list formatter restore_anf_decl "\n")
    declarations
;;
