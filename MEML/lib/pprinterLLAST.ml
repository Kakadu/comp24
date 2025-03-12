(** Copyright 2024-2025, Perevalov Efim, Ermolovich Anna *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llast
open Ast
open PprinterAST

let rec pp_llexpression formatter e =
  match e with
  | LLConst c -> pp_constant formatter c
  | LLVar n -> Format.fprintf formatter "%s" n
  | LLApp (e1, e2) ->
    Format.fprintf formatter "(%a %a)" pp_llexpression e1 pp_llexpression e2
  | LLIfElse (i, t, e) ->
    Format.fprintf
      formatter
      "\n  if %a\n  then %a\n  else %a"
      pp_llexpression
      i
      pp_llexpression
      t
      pp_llexpression
      e
  | LLTuple t -> Format.fprintf formatter "(%a)" (pp_tuple pp_llexpression) t
  | LLEbinOp (op, l, r) ->
    Format.fprintf formatter "(%a %a %a)" pp_llexpression l pp_binop op pp_llexpression r
  | LLVars (head, tail) ->
    let rec print_list formatter = function
      | LLList (h, t) -> Format.fprintf formatter "%a; %a" pp_llexpression h print_list t
      | _ -> Format.fprintf formatter "%a" pp_llexpression tail
    in
    Format.fprintf formatter "%a" print_list (LLList (head, tail))
  | LLList (head, tail) ->
    let rec print_list formatter = function
      | LLConst CNil -> Format.fprintf formatter ""
      | LLList (h, t) -> Format.fprintf formatter "%a; %a" pp_llexpression h print_list t
      | _ -> Format.fprintf formatter "%a" pp_llexpression tail
    in
    Format.fprintf formatter "[%a]" print_list (LLList (head, tail))
  | LLMatch (m, p) ->
    Format.fprintf formatter "(match %a with" pp_llexpression m;
    List.iter
      (fun (p, lle) ->
        Format.fprintf formatter "\n| %a -> %a" pp_pattern p pp_llexpression lle)
      p;
    Format.fprintf formatter ")"
  | LLPatLetIn (names, e, ine) ->
    Format.fprintf
      formatter
      "\n  let %a = %a\n  in %a"
      pp_pattern
      names
      pp_llexpression
      e
      pp_llexpression
      ine
  | LLLetIn (n, e, ine) ->
    Format.fprintf
      formatter
      "\n  let %s = %a\n  in %a"
      n
      pp_llexpression
      e
      pp_llexpression
      ine
;;

let pp_llbindings formatter bindings =
  List.iter
    (fun bind ->
      match bind with
      | LLLet (r, n_list, p, e) ->
        Format.fprintf
          formatter
          "let %a %s %a = %a"
          pp_rec
          r
          n_list
          (fun fmt -> List.iter (fun pat -> Format.fprintf fmt "%a " pp_pattern pat))
          p
          pp_llexpression
          e;
        Format.fprintf formatter "\n"
      | LLLetPat (n_list, e) ->
        Format.fprintf formatter "let %a = %a" pp_pattern n_list pp_llexpression e;
        Format.fprintf formatter "\n"
      | LLExpression e ->
        pp_llexpression formatter e;
        Format.fprintf formatter "\n")
    bindings
;;

let pp_lambda_lifting bindings = Format.asprintf "%a" pp_llbindings bindings
