(** Copyright 2024-2025, Perevalov Efim, Ermolovich Anna *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Cast
open Ast
open PprinterAST

let rec pp_llexpression formatter e =
  match e with
  | CConst c -> pp_constant formatter c
  | CVar n -> Format.fprintf formatter "%s" n
  | CApp (e1, e2) ->
    Format.fprintf formatter "(%a %a)" pp_llexpression e1 pp_llexpression e2
  | CIfElse (i, t, e) ->
    Format.fprintf
      formatter
      "\n  if %a\n  then %a\n  else %a"
      pp_llexpression
      i
      pp_llexpression
      t
      pp_llexpression
      e
  | CLetIn (r, n, args, e, ine) ->
    Format.fprintf
      formatter
      "\n  let %a %s %a = %a\n  in %a"
      pp_rec
      r
      n
      (fun fmt -> List.iter (fun pat -> Format.fprintf fmt "%a " pp_pattern pat))
      args
      pp_llexpression
      e
      pp_llexpression
      ine
  | CPatLetIn (names, e, ine) ->
    Format.fprintf
      formatter
      "\n  let %a = %a\n  in %a"
      pp_pattern
      names
      pp_llexpression
      e
      pp_llexpression
      ine
  | CTuple t -> Format.fprintf formatter "(%a)" (pp_tuple pp_llexpression) t
  | CEbinOp (op, l, r) ->
    Format.fprintf formatter "(%a %a %a)" pp_llexpression l pp_binop op pp_llexpression r
  | CList (head, tail) ->
    let rec print_list formatter = function
      | CConst CNil -> Format.fprintf formatter ""
      | CList (h, t) -> Format.fprintf formatter "%a; %a" pp_llexpression h print_list t
      | _ -> Format.fprintf formatter "%a" pp_llexpression tail
    in
    Format.fprintf formatter "[%a]" print_list (CList (head, tail))
  | CMatch (m, p) ->
    Format.fprintf formatter "(match %a with" pp_llexpression m;
    List.iter
      (fun (p, lle) ->
        Format.fprintf formatter "\n| %a -> %a" pp_pattern p pp_llexpression lle)
      p;
    Format.fprintf formatter ")"
;;

let pp_llbindings formatter bindings =
  List.iter
    (fun bind ->
      match bind with
      | CLets (r, clets_list) ->
        (* Перебираем все let-объявления внутри CLet *)
        List.iteri
          (fun i clets ->
            match clets with
            | CLet (n, p, e) ->
              if i = 0
              then
                (* Первое объявление начинается с "let" *)
                Format.fprintf
                  formatter
                  "let %a %s %a = %a"
                  pp_rec
                  r
                  n
                  (fun fmt ->
                    List.iter (fun pat -> Format.fprintf fmt "%a " pp_pattern pat))
                  p
                  pp_llexpression
                  e
              else
                (* Все последующие объявления начинаются с "and" *)
                Format.fprintf
                  formatter
                  "and %a %s %a = %a"
                  pp_rec
                  r
                  n
                  (fun fmt ->
                    List.iter (fun pat -> Format.fprintf fmt "%a " pp_pattern pat))
                  p
                  pp_llexpression
                  e
            | CLetPat (names, e) ->
              if i = 0
              then
                (* Первое объявление начинается с "let" *)
                Format.fprintf formatter "let %a = %a" pp_pattern names pp_llexpression e
              else
                (* Все последующие объявления начинаются с "and" *)
                Format.fprintf formatter "and %a = %a" pp_pattern names pp_llexpression e)
          clets_list;
        Format.fprintf formatter "\n"
      | CExpression e ->
        pp_llexpression formatter e;
        Format.fprintf formatter "\n")
    bindings
;;

let pp_closure bindings = Format.asprintf "%a" pp_llbindings bindings
