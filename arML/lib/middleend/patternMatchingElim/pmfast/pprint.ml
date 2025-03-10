(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.Pprint
open Pmfast

let rec pp_pmf_expr ppf = function
  | PMFConstant c -> pp_constant ppf c
  | PMFIdentifier id -> pp_identifier ppf id
  | PMFApplication (e1, e2, es) ->
    Format.fprintf
      ppf
      "(%a %a)"
      pp_pmf_expr
      e1
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") pp_pmf_expr)
      (e2 :: es)
  | PMFLetIn (c, e) ->
    Format.fprintf
      ppf
      "let %a in@,%a"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf "@,and ")
         pp_pmf_let_in_case)
      (c :: [])
      pp_pmf_expr
      e
  | PMFIfThenElse (cond, t, f) ->
    Format.fprintf
      ppf
      "(if %a then %a%a)"
      pp_pmf_expr
      cond
      pp_pmf_expr
      t
      (fun ppf -> function
         | None -> ()
         | Some e -> Format.fprintf ppf " else %a" pp_pmf_expr e)
      f
  | PMFTuple (e1, e2, es) ->
    Format.fprintf
      ppf
      "(%a)"
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ") pp_pmf_expr)
      (e1 :: e2 :: es)
  | PMFEmptyList -> Format.fprintf ppf "[]"
  | PMFListConstructor (e1, e2) ->
    Format.fprintf ppf "(%a :: %a)" pp_pmf_expr e1 pp_pmf_expr e2
  | PMFTyped (e, t) -> Format.fprintf ppf "(%a : %a)" pp_pmf_expr e pp_type_definition t

and pp_pmf_let_in_case ppf (id, e) =
  Format.fprintf ppf "%a = %a" pp_identifier id pp_pmf_expr e

and pp_pmf_decl_case ppf (id, args, e) =
  match args with
  | [] -> Format.fprintf ppf "%a = %a" pp_identifier id pp_pmf_expr e
  | _ ->
    Format.fprintf
      ppf
      "%a %a = %a"
      pp_identifier
      id
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") pp_identifier)
      args
      pp_pmf_expr
      e
;;

let pp_pmf_decl ppf = function
  | PMFDOrdinary c ->
    Format.fprintf
      ppf
      "let %a"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf "@,and ")
         pp_pmf_decl_case)
      (c :: [])
  | PMFDRecursive (c, cs) ->
    Format.fprintf
      ppf
      "let rec %a"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf "@,and ")
         pp_pmf_decl_case)
      (c :: cs)
;;

let pp_pmf_program ppf p =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf "\n\n")
    pp_pmf_decl
    ppf
    p
;;

let print_pmf_expression expr =
  Format.printf "%s\n" (Format.asprintf "%a" pp_pmf_expr expr)
;;

let print_pmf_program program =
  Format.printf "%s\n" (Format.asprintf "%a" pp_pmf_program program)
;;
