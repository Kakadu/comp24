(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.Pprint
open Llast

let rec pp_ll_expr ppf = function
  | LEConstant c -> pp_constant ppf c
  | LEIdentifier id -> pp_identifier ppf id
  | LEApplication (e1, e2, es) ->
    Format.fprintf
      ppf
      "(%a %a)"
      pp_ll_expr
      e1
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") pp_ll_expr)
      (e2 :: es)
  | LELetIn (c, cs, e) ->
    Format.fprintf
      ppf
      "(let %a in %a)"
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " and ") pp_ll_let_in_case)
      (c :: cs)
      pp_ll_expr
      e
  | LEIfThenElse (cond, t, f) ->
    Format.fprintf
      ppf
      "(if %a then %a%a)"
      pp_ll_expr
      cond
      pp_ll_expr
      t
      (fun ppf -> function
         | None -> ()
         | Some e -> Format.fprintf ppf " else %a" pp_ll_expr e)
      f
  | LETuple (e1, e2, es) ->
    Format.fprintf
      ppf
      "(%a)"
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ") pp_ll_expr)
      (e1 :: e2 :: es)
  | LEEmptyList -> Format.fprintf ppf "[]"
  | LEListConstructor (e1, e2) ->
    Format.fprintf ppf "(%a :: %a)" pp_ll_expr e1 pp_ll_expr e2
  | LEMatchWith (e, c, cs) ->
    Format.fprintf
      ppf
      "(match %a with %a)"
      pp_ll_expr
      e
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " | ") pp_ll_case)
      (c :: cs)
  | LETyped (e, t) -> Format.fprintf ppf "(%a : %a)" pp_ll_expr e pp_type_definition t

and pp_ll_case ppf (p, e) = Format.fprintf ppf "%a -> %a" pp_pattern p pp_ll_expr e

and pp_ll_let_in_case ppf (p, e) = Format.fprintf ppf "%a = %a" pp_pattern p pp_ll_expr e

and pp_ll_decl_case ppf (p, args, e) =
  match args with
  | [] -> Format.fprintf ppf "%a = %a" pp_pattern p pp_ll_expr e
  | _ ->
    Format.fprintf
      ppf
      "%a %a = %a"
      pp_pattern
      p
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") pp_pattern)
      args
      pp_ll_expr
      e
;;

let pp_ll_decl ppf = function
  | LDOrdinary (c, cs) ->
    Format.fprintf
      ppf
      "let %a"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf " and ")
         pp_ll_decl_case)
      (c :: cs)
  | LDRecursive (c, cs) ->
    Format.fprintf
      ppf
      "let rec %a"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf " and ")
         pp_ll_decl_case)
      (c :: cs)
;;

let pp_ll_program ppf p =
  Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "\n") pp_ll_decl ppf p
;;

let print_ll_program program =
  Format.printf "%s\n" (Format.asprintf "%a" pp_ll_program program)
;;
