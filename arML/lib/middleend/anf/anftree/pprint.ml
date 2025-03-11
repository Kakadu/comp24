(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree
open Ast.Pprint
open Anftree

let pp_identifier ppf (Id id) = Format.fprintf ppf "%s" id

let rec pp_immut_expr ppf = function
  | IConstant c -> pp_constant ppf c
  | IEmptyList -> Format.fprintf ppf "[]"
  | IIdentifier id -> pp_identifier ppf id
  | ITuple (e1, e2, es) ->
    Format.fprintf
      ppf
      "(%a)"
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ") pp_immut_expr)
      (e1 :: e2 :: es)
;;

let rec pp_complex_expr ppf = function
  | CAtom e -> pp_immut_expr ppf e
  | CApplication (e1, e2, es) ->
    Format.fprintf
      ppf
      "(%a %a)"
      pp_immut_expr
      e1
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") pp_immut_expr)
      (e2 :: es)
  | CIfThenElse (cond, t, f) ->
    Format.fprintf
      ppf
      "(if %a then %a else %a)"
      pp_immut_expr
      cond
      pp_a_expr
      t
      pp_a_expr
      f
  | CListConstructor (e1, e2) ->
    Format.fprintf ppf "(%a :: %a)" pp_immut_expr e1 pp_immut_expr e2
  | CTyped (e, t) -> Format.fprintf ppf "(%a : %a)" pp_complex_expr e pp_type_definition t

and pp_a_expr ppf = function
  | ALetIn (id, e, body) ->
    Format.fprintf
      ppf
      "(let %a = %a in@\n%a)"
      pp_identifier
      id
      pp_complex_expr
      e
      pp_a_expr
      body
  | AComplex e -> pp_complex_expr ppf e
;;

let pp_anf_decl_case ppf (id, args, body) =
  match args with
  | [] -> Format.fprintf ppf "%a = %a" pp_identifier id pp_a_expr body
  | _ ->
    Format.fprintf
      ppf
      "%a %a = %a"
      pp_identifier
      id
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") pp_identifier)
      args
      pp_a_expr
      body
;;

let pp_anf_decl ppf = function
  | ADOrdinary decl -> Format.fprintf ppf "let %a \n" pp_anf_decl_case decl
  | ADRecursive (decl, decls) ->
    Format.fprintf
      ppf
      "let rec %a \n"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf " and ")
         pp_anf_decl_case)
      (decl :: decls)
;;

let pp_anf_program ppf program =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf "\n")
    pp_anf_decl
    ppf
    program
;;

let print_anf_expression expr = Format.printf "%s\n" (Format.asprintf "%a" pp_a_expr expr)

let print_anf_program program =
  Format.printf "%s\n" (Format.asprintf "%a" pp_anf_program program)
;;
