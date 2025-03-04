(** Copyright 2024-2025, CursedML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Anf
open Ast
open Stdlib.Format

let binop_to_string = function
  | Mul -> "*"
  | Div -> "/"
  | Add -> "+"
  | Sub -> "-"
  | Eq -> "="
  | Neq -> "<>"
  | Lt -> "<"
  | Gt -> ">"
  | And -> "&&"
  | Or -> "||"
;;

let pp_const ppf = function
  | Const_int i -> fprintf ppf "@[%i@]" i
  | Const_bool b -> fprintf ppf "@[%b@]" b
  | Const_nil -> fprintf ppf "@[[]@]"
  | Const_unit -> fprintf ppf "@[()@]"
;;

let pp_imm ppf = function
  | Avar id -> fprintf ppf "@[%s@]" id
  | Aconst c -> pp_const ppf c
;;

let rec pp_cexpr ppf = function
  | CImm imm -> pp_imm ppf imm
  | CCons (x, xs) -> fprintf ppf "@[ %a :: %a @]" pp_imm x pp_imm xs
  | CGetfield (idx, i) -> fprintf ppf "@[ getfield %i %a @]" idx pp_imm i
  | CBinop (op, x, y) ->
    fprintf ppf "@[%a %s %a @]" pp_imm x (binop_to_string op) pp_imm y
  | CUnop (Not, x) -> fprintf ppf "@[!%a @]" pp_imm x
  | CApp (f, args) -> fprintf ppf "@[%a %a@]" pp_imm f (pp_list "") args
  | CFun (f, a) -> fprintf ppf "@[<2>fun %s -> @,%a@,@]" f pp_aexpr a
  | CIte (c, t, e) ->
    fprintf ppf "@[<2>if %a @, then %a @, else @, %a @, @]" pp_imm c pp_aexpr t pp_aexpr e
  | CTuple elems -> pp_list "," ppf elems

and pp_aexpr ppf = function
  | ALet (Recursive, id, e, scope) ->
    fprintf ppf "@[let rec %s = @, %a in @, %a@]" id pp_cexpr e pp_aexpr scope
  | ALet (NonRecursive, id, e, scope) ->
    fprintf ppf "@[let %s = @, %a in @, %a@]" id pp_cexpr e pp_aexpr scope
  | AExpr cexpr -> pp_cexpr ppf cexpr

and pp_list sep ppf list =
  let rec helper = function
    | [] -> Utils.unreachable ()
    | [ e ] -> pp_imm ppf e
    | e :: es ->
      fprintf ppf "@[%a%s @]" pp_imm e sep;
      helper es
  in
  helper list
;;

let pp ppf astructure =
  let pp_item (AStr_value (rf, bindings)) =
    let pp_decl ppf start (id, e) = fprintf ppf "@[%s %s = %a @]@." start id pp_aexpr e in
    match rf, bindings with
    | Recursive, [ (id, e) ] -> pp_decl ppf "let rec" (id, e)
    | NonRecursive, [ (id, e) ] -> pp_decl ppf "let" (id, e)
    | Recursive, (id, e) :: bs ->
      pp_decl ppf "let rec" (id, e);
      List.iter (fun (id, e) -> pp_decl ppf "and" (id, e)) bs
    | _ ->
      Utils.internalfail
      @@ Format.sprintf "unexpected declaration %i" (List.length bindings)
  in
  List.iter pp_item astructure
;;
