(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Me_ast
open Format

let pp_ident ppf = function
  | Me_unit -> fprintf ppf "()"
  | Me_name s -> fprintf ppf "%s" s
;;

let needs_parens = function
  | MExp_ident id -> Common.Base_lib.is_binop id
  | _ -> false
;;

let rec pp_expr ppf = function
  | MExp_constant c -> pp_const ppf c
  | MExp_ident id as e ->
    (match needs_parens e with
     | true -> fprintf ppf "(%s)" id
     | false -> fprintf ppf "%s" id)
  | MExp_tuple exprs ->
    fprintf ppf "(@[<hov>";
    pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ",@ ") pp_expr ppf exprs;
    fprintf ppf "@])"
  | MExp_apply (e1, e2) ->
    (match e1 with
     | MExp_apply (op, e1') ->
       fprintf ppf "@[<hov 2>(%a %a)@ %a@]" pp_expr op pp_expr e1' pp_expr e2
     | _ -> fprintf ppf "@[<hov 2>%a %a@]" pp_expr e1 pp_expr_atomic e2)
  | MExp_list (e1, e2) -> fprintf ppf "@[<hov 2>%a ::@ %a@]" pp_expr_atomic e1 pp_expr e2
  | MExp_ifthenelse (cond, then_, else_) ->
    fprintf
      ppf
      "@[<v>if %a@,then@,  @[<v>%a@]@,else@,  @[<v>%a@]@]"
      pp_expr
      cond
      pp_expr
      then_
      pp_expr
      else_
  | MExp_function (param, body) ->
    fprintf ppf "@[<v>fun %a ->@,  @[<v>%a@]@]" pp_ident param pp_expr body
  | MExp_let (decl, body) -> fprintf ppf "@[<v>%a@,in@,%a@]" pp_decl decl pp_expr body

and pp_expr_atomic ppf e =
  match e with
  | (MExp_constant _ | MExp_ident _) when not (needs_parens e) -> pp_expr ppf e
  | _ -> fprintf ppf "(%a)" pp_expr e

and pp_const ppf = function
  | Const_int n -> fprintf ppf "%d" n
  | Const_bool b -> fprintf ppf "%b" b
  | Const_nil -> fprintf ppf "[]"
  | Const_unit -> fprintf ppf "()"

and pp_value_binding ppf vb =
  fprintf ppf "@[<v>%a =@,  @[<v>%a@]@]" pp_ident vb.m_vb_pat pp_expr vb.m_vb_expr

and pp_decl ppf (MDecl (rec_flag, bindings)) =
  let rec_str =
    match rec_flag with
    | Recursive -> " rec"
    | Nonrecursive -> ""
  in
  fprintf
    ppf
    "@[<v>let%s %a@]"
    rec_str
    (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@,and ") pp_value_binding)
    bindings
;;

let pp_me_program ppf decls =
  fprintf ppf "@[<v>";
  pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@,@,") pp_decl ppf decls;
  fprintf ppf "@]@."
;;
