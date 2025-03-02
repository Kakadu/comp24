(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Format

let rec collect_fun_args expr args =
  match expr with
  | Exp_function (pat, body) -> collect_fun_args body (pat :: args)
  | _ -> List.rev args, expr
;;

let ident_pp fmt id = fprintf fmt "%s" id

let rec_flag_pp fmt rf =
  match rf with
  | Nonrecursive -> fprintf fmt " "
  | Recursive -> fprintf fmt " rec "
;;

let const_pp fmt c =
  match c with
  | Const_int i -> fprintf fmt "%d" i
  | Const_bool b -> fprintf fmt "%b" b
  | Const_nil -> fprintf fmt "[]"
  | Const_unit -> fprintf fmt "()"
;;

let rec core_type_pp fmt typ =
  match typ with
  | Ptyp_int -> fprintf fmt "int"
  | Ptyp_bool -> fprintf fmt "bool"
  | Ptyp_unit -> fprintf fmt "unit"
  | Ptyp_var id -> fprintf fmt "'%s" id
  | Ptyp_list t -> fprintf fmt "(%a) list" core_type_pp t
  | Ptyp_tuple types ->
    fprintf
      fmt
      "(%a)"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " * ") core_type_pp)
      types
  | Ptyp_arrow (t1, t2) -> fprintf fmt "(%a -> %a)" core_type_pp t1 core_type_pp t2
;;

let pp_spaces fmt n =
  let spaces = String.make n ' ' in
  fprintf fmt "%s" spaces
;;

let rec pattern_pp fmt pat =
  match pat with
  | Pat_const c -> const_pp fmt c
  | Pat_var id -> ident_pp fmt id
  | Pat_cons (p1, p2) -> fprintf fmt "(%a :: %a)" pattern_pp p1 pattern_pp p2
  | Pat_any -> fprintf fmt "_"
  | Pat_tuple patterns ->
    fprintf
      fmt
      "(%a)"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pattern_pp)
      patterns
  | Pat_constraint (p, typ) -> fprintf fmt "(%a : %a)" pattern_pp p core_type_pp typ
;;

let rec expression_pp (n_sp : int) fmt expr =
  let rec helper (n_sp, in_app) fmt expr =
    match expr with
    | Exp_type (e, t) -> fprintf fmt "(%a : %a)" (expression_pp n_sp) e core_type_pp t
    | Exp_constant c -> const_pp fmt c
    | Exp_ident id -> ident_pp fmt id
    | Exp_tuple exprs ->
      fprintf
        fmt
        "(%a)"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") (expression_pp n_sp))
        exprs
    | Exp_function _ ->
      let args, body = collect_fun_args expr [] in
      fprintf
        fmt
        "(fun %a -> %a)"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") pattern_pp)
        args
        (expression_pp n_sp)
        body
    | Exp_let (decl, e) -> decl_expression_pp fmt decl e n_sp
    | Exp_match (e, cases) ->
      fprintf fmt "match %a with@\n" (expression_pp n_sp) e;
      pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt "@\n")
        (fun fmt (pat, e) -> 
          fprintf fmt "%a| %a -> \n%a%a" 
          pp_spaces (n_sp) 
          pattern_pp pat
          pp_spaces (n_sp+2) 
          (expression_pp (n_sp+4)) e)
        fmt
        cases
    | Exp_ifthenelse (e1, e2, e3) ->
      fprintf
        fmt
        "(if %a\n%athen\n%a%a\n%aelse\n%a%a)"
        (expression_pp (n_sp+2)) e1
        pp_spaces (n_sp-2)
        pp_spaces n_sp
        (expression_pp (n_sp+2)) e2
        pp_spaces (n_sp-2)
        pp_spaces n_sp
        (expression_pp (n_sp+2)) e3
    | Exp_apply (Exp_ident op, e) when Base_lib.is_binop op ->
      fprintf fmt "(( %s ) %a)" op (expression_pp n_sp) e
    | Exp_apply (e1, e2) ->
      let pp_app fmt e1 e2 =
        if in_app
        then fprintf fmt "(%a %a)" (helper (n_sp, true)) e1 (helper (n_sp, true)) e2
        else fprintf fmt "%a %a" (helper (n_sp, true)) e1 (helper (n_sp, true)) e2
      in
      pp_app fmt e1 e2
    | Exp_list (e1, e2) -> fprintf fmt "(%a :: %a)" (expression_pp n_sp) e1 (expression_pp n_sp) e2
  in
  helper (n_sp, false) fmt expr

and decl_expression_pp fmt decl expr n_sp =
  match decl with
  | Decl (rf, vbs) ->
    fprintf
      fmt
      "let%a%a in\n%a%a"
      rec_flag_pp rf
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\nand ") (value_binding_pp n_sp)) vbs
      pp_spaces n_sp
      (expression_pp (n_sp+2)) expr

and value_binding_pp n_sp fmt vb =
  match vb.vb_expr with
  | Exp_function _ ->
    let args, body = collect_fun_args vb.vb_expr [] in
    fprintf
      fmt
      "%a %a =\n%a%a"
      pattern_pp vb.vb_pat
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") pattern_pp) args
      pp_spaces n_sp
      (expression_pp (n_sp+2)) body
  | _ -> fprintf fmt "%a = %a" pattern_pp vb.vb_pat (expression_pp (n_sp+2)) vb.vb_expr
;;

let structure_item_pp fmt item =
  match item with
  | Str_eval e -> fprintf fmt "%a;;" (expression_pp 2) e
  | Str_value decl ->
    (match decl with
     | Decl (rf, vbs) ->
       fprintf
         fmt
         "let%a%a\n;;\n"
         rec_flag_pp
         rf
         (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\nand ") (value_binding_pp 2)) vbs)
;;

let pp_program fmt prog =
  pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n") structure_item_pp fmt prog
;;
