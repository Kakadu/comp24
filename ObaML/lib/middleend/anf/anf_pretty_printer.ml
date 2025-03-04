(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Format

let print_id fmt id = fprintf fmt "%s" id

let rec print_id_lst fmt ident_lst =
  match ident_lst with
  | [] -> ()
  | [ h ] -> fprintf fmt "%a " print_id h
  | h :: tl -> fprintf fmt "%a %a" print_id h print_id_lst tl
;;

let rec print_tuple fmt = function
  | [] -> ()
  | [ h ] -> fprintf fmt "%a" print_immexpr h
  | h :: tl -> fprintf fmt "%a, %a" print_immexpr h print_tuple tl

and print_immexpr fmt immexpr =
  match immexpr with
  | Anf.ImmId s -> fprintf fmt "%s" s
  | Anf.ImmInt n -> fprintf fmt "%d" n
  | Anf.ImmString s -> fprintf fmt "%S" s
  | Anf.ImmBool b -> fprintf fmt "%b" b
  | Anf.ImmEmptyList -> fprintf fmt "[]"
  | Anf.ImmUnit -> fprintf fmt "()"
  | Anf.ImmTuple im_lst -> fprintf fmt "(%a)" print_tuple im_lst

and print_immexpr_lst fmt = function
  | [] -> ()
  | [ h ] -> fprintf fmt "%a" print_immexpr h
  | h :: tl -> fprintf fmt "%a %a" print_immexpr h print_immexpr_lst tl
;;

let rec print_cexpr fmt cexpr =
  match cexpr with
  | Anf.CImmExpr immexpr -> fprintf fmt "%a" print_immexpr immexpr
  | Anf.CApp (app_var, immexpr_lst) ->
    fprintf fmt "(%s %a)" app_var print_immexpr_lst immexpr_lst
  | Anf.CIf (imm1, aex2, aex3) ->
    fprintf
      fmt
      "\n\tif %a\n\tthen %a\n\telse %a"
      print_immexpr
      imm1
      print_aexpr
      aex2
      print_aexpr
      aex3
  | Anf.CCons (imm1, imm2) -> fprintf fmt "%a :: %a" print_immexpr imm1 print_immexpr imm2

and print_aexpr fmt aexpr =
  match aexpr with
  | Anf.ALet (id, cexpr, aexpr) ->
    fprintf fmt "\n\tlet %a = %a in %a" print_id id print_cexpr cexpr print_aexpr aexpr
  | Anf.ACExpr cexpr -> fprintf fmt "%a" print_cexpr cexpr
;;

let print_value_binding fmt value_binding =
  let ident, args, aexpr = value_binding in
  fprintf fmt "%a %a= %a" print_id ident print_id_lst args print_aexpr aexpr
;;

let rec print_value_binding_lst fmt value_binding_lst =
  match value_binding_lst with
  | [] -> ()
  | [ h ] -> fprintf fmt "%a" print_value_binding h
  | h :: tl -> fprintf fmt "%a\nand%a" print_value_binding h print_value_binding_lst tl
;;

let print_program fmt program =
  List.iter
    (fun tp ->
      let rec_flag, value_bindings = tp in
      match rec_flag with
      | false -> fprintf fmt "let %a;;\n\n" print_value_binding_lst value_bindings
      | true -> fprintf fmt "let rec %a;;\n\n" print_value_binding_lst value_bindings)
    program
;;
