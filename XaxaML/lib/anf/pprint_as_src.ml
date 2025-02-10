(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Ast

let const_to_str = function
  | C_bool b -> if b then "true" else "false"
  | C_int i -> Format.sprintf "%i" i
  | C_empty_list -> "[]"
  | C_unit -> "()"
;;

let rec pat_to_str = function
  | P_typed (p, _) -> pat_to_str p
  | P_const c -> const_to_str c
  | P_any -> "_"
  | P_val v -> v
  | P_tuple (p_hd, p_tl) ->
    Format.sprintf
      "(%s)"
      (pat_to_str p_hd ^ List.fold_left (fun acc p -> acc ^ ", " ^ pat_to_str p) "" p_tl)
  | P_cons_list (p1, p2) -> Format.sprintf "(%s::%s)" (pat_to_str p1) (pat_to_str p2)
;;

let rec expr_to_str = function
  | E_typed (e, _) -> expr_to_str e
  | E_const c -> const_to_str c
  | E_ident id -> id
  | E_ite (e1, e2, e3) ->
    Format.sprintf
      "if (%s) then (%s) else (%s)"
      (expr_to_str e1)
      (expr_to_str e2)
      (expr_to_str e3)
  | E_fun (first, other, e) ->
    let args = first :: other in
    Format.sprintf
      "(fun%s -> %s"
      (List.fold_left (fun acc p -> acc ^ " " ^ pat_to_str p) "" args)
      (expr_to_str e)
  | E_app (e1, e2) -> Format.sprintf "(%s %s)" (expr_to_str e1) (expr_to_str e2)
  | E_let (Non_rec (p, _, e1), e2) ->
    Format.sprintf "let %s = %s in %s" (pat_to_str p) (expr_to_str e1) (expr_to_str e2)
  | E_let (Rec decl_list, e2) ->
    let p, _, e = List.hd decl_list in
    let tl = List.tl decl_list in
    Format.sprintf "let %s = %s" (pat_to_str p) (expr_to_str e)
    ^ List.fold_left
        (fun acc (p, _, e) ->
          acc ^ Format.sprintf " and %s = %s" (pat_to_str p) (expr_to_str e))
        ""
        tl
    ^ Format.sprintf " in %s" (expr_to_str e)
  | E_match (e, case_list) ->
    Format.sprintf "match %s with " (expr_to_str e)
    ^ List.fold_left
        (fun acc (p, e) ->
          acc ^ Format.sprintf "| %s -> %s " (pat_to_str p) (expr_to_str e))
        ""
        case_list
  | E_cons_list (e1, e2) -> Format.sprintf "(%s::%s)" (expr_to_str e1) (expr_to_str e2)
  | E_tuple (e, e_list) ->
    Format.sprintf
      "(%s)"
      (expr_to_str e
       ^ List.fold_left
           (fun acc p -> acc ^ Format.sprintf ", %s" (expr_to_str p))
           ""
           e_list)
;;

let toplevel_src = function
  | Let_decl (Non_rec (p, _, e)) ->
    Format.sprintf "let %s = %s" (pat_to_str p) (expr_to_str e)
  | Let_decl (Rec decl_list) ->
    let p, _, e = List.hd decl_list in
    let tl = List.tl decl_list in
    Format.sprintf "let %s = %s" (pat_to_str p) (expr_to_str e)
    ^ List.fold_left
        (fun acc (p, _, e) ->
          acc ^ Format.sprintf "\nand %s = %s" (pat_to_str p) (expr_to_str e))
        ""
        tl
  | Expr e -> Format.sprintf "%s;;" (expr_to_str e)
;;

let rec print_splitted ppf str =
  let is_whitespace = function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false
  in
  let len = String.length str in
  if len >= 70
  then (
    let rec helper until =
      if until = len || is_whitespace str.[until] then until else helper (until + 1)
    in
    let until = helper 70 in
    Format.fprintf ppf "%s\n" (String.sub str 0 until);
    print_splitted ppf (String.sub str until (len - until)))
  else Format.fprintf ppf "%s" str
;;

let pp_expr_as_src ppf expr =
  let str = expr_to_str expr in
  print_splitted ppf str
;;

let pp_program_as_src ppf p =
  let str = List.fold_left (fun acc a -> acc ^ toplevel_src a ^ "\n") "" p in
  print_splitted ppf str
;;
