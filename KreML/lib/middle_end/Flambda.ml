(** Copyright 2024-2025, KreML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Anf

module StringSet = struct
  include Stdlib.Set.Make (String)

  (* let pp fmt s =
     let open Stdlib.Format in
     fprintf fmt "[ ";
    iter (fprintf fmt "%s; ") s;
    fprintf fmt "]"
     ;; *)
end

type flambda =
  | Fl_const of const
  | Fl_var of ident
  | Fl_binop of binop * flambda * flambda
  | Fl_getfield of int * flambda
  | Fl_cons of flambda * flambda
  | Fl_tuple of flambda list
  | Fl_app of flambda * flambda list
  | Fl_closure of closure
  | Fl_ite of flambda * flambda * flambda
  | Fl_let of ident * flambda * flambda

and closure =
  { name : ident
  ; env_size : int
  ; arrange : (int * flambda) list (* idx, value*)
  ; arity : int
  }

type fun_with_env =
  { arg : ident
  ; captured_args : ident list
  ; arity : int
  ; body : flambda
  }

type fl_fun =
  | Fun_with_env of fun_with_env
  | Fun_without_env of ident option * flambda (** [Fun_without_env(arg, body)] *)

type flstructure = (ident * fl_fun) list

let flvar id = Fl_var id

open Stdlib.Format

let rec pp_flambda ppf = function
  | Fl_const c -> Anf_printer.pp_const ppf c
  | Fl_var id -> fprintf ppf "%s" id
  | Fl_binop (binop, x, y) ->
    fprintf
      ppf
      "@[%a %s %a@]"
      pp_flambda
      x
      (Anf_printer.binop_to_string binop)
      pp_flambda
      y
  | Fl_getfield (idx, fl) -> fprintf ppf "@[%s %i %a@]" "getfield" idx pp_flambda fl
  | Fl_cons (x, xs) -> fprintf ppf "@[%a :: %a@]" pp_flambda x pp_flambda xs
  | Fl_tuple elems ->
    let pp_tuple ppf l = pp_list ppf l (fun e -> fprintf ppf "@[%a @]" pp_flambda e) in
    pp_tuple ppf elems
  | Fl_app (f, args) ->
    let pp_args ppf l = pp_list ppf l (fun e -> fprintf ppf "@[%a @]" pp_flambda e) in
    fprintf ppf "@[%a (%a)@]" pp_flambda f pp_args args
  | Fl_closure { name; env_size; arrange; arity } ->
    let pp_arange ppf list =
      List.iter (fun (idx, value) -> fprintf ppf "@[%i: %a; @]" idx pp_flambda value) list
    in
    fprintf
      ppf
      "@[{ name: %s, arity: %i env_size: %i, arrange [%a ]} @]"
      name
      arity
      env_size
      pp_arange
      arrange
  | Fl_ite (c, t, e) ->
    fprintf
      ppf
      "@[if %a @, then %a @, else @, %a @]"
      pp_flambda
      c
      pp_flambda
      t
      pp_flambda
      e
  | Fl_let (id, e, scope) ->
    fprintf ppf "@[let %s = %a in @, %a @]" id pp_flambda e pp_flambda scope

and pp_list ppf list elem_printer =
  fprintf ppf "[ ";
  List.iter elem_printer list;
  fprintf ppf " ]"
;;

let pp_fl_fun ppf = function
  | Fun_without_env (Some arg, body) -> fprintf ppf "@[(%s) {%a} @]@." arg pp_flambda body
  | Fun_without_env (None, body) -> fprintf ppf "@[() {%a} @]@." pp_flambda body
  | Fun_with_env { arg; captured_args; body; _ } ->
    let print_args ppf list = List.iter (fun s -> fprintf ppf "%s " s) list in
    fprintf ppf "@[(%a) {%a} @]@." print_args (captured_args @ [ arg ]) pp_flambda body
;;

let pp ppf flstructure =
  List.iter (fun (id, f) -> fprintf ppf "@[%s%a@]@." id pp_fl_fun f) flstructure
;;
