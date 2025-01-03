(** Copyright 2024-2025, Perevalov Efim, Dyachkov Vitaliy *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format

type binder = int

module VarSet = struct
  include Stdlib.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

type binder_set = VarSet.t

type ty =
  | TBool
  | TInt
  | TVar of binder * Ast.type_of_var
  | TArrow of ty * ty
  | TList of ty
  | TTuple of ty list

type error =
  [ `Occurs_check
  | `Empty_pattern
  | `No_variable of string
  | `Unification_failed of ty * ty
  ]

type scheme = S of binder_set * ty

let pp_list helper l sep =
  Format.pp_print_list ~pp_sep:(fun ppf _ -> Format.fprintf ppf sep) helper l
;;

let rec pp_typ ppf = function
  | TVar (n, _) -> fprintf ppf "%s" @@ "'" ^ Char.escaped (Char.chr (n + 97))
  | TInt -> fprintf ppf "int"
  | TBool -> fprintf ppf "bool"
  | TList t -> fprintf ppf "%a list" pp_typ t
  | TTuple ts -> fprintf ppf "(%a)" (fun ppf -> pp_list pp_typ ppf " * ") ts
  | TArrow (l, r) ->
    (match l with
     | TArrow (_, _) -> fprintf ppf "(%a) -> %a" pp_typ l pp_typ r
     | _ -> fprintf ppf "%a -> %a" pp_typ l pp_typ r)
;;

let pp_scheme ppf = function
  | S (xs, t) -> fprintf ppf "forall %a . %a" VarSet.pp xs pp_typ t
;;

let print_typ typ =
  let s = Format.asprintf "%a" pp_typ typ in
  Format.printf "%s\n" s
;;

let pp_error ppf : error -> _ = function
  | `Empty_pattern -> Format.fprintf ppf "Error: Empty pattern"
  | `Occurs_check -> Format.fprintf ppf "Error: Occurs check failed"
  | `No_variable s -> Format.fprintf ppf "Error: Undefined variable '%s'" s
  | `Unification_failed (l, r) ->
    Format.fprintf ppf "Error: Unification failed on %a and %a" pp_typ l pp_typ r
;;

let print_typ_err e = Format.printf "%a\n" pp_error e
