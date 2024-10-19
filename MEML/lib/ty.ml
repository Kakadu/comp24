(** Copyright 2023-2024, Perevalov Efim, Dyachkov Vitaliy *)

(** SPDX-License-Identifier: LGPL-3.0 *)

open Format

type binder = int [@@deriving eq, show { with_path = false }]

module VarSet = struct
  include Stdlib.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

type binder_set = VarSet.t [@@deriving show { with_path = false }]

type ty =
  | TPrim of string
  | TBool
  | TInt
  | TString
  | TUnknown
  | TVar of binder * ty
  | TArrow of ty * ty
  | TList of ty
  | TTuple of ty list
[@@deriving eq, show { with_path = false }]

type error =
  [ `Occurs_check
  | `No_variable of string
  | `Unification_failed of ty * ty
  ]

type scheme = S of binder_set * ty [@@deriving show { with_path = false }]

let arrow l r = TArrow (l, r)
let int_typ = TInt
let bool_typ = TBool
let str_typ = TString
let list_typ l = TList l
let tuple_typ t = TTuple t
let unit_typ = TUnknown
let v x = TVar (x, TUnknown)
let v1 x t = TVar (x, t)

let pp_list helper l sep =
  Format.pp_print_list ~pp_sep:(fun ppf _ -> Format.fprintf ppf sep) helper l
;;

let rec pp_typ ppf = function
  | TPrim s -> pp_print_string ppf s
  | TVar (n, _) -> fprintf ppf "%s" @@ "'" ^ Char.escaped (Char.chr (n + 97))
  | TInt -> fprintf ppf "int"
  | TBool -> fprintf ppf "bool"
  | TString -> fprintf ppf "string"
  | TList t -> fprintf ppf "%a list" pp_typ t
  | TTuple ts -> fprintf ppf "(%a)" (fun ppf -> pp_list pp_typ ppf " * ") ts
  | TArrow (l, r) ->
    (match l with
     | TArrow (_, _) -> fprintf ppf "(%a) -> %a" pp_typ l pp_typ r
     | _ -> fprintf ppf "%a -> %a" pp_typ l pp_typ r)
  | TUnknown -> fprintf ppf "()"
;;

let pp_scheme ppf = function
  | S (xs, t) -> fprintf ppf "forall %a . %a" VarSet.pp xs pp_typ t
;;

let print_typ typ =
  let s = Format.asprintf "%a" pp_typ typ in
  Format.printf "%s\n" s
;;

let pp_error ppf : error -> _ = function
  | `Occurs_check -> Format.fprintf ppf "Occurs check failed"
  | `No_variable s -> Format.fprintf ppf "Undefined variable '%s'" s
  | `Unification_failed (l, r) ->
    Format.fprintf ppf "unification failed on %a and %a" pp_typ l pp_typ r
;;

let print_typ_err e =
  let s = Format.asprintf "%a" pp_error e in
  Format.printf "%s\n" s
;;
