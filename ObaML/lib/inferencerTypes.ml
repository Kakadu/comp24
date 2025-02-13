(** Copyright 2025, tepa46 *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Format
module VarSet = Stdlib.Set.Make (Int)
module VarMap = Stdlib.Map.Make (Int)
module StringMap = Stdlib.Map.Make (String)

type ty =
  | ITVar of int
  | ITPrim of string
  | ITArr of ty * ty
  | ITTuple of ty list
  | ITList of ty

let type_var x = ITVar x
let tprim_int = ITPrim "int"
let tprim_bool = ITPrim "bool"
let tprim_string = ITPrim "string"
let tprim_unit = ITPrim "unit"
let tarr l r = ITArr (l, r)
let ( @-> ) = tarr
let tlist ty = ITList ty
let ttuple ty_lst = ITTuple ty_lst

let rec pp_ty_tuple fmt = function
  | [] -> ()
  | [ h ] ->
    (match h with
     | ITArr (_, _) -> fprintf fmt "(%a)" pp_ty h
     | _ -> fprintf fmt "%a" pp_ty h)
  | h :: tl ->
    (match h with
     | ITArr (_, _) -> fprintf fmt "(%a) * %a" pp_ty h pp_ty_tuple tl
     | _ -> fprintf fmt "%a * %a" pp_ty h pp_ty_tuple tl)

and pp_ty fmt = function
  | ITVar num -> fprintf fmt "'%d" num
  | ITPrim str -> fprintf fmt "%s" str
  | ITArr (ty1, ty2) ->
    (match ty1, ty2 with
     | ITArr (_, _), _ -> fprintf fmt "(%a) -> %a" pp_ty ty1 pp_ty ty2
     | _ -> fprintf fmt "%a -> %a" pp_ty ty1 pp_ty ty2)
  | ITTuple ty_lst -> fprintf fmt "%a" pp_ty_tuple ty_lst
  | ITList ty1 ->
    (match ty1 with
     | ITVar _ | ITPrim _ -> fprintf fmt "%a list" pp_ty ty1
     | _ -> fprintf fmt "(%a) list" pp_ty ty1)
;;

type scheme = Scheme of VarSet.t * ty

type error =
  [ `Occurs_check
  | `Unification_failed of ty * ty
  | `Wrong_exp
  | `Wrong_type
  | `Unbound_variable of string
  | `Pattern_matching_failed
  | `Pattern_multi_bound of string
  ]

let pp_inf_err fmt = function
  | `Occurs_check -> fprintf fmt "Occurs_check"
  | `Unification_failed (typ1, typ2) ->
    fprintf fmt "Unification_failed: %a # %a" pp_ty typ1 pp_ty typ2
  | `Wrong_exp -> fprintf fmt "Wrong_exp"
  | `Wrong_type -> fprintf fmt "Wrong_type"
  | `Unbound_variable str -> fprintf fmt "Unbound_variable: %S" str
  | `Pattern_matching_failed -> fprintf fmt "Pattern_matching_failed"
  | `Pattern_multi_bound str -> fprintf fmt "Pattern_matching_failed: %S" str
;;
