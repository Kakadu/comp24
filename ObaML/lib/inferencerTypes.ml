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

let rec pretty_pp_ty_tuples fmt acc =
  let typ, mp = acc in
  match typ with
  | [] -> ()
  | [ h ] ->
    (match h with
     | ITArr (_, _) -> fprintf fmt "(%a)" pretty_pp_ty (h, mp)
     | _ -> fprintf fmt "%a" pretty_pp_ty (h, mp))
  | h :: tl ->
    (match h with
     | ITArr (_, _) -> fprintf fmt "(%a) * %a" pretty_pp_ty (h, mp) pretty_pp_ty_tuples (tl, mp)
     | _ -> fprintf fmt "%a * %a" pretty_pp_ty (h, mp) pretty_pp_ty_tuples (tl, mp))

and pretty_pp_ty fmt acc =
  let typ, mp = acc in
  match typ with
  | ITVar num ->
    (match VarMap.find_opt num mp with
     | Some x -> fprintf fmt "'%s" x
     | None -> fprintf fmt "'%d" num)
  | ITPrim str -> fprintf fmt "%s" str
  | ITArr (ty1, ty2) ->
    (match ty1, ty2 with
     | ITArr (_, _), _ -> fprintf fmt "(%a) -> %a" pretty_pp_ty (ty1, mp) pretty_pp_ty (ty2, mp)
     | _ -> fprintf fmt "%a -> %a" pretty_pp_ty (ty1, mp) pretty_pp_ty (ty2, mp))
  | ITTuple ty_lst -> fprintf fmt "%a" pretty_pp_ty_tuples (ty_lst, mp)
  | ITList ty1 ->
    (match ty1 with
     | ITVar _ | ITPrim _ -> fprintf fmt "%a list" pretty_pp_ty (ty1, mp)
     | _ -> fprintf fmt "(%a) list" pretty_pp_ty (ty1, mp))
;;

type scheme = Scheme of VarSet.t * ty

type error =
  [ `Occurs_check
  | `Unification_failed of ty * ty
  | `Unexpected_type
  | `Unbound_variable of string
  | `Several_bound of string
  | `WrongRecursiveValueBinding
  ]

let pp_inf_err fmt = function
  | `Occurs_check -> fprintf fmt "Occurs check error"
  | `Unification_failed (typ1, typ2) ->
    fprintf
      fmt
      "Unification failed: %a and %a"
      pretty_pp_ty
      (typ1, VarMap.empty)
      pretty_pp_ty
      (typ2, VarMap.empty)
  | `Unexpected_type -> fprintf fmt "Unexpected type"
  | `Several_bound str ->
    fprintf fmt "Variable %S is bound several times in this matching" str
  | `Unbound_variable str -> fprintf fmt "Unbound value %S" str
  | `WrongRecursiveValueBinding ->
    fprintf fmt "Only variables are allowed as left-hand side of `let rec'"
;;
