(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Typedtree

let recalculate_vars typ =
  let insert (old_v, new_v) acc =
    match Base.Map.find acc old_v with
    | Some _ -> new_v, acc
    | None -> new_v + 1, Base.Map.update acc old_v ~f:(fun _ -> new_v)
  in
  let new_vars =
    let rec helper (last_v, acc) = function
      | TVar n -> insert (n, last_v) acc
      | TList t -> helper (last_v, acc) t
      | TTuple l -> List.fold_left helper (last_v, acc) l
      | TFunction (l, r) ->
        let new_last_v, new_acc = helper (last_v, acc) l in
        helper (new_last_v, new_acc) r
      | TInt | TBool | TUnit -> last_v, acc
    in
    let _, res = helper (0, Base.Map.empty (module Base.Int)) typ in
    res
  in
  let rec helper = function
    | TVar n -> tvar (Base.Map.find_exn new_vars n)
    | TList t -> tlist (helper t)
    | TTuple l -> ttuple (List.map helper l)
    | TFunction (l, r) -> tfunction (helper l) (helper r)
    | other -> other
  in
  helper typ
;;

let pp_type ppf typ =
  let rec helper ppf = function
    | TUnit -> Format.fprintf ppf "unit"
    | TInt -> Format.fprintf ppf "int"
    | TBool -> Format.fprintf ppf "bool"
    | TVar v ->
      let var_name = Char.chr (Char.code 'a' + v) in
      Format.fprintf ppf "'%c" var_name
    | TFunction (l, r) ->
      (match l with
       | TFunction (_, _) -> Format.fprintf ppf "(%a) -> %a" helper l helper r
       | _ -> Format.fprintf ppf "%a -> %a" helper l helper r)
    | TList t ->
      (match t with
       | TFunction (_, _) -> Format.fprintf ppf "(%a) list" helper t
       | _ -> Format.fprintf ppf "%a list" helper t)
    | TTuple tl ->
      Format.fprintf
        ppf
        "%a"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf " * ")
           (fun ppf ty ->
              match ty with
              | TTuple _ | TFunction _ -> Format.fprintf ppf "(%a)" helper ty
              | _ -> helper ppf ty))
        tl
  in
  helper ppf (recalculate_vars typ)
;;

let pp_error ppf = function
  | `Occurs_check (v, k) ->
    Format.fprintf ppf "The type variable %a occurs inside %a" pp_type (tvar v) pp_type k
  | `Unification_failed (l, r) ->
    Format.fprintf
      ppf
      "Type error: unification failed - type %a does not match expected type %a"
      pp_type
      l
      pp_type
      r
  | `Unbound_variable name -> Format.fprintf ppf "Type error: unbound variable '%s'" name
  | `Several_bounds name ->
    Format.fprintf ppf "Type error: variable '%s' is bound several times" name
  | `Not_impl -> Format.fprintf ppf "TODO: Not implemented."
;;

let print_inferencer_error e =
  let error_str = Format.asprintf "%a" pp_error e in
  Format.printf "%s\n" error_str
;;

let type_to_string typ = Format.asprintf "%a" pp_type typ
let expr_without_name typ = "- : " ^ type_to_string typ
let expr_with_name name typ = String.concat " " [ "val"; name; ":"; type_to_string typ ]
let print_expr_type typ = Format.printf "%s\n" (expr_without_name typ)

let print_program_type env names_list =
  Base.List.iter names_list ~f:(fun name ->
    let (Scheme (_, ty)) = Base.Map.find_exn env name in
    Format.printf "%s\n" (expr_with_name name ty))
;;
