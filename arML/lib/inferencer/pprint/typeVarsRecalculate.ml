(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open TypeTree

(** Recalculates type variables in a type expression.
    This function renames type variables sequentially (starting from 0)
    to standardize type representations. *)
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
      | TTuple tl -> List.fold_left helper (last_v, acc) tl
      | TArr (l, r) ->
        let new_last_v, new_acc = helper (last_v, acc) l in
        helper (new_last_v, new_acc) r
      | TGround _ -> last_v, acc
    in
    let _, res = helper (0, Base.Map.empty (module Base.Int)) typ in
    res
  in
  let rec helper = function
    | TVar n -> TVar (Base.Map.find_exn new_vars n)
    | TList t -> TList (helper t)
    | TTuple tl -> TTuple (List.map helper tl)
    | TArr (l, r) -> TArr ((helper l), (helper r))
    | other -> other
  in
  helper typ
;;
