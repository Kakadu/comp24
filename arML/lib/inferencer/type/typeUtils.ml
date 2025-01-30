(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open TypeTree

let type_vars =
  let rec helper acc = function
    | TVar n -> TypeVarSet.add n acc
    | TArr (left, right) -> helper (helper acc left) right
    | TList typ -> helper acc typ
    | TTuple typ_list -> List.fold_left helper acc typ_list
    | TGround _ -> acc
    | _ -> acc
  in
  helper TypeVarSet.empty
;;

let (@->) left right = TArr (left, right)

let get_ground_type_by_annotation = function
  | Ast.GTDInt -> GTInt
  | Ast.GTDBool -> GTBool
  | Ast.GTDChar -> GTChar
  | Ast.GTDString -> GTString
  | Ast.GTDUnit -> GTUnit
;;

let rec get_type_by_annotation = function
  | Ast.TDGround td -> TGround (get_ground_type_by_annotation td)
  | Ast.TDArrow (l, r) -> (get_type_by_annotation l) @-> (get_type_by_annotation r)
  | Ast.TDTuple (fst, snd, other) -> TTuple (get_type_by_annotation fst :: get_type_by_annotation snd :: List.map (fun x -> get_type_by_annotation x) other)
  | Ast.TDList ty -> TList (get_type_by_annotation ty)
  | Ast.TDPolymorphic -> (get_type_by_annotation (Ast.TDGround (Ast.TDInt))) (* !!! *)
;;
