(** Copyright 2024, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Set of variables *)
open Format
open Ast

(* Base types *)
type base_type =
  | BInt (** Basic integer type *)
  | BBool (** Basic bool type *)
  | BUnit (** Unit type *)

(* Types *)
type ty =
  | TBase of base_type (** Type of integer *)
  | TVar of int (** Polymorphic type e.g. 'a *)
  | TArrow of ty * ty (** Type of function ty1 -> ty2 *)
  | TTuple of ty * ty * ty list (** Type of tuple *)
  | TList of ty (** Type of list *)

let map_type =
  let rec helper ast_type =
    match ast_type with
    | TInt -> TBase BInt
    | TBool -> TBase BBool
    | TUnit -> TBase BUnit
    | TFun (l, r) -> TArrow (helper l, helper r)
    | TList typ -> TList (helper typ)
    | TTuple (ty1, ty2, tys) ->
      TTuple (helper ty1, helper ty2, Base.List.map tys ~f:helper)
  in
  helper
;;

let map_var_types typ =
  let rec find_vars vars = function
    | TBase _ -> vars
    | TVar x -> Base.Set.add vars x
    | TArrow (l, r) ->
      let vars_l = find_vars vars l in
      find_vars vars_l r
    | TTuple (ty1, ty2, tys) ->
      let vars1 = find_vars vars ty1 in
      let vars2 = find_vars vars1 ty2 in
      Base.List.fold_left ~init:vars2 ~f:find_vars tys
    | TList typ -> find_vars vars typ
  in
  let vars = Base.Set.elements (find_vars (Base.Set.empty (module Base.Int)) typ) in
  let _, mapping =
    Base.List.fold_left
      ~init:(Char.code 'a', Base.Map.empty (module Base.Int))
      ~f:(fun (curr_char, mapping) var ->
        curr_char + 1, Base.Map.set mapping ~key:var ~data:(Char.chr curr_char))
      vars
  in
  mapping
;;

let pp_ty fmt ty =
  let pp_base_ty fmt = function
    | BInt -> fprintf fmt "int"
    | BBool -> fprintf fmt "bool"
    | BUnit -> fprintf fmt "unit"
  in
  let mapping = map_var_types ty in
  let rec helper fmt = function
    | TBase base_ty -> fprintf fmt "%a" pp_base_ty base_ty
    | TVar x -> fprintf fmt "'%c" (Base.Map.find_exn mapping x)
    | TArrow (l, r) ->
      (match l with
       | TArrow _ -> fprintf fmt "(%a) -> %a" helper l helper r
       | _ -> fprintf fmt "%a -> %a" helper l helper r)
    | TTuple (ty1, ty2, tys) ->
      fprintf
        fmt
        "%a"
        (pp_print_list
           ~pp_sep:(fun fmt _ -> fprintf fmt " * ")
           (fun fmt typ ->
             match typ with
             | TArrow _ -> fprintf fmt "(%a)" helper typ
             | _ -> fprintf fmt "%a" helper typ))
        (ty1 :: ty2 :: tys)
    | TList ty ->
      (match ty with
       | TArrow _ -> fprintf fmt "(%a) list" helper ty
       | _ -> fprintf fmt "%a list" helper ty)
  in
  helper fmt ty
;;

type error =
  | OccursCheckFailed of int * ty (** OCaml's Occurs check *)
  | UndeclaredVariable of id (** Attempt to use non-initialized variable *)
  | UnificationFailed of ty * ty (** Failed to unify left and right types *)
  | OrPatternBoundsDiff of id (** Variable id doesn't occure in some of Or patterns *)
  | OrPatternTypeDiff of id * ty * ty (** Types of some bounds in Or pattern differ *)
  | NotImplemented (** Still not implemented features *)
  | NotReachable
  (** For non reachable code. If it is reached, there are serious problems in your typechecker *)

let pp_error fmt = function
  | OccursCheckFailed (tv, ty) ->
    let mapping = map_var_types ty in
    fprintf
      fmt
      "The type variable '%c occurs inside %a"
      (Base.Map.find_exn mapping tv)
      pp_ty
      ty
  | UndeclaredVariable id -> fprintf fmt "Unbound value %s" id
  | UnificationFailed (l, r) ->
    fprintf fmt "Failed to unify types %a and %a" pp_ty l pp_ty r
  | OrPatternBoundsDiff id ->
    fprintf fmt "Variable %s doesn't occure in some 'or' patterns" id
  | OrPatternTypeDiff (id, ty1, ty2) ->
    fprintf
      fmt
      "Variable %s has different types in 'or' patterns: %a and %a are not equal"
      id
      pp_ty
      ty1
      pp_ty
      ty2
  | NotImplemented -> Stdlib.print_endline "Expression contains not implemented features"
  | NotReachable ->
    Stdlib.print_endline
      "This code must be not reachable. There are serious problems in your program"
;;
