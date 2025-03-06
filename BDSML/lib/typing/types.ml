(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

module TVarId = struct
  type t = int [@@deriving show { with_path = false }]

  let to_string n = Int.to_string n
  let create n : t = n
  let compare = Int.compare
  let add = ( + )
end

type base_type =
  | TInt
  | TChar
  | TString
  | TBool
  | TUnit

let show_base_type = function
  | TInt -> "int"
  | TChar -> "char"
  | TString -> "string"
  | TBool -> "bool"
  | TUnit -> "unit"
;;

let pp_base_type fmt ty = Format.fprintf fmt "%s" @@ show_base_type ty

type type_val =
  | TVar of TVarId.t (** e.g. ['a] *)
  | TBase of base_type (** e.g. [int] *)
  | TConstructor of type_val option * string (** e.g. [int list] *)
  | TTuple of type_val list (** e.g. [int * int] *)
  | TArrow of type_val * type_val (** e.g. [int -> int] *)

let enclose s = "(" ^ s ^ ")"

let rec show_type_val = function
  | TBase b -> show_base_type b
  | TArrow (l, r) ->
    let modifier =
      match l with
      | TArrow _ -> enclose
      | _ -> Fun.id
    in
    modifier (show_type_val l) ^ " -> " ^ show_type_val r
  | TTuple l -> enclose @@ String.concat " * " (List.map show_type_val l)
  | TVar id ->
    let alph_size = Char.code 'z' - Char.code 'a' in
    let symb = Char.chr @@ (Char.code 'a' + (id mod alph_size)) in
    let count = (id / alph_size) + 1 in
    "'" ^ String.make count symb
  | TConstructor (Some t1, name) -> show_type_val t1 ^ " " ^ name
  | TConstructor (None, name) -> name
;;

let pp_type_val fmt ty = Format.fprintf fmt "%s" @@ show_type_val ty

type error =
  | Unification_failed of type_val * type_val
  | Occurs_check
  | No_variable of string
  | Invalid_let
  | Invalid_list_constructor_argument
  | Invalid_ast of string
  | Invalid_predefined_operators of string

module Monads =
  Utils.Counter_monad.Make
    (TVarId)
    (struct
      type t = error
    end)

module VarSet = Set.Make (TVarId)

let rec occurs_in (v : TVarId.t) = function
  | TVar b -> b = v
  | TConstructor (Some l, _) -> occurs_in v l
  | TTuple l -> List.fold_left (fun acc el -> acc || occurs_in v el) false l
  | TArrow (l, r) -> occurs_in v l || occurs_in v r
  | _ -> false
;;

let free_vars =
  let rec helper acc = function
    | TVar b -> VarSet.add b acc
    | TArrow (l, r) -> helper (helper acc l) r
    | TTuple l -> List.fold_left (fun acc l -> helper acc l) acc l
    | TConstructor (Some l, _) -> helper acc l
    | _ -> acc
  in
  helper VarSet.empty
;;
