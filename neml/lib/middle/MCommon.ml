[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

open LMisc
open LAst

module IdSet = struct
  type t = (Id.t, Id.comparator_witness) Set.t

  let empty : t = Set.empty (module Id)
  let single x : t = Set.singleton (module Id) x

  let of_list : Id.t list -> t = Set.of_list (module Id)
end

module IdTagged = struct
  type tag = Gen | User [@@deriving eq]
  type t = Id.t * tag [@@deriving eq]
end

module FuncDef = struct
  type 'a t =
    | Func of {recf: Expr.rec_flag; id: IdTagged.t; args: Id.t List1.t; body: 'a}

  let to_stritem (to_expr : 'a -> Expr.t)
      (Func {recf; id= id, _; args; body} : 'a t) : StrItem.t =
    let efunc : Expr.t =
      Fun (List1.map args ~f:(fun id -> Pat.Var id), to_expr body)
    in
    Let (recf, List1.of_list_exn [Expr.{pat= Pat.Var id; expr= efunc}])
end

module Const = struct
  type t = Int of int | Char of char | String of string | Bool of bool | Unit
  [@@deriving show {with_path= false}]

  let to_expr : t -> Expr.t = function
    | Int i ->
        Const (Int i)
    | Char c ->
        Const (Char c)
    | String s ->
        Const (String s)
    | Bool b ->
        Construct (I (if b then "true" else "false"), None)
    | Unit ->
        Expr.unit

  let from_const : LAst.Const.t -> t = function
    | Int i ->
        Int i
    | Char c ->
        Char c
    | String s ->
        String s
end
