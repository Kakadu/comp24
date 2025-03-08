(** Copyright 2024-2025, Danil Slinchuk, Julia Kononova *)

(** SPDX-License-Identifier: MIT *)

(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Vars
open Smisc
module Format = Stdlib.Format

module Ty : sig
  type t =
    | Ty_var of Var.t  (** A type variable such as ['a] *)
    | Ty_arr of t * t  (** [Ty_arr(T1, Eff, T2)] represents [T1 -Eff-> T2] *)
    | Ty_tuple of t list2
        (** [Ty_tuple([T1 ; ... ; Tn])] represents [T1 * ... * Tn].
            Invariant: [n >= 2].
        *)
    | Ty_con of Ident.t * t list
        (** [Ty_con(ident, l)] represents:
              - [tconstr]               when [l=[]],
              - [T tconstr]             when [l=[T]],
              - [(T1, ..., Tn) tconstr] when [l=[T1 ; ... ; Tn]].
        *)

  val pp : Format.formatter -> t -> unit

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val sexp_of_t : t -> Sexp.t

  val unit : t
  val int : t
  val bool : t
  val char : t
  val string : t

  val vars : t -> VarSet.t
  (** Type variables occuring in a type *)
end = struct
  type t =
    | Ty_var of Var.t
    | Ty_arr of t * t
    | Ty_tuple of t list2
    | Ty_con of Ident.t * t list
  [@@deriving eq, ord, sexp_of]

  let rec pp ppf =
    let open Format in
    let pp_raw = pp in
    let pp ppf ty =
      match ty with
      | Ty_arr _ | Ty_tuple _ ->
          (* wrap arrow and tuple types in parentheses *)
          fprintf ppf "(%a)" pp_raw ty
      | _ ->
          pp_raw ppf ty
    in
    function
    | Ty_var var ->
        Var.pp ppf var
    | Ty_arr (l, r) ->
        fprintf ppf "%a -> %a" pp l pp_raw r
    | Ty_tuple tys ->
        let pp_tys =
          pp_print_list pp ~pp_sep:(fun out () -> fprintf out " * ")
        in
        fprintf ppf "%a" pp_tys (from_list2 tys)
    | Ty_con (Ident name, args) -> (
        let pp_args =
          pp_print_list pp ~pp_sep:(fun out () -> fprintf out ", ")
        in
        match args with
        | [] ->
            fprintf ppf "%s" name
        | [arg] ->
            fprintf ppf "%a %s" pp arg name
        | _ ->
            fprintf ppf "(%a) %s" pp_args args name )

  let unit = Ty_con (Ident "unit", [])
  let int = Ty_con (Ident "int", [])
  let bool = Ty_con (Ident "bool", [])
  let char = Ty_con (Ident "char", [])
  let string = Ty_con (Ident "string", [])

  let rec vars = function
    | Ty_var x ->
        VarSet.singleton x
    | Ty_arr (ty1, ty2) ->
        VarSet.union_list [vars ty1; vars ty2]
    | Ty_tuple tys ->
        List.map ~f:vars (from_list2 tys) |> VarSet.union_list
    | Ty_con (_, tys) ->
        List.map ~f:vars tys |> VarSet.union_list
end

module Scheme : sig
  (** Type with universally quantified type variables *)
  type t = Forall of VarSet.t * Ty.t

  val pp : Format.formatter -> t -> unit

  val compare : t -> t -> int
  val sexp_of_t : t -> Sexp.t

  val free_vars : t -> VarSet.t
  (** Free type variables in scheme *)
end = struct
  type t = Forall of VarSet.t * Ty.t [@@deriving ord, sexp_of]

  let pp ppf (Forall (quantified, ty)) =
    let open Format in
    if VarSet.is_empty quantified then fprintf ppf "%a" Ty.pp ty
    else fprintf ppf "%a. %a" VarSet.pp quantified Ty.pp ty

  let free_vars (Forall (quantified, ty)) = VarSet.diff (Ty.vars ty) quantified
end
