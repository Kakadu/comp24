[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open LMisc

module Format = Stdlib.Format

module Var = struct
  module T = struct
    type t = V of string
    [@@deriving show {with_path= false}, ord, sexp_of, eq]
  end

  include T
  include Comparator.Make (T)
end

module VarSet = struct
  type t = (Var.t, Var.comparator_witness) Set.t

  let compare = Set.compare_direct
  let sexp_of_t set = Set.sexp_of_m__t (module Var) set

  let empty : t = Set.empty (module Var)
  let single x : t = Set.singleton (module Var) x

  let pp ppf (set : t) =
    let pp_contents =
      Format.pp_print_list
        ~pp_sep:(fun out () -> Format.fprintf out ",@ ")
        Var.pp
    in
    Format.fprintf ppf "{%a}" pp_contents (Set.to_list set)
end

module Ty = struct
  type t =
    | Var of Var.t  (** A type variable such as ['a] *)
    | Arr of t * t  (** [T1 -> T2] *)
    | Tuple of t List2.t  (** [T1 * ... * Tn] *)
    | Con of Id.t * t list
        (** [Con(tconstr, l)] represents:
          - [tconstr]               when [l=[]]
          - [T tconstr]             when [l=[T]]
          - [(T1, ..., Tn) tconstr] when [l=[T1, ..., Tn]]
        *)
  [@@deriving show {with_path= false}, ord, sexp_of, eq]

  let unit = Con (I "unit", [])
  let bool = Con (I "bool", [])
  let int = Con (I "int", [])
  let char = Con (I "char", [])
  let string = Con (I "string", [])

  let rec vars = function
    | Var var ->
        VarSet.single var
    | Arr (ty1, ty2) ->
        Set.union_list (module Var) [vars ty1; vars ty2]
    | Tuple tys ->
        List.map ~f:vars (List2.to_list tys) |> Set.union_list (module Var)
    | Con (_, tys) ->
        List.map ~f:vars tys |> Set.union_list (module Var)
end
