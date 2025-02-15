(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

module VarId = struct
  type t = int [@@deriving show { with_path = false }]

  let to_string n = Int.to_string n
  let create n : t = n
  let compare = Int.compare
  let ( + ) = ( + )
end

type base_type =
  | TInt
  | TChar
  | TString
[@@deriving show { with_path = false }]

type type_val =
  | TVar of VarId.t (** e.g. ['a] *)
  | TBase of base_type (** e.g. [int] *)
  | TParams of type_val * type_val (** e.g. [int list] *)
  | TTuple of type_val list (** e.g. [int * int] *)
  | TArrow of type_val * type_val (** e.g. [int -> int] *)
[@@deriving show { with_path = false }]

type error =
  | Unification_failed of type_val * type_val
  | Occurs_check

exception Unimplemented of string

module VarSet = Set.Make (VarId)

module Scheme = struct
  type t = VarSet.t * type_val

  let rec occurs_in v = function
    | TVar b -> b = v
    | TTuple (f :: tl) -> occurs_in v f || (occurs_in v @@ TTuple tl)
    | TArrow (l, r) -> occurs_in v l || occurs_in v r
    | TBase _ -> false
    | _ -> raise (Unimplemented "occurs_in")
  ;;

  let free_vars =
    let rec helper acc = function
      | TVar b -> VarSet.add b acc
      | TArrow (l, r) -> helper (helper acc l) r
      | TBase _ -> acc
      | _ -> raise (Unimplemented "occurs_in")
    in
    helper VarSet.empty
  ;;
end

module TypeEnv = struct
  module Map = Map.Make (String)

  type t = Scheme.t Map.t

  let extend map name scheme = Map.add name scheme map
  let empty : t = Map.empty

  let init values =
    let rec helper map = function
      | (k, v) :: tl -> helper (extend map k v) tl
      | _ -> map
    in
    helper empty values
  ;;
end
