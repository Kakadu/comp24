(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module VarSet = struct
  include Stdlib.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

type binder = int [@@deriving eq, show { with_path = false }]
type binder_set = VarSet.t [@@deriving show { with_path = false }]

type ty =
  | TPrim of string (** Ground types: [int], [bool] *)
  | TVar of binder (** Variables: ['a], ['b] *)
  | TTuple of ty list (** [TTuple [t1;..;tn]] ==> [(t1 * ... * tn)]*)
  | TArrow of ty * ty (** [TArrow (t1, t2)] ==> [e1 -> e2] *)
  | TList of ty (** [['a; 'b; ...]] *)
[@@deriving eq, show { with_path = false }]

type scheme = S of binder_set * ty [@@deriving show { with_path = false }]
