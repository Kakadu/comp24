(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type identifier = Id of string [@@deriving eq, ord, show { with_path = false }]

type constant =
  | CInt of int (** [52] *)
  | CString of string (** ["abc"] *)
  | CBool of bool (** [true] or [false] *)
  | CEmptyList (** [[]] *)
  | CUnit (** [()] *)
[@@deriving eq, show { with_path = false }]

type typ =
  | TInt (** [int] *)
  | TString (** [string] *)
  | TBool (** [bool] *)
  | TUnit (** [()] *)
  | TVar of identifier (** ['a] *)
  | TTuple of typ list (** [int * string], invariant: [list length >= 2] *)
  | TArrow of typ * typ (** [int -> string] *)
  | TList of typ (** [int list] *)
[@@deriving eq, show { with_path = false }]

type pattern =
  | PAny (* _ *)
  | PConst of constant (** [52] | ["hola"]*)
  | PVar of identifier (** [variable] *)
  | PTuple of pattern list (** [first, second, third], invariant: [list length >= 2] *)
  | PCons of pattern * pattern (** [1 :: []] *)
  | PType of pattern * typ (** [a : int] *)
[@@deriving eq, show { with_path = false }]

type rec_flag =
  | Recursive (** with [rec] modifier *)
  | Nonrecursive (** without [rec] modifier *)
[@@deriving eq, show { with_path = false }]

(** [let a = e;;] | [let fst, snd = pair in fst + snd] *)
type value_binding = pattern * expr [@@deriving eq, show { with_path = false }]

(** [Tree(left, right) -> left, right] *)
and case = pattern * expr [@@deriving eq, show { with_path = false }]

and expr =
  | EConst of constant (** [52] | ["hola"] *)
  | EVar of identifier (** [x] *)
  | ETuple of expr list (** [fst, snd, trd], invariant: [list length >= 2] *)
  | EFun of pattern list * expr (** [fun n -> n + 1] *)
  | ELet of rec_flag * value_binding * expr (** [let abc = "a" in abc] *)
  | EApp of expr * expr (** [f x] *)
  | EMatch of expr * case list (** [match x with | 1 -> true | _ -> false] *)
  | EIf of expr * expr * expr (** [if true then false else true] *)
  | ECons of expr * expr (** [[1; 2; 3]], [1 :: []]*)
  | EType of expr * typ (** [(g x: bool)] *)
[@@deriving eq, show { with_path = false }]

type structure_item =
  | SILet of rec_flag * value_binding list
  | SIExpr of expr (** [5 + 2] *)
[@@deriving eq, show { with_path = false }]

type structure = structure_item list [@@deriving eq, show { with_path = false }]
