(* Based on https://gitlab.com/Kakadu/fp2020course-materials/-/blob/master/code/miniml*)
open Base

type var_id = int [@@deriving show { with_path = false }]

type ground =
  | TInt
  | TBool
  | TUnit
[@@deriving show { with_path = false }, eq]

type ty =
  | TGround of ground
  | TVar of var_id
  | TArrow of ty * ty
[@@deriving show { with_path = false }]

type error =
  [ `Occurs_check
  | `No_variable of string
  | `Unification_failed of ty * ty
  | `TODO
  ]

let arrow l r = TArrow (l, r)
let var x = TVar x
let ( => ) = arrow

type var_id_set = (var_id, Int.comparator_witness) Set.t
type scheme = var_id_set * ty
