open Format

type binder = int [@@deriving show { with_path = false }]

module VarSet : sig
  include module type of Stdlib.Set.Make (Int)

  val pp : formatter -> t -> unit
end

type binder_set = VarSet.t [@@deriving show { with_path = false }]

type typ =
  | TBool
  | TInt
  | TUnit
  | TVar of binder
  | TArrow of typ * typ
(* [@@deriving show { with_path = false }] *)

type err =
  [ `Occurs_check
  | `No_variable of string
  | `Unification_failed of typ * typ
  ]

type scheme = S of binder_set * typ

val arrow : typ -> typ -> typ
val int_typ : typ
val bool_typ : typ
val unit_typ : typ
val var : binder -> typ
val pp_typ : formatter -> typ -> unit
val pp_scheme : formatter -> scheme -> unit
val print_typ : typ -> unit
val pp_err : formatter -> err -> unit
val print_typ_err : err -> unit