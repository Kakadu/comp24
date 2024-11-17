type id = string [@@deriving eq, show { with_path = false }]

type bin_op =
  | Add
  | Sub
  | Mul
  | Div (** / *)
  | Mod (** % *)
  | Les (** < *)
  | Leq (** <= *)
  | Gre (** > *)
  | Geq (** >= *)
  | Equ 
  | Neq (** <> *)
  | And 
  | Dsj (** || *)
[@@deriving show { with_path = false }]

type const =
  | CBool of bool
  | CInt of int
  | CUnit
[@@deriving show { with_path = false }]

type pat = 
  | PWild
  | PConst of const
  | PVar of id
[@@deriving show { with_path = false }]

type expr = 
  | EConst of const
  | EBinOp of bin_op * expr * expr
  | EVar of id
  | EIf of expr * expr * expr
  | EFun of pat * expr
  | ELetIn of bool * id * expr * expr
  | EApp of expr * expr
[@@deriving show { with_path = false }]

type binding = ELet of bool * id * expr
[@@deriving show { with_path = false }]

type program = binding list [@@deriving show { with_path = false }]

val constr_cint : int -> const
val constr_cbool : bool -> const

val constr_pwild : string -> pat
val constr_pconst : const -> pat
val constr_pvar : id -> pat

val constr_econst : const -> expr
val constr_ebinop : bin_op -> expr -> expr -> expr
val constr_evar : id -> expr
val constr_eif : expr -> expr -> expr -> expr
val constr_efun : pat list -> expr -> expr
val constr_eletin : bool -> id -> expr -> expr -> expr
val constr_eapp : expr -> expr list -> expr

val constr_elet : bool -> id -> expr -> binding