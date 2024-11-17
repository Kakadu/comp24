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

let constr_cint n = CInt n
let constr_cbool b = CBool b

let constr_pwild _ = PWild
let constr_pconst c = PConst c
let constr_pvar id = PVar id

let constr_econst c = EConst c
let constr_ebinop op e1 e2 = EBinOp (op, e1, e2)
let constr_evar id = EVar id
let constr_eif e1 e2 e3 = EIf (e1, e2, e3)
let constr_efun ps e = List.fold_right (fun p e -> EFun (p, e)) ps e
let constr_eletin is_rec id e1 e2 = ELetIn (is_rec, id, e1, e2)
let constr_eapp f args = List.fold_left (fun f arg -> EApp (f, arg)) f args

let constr_elet is_rec id e = ELet (is_rec, id, e)