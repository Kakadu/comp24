type id = string [@@deriving eq, show { with_path = false }]

type rec_flag =
  | Rec
  | NonRec
[@@deriving show { with_path = false }]

type constant =
  | CInt of int (** 42 *)
  | CBool of bool (** true | false *)
  | CUnit (** () *)
[@@deriving show { with_path = false }]

type unary_operator = Neg | Not [@@deriving show { with_path = false }]

type binary_operator =
  | Add (** + *)
  | Sub (** - *)
  | Mul (** * *)
  | Div (** / *)
  | Eq (** == *)
  | Ne (** != *)
  | Gt (** > *)
  | Lt (** < *)
  | Ge (** >= *)
  | Le (** <= *)
  | And (** && *)
  | Or (** || *)
[@@deriving show { with_path = false }]

type type_ann =
  | TInt (** int *)
  | TBool (** bool *)
  | TUnit (** () *)
  | TFun of type_ann * type_ann (** 'a -> 'b *)
[@@deriving show { with_path = false }]

type pattern = 
  | PConst of constant 
  | PWild (** _ *)
  | PIdent of id * type_ann option (** x | (x:int) *)
[@@deriving show { with_path = false }]

type expr =
  | EConst of constant (** 42, true, ()*)
  | EVar of id (** x *)
  | EUnaryOp of unary_operator * expr (** -x *)
  | EBinaryOp of binary_operator * expr * expr (** x + y *)
  | EApp of expr * expr (** f x *)
  | EIfElse of expr * expr * expr (** if x then y else z *)
  | EFun of pattern * expr (** fun x -> y *)
  | ELetIn of definition * expr (** let x = y in z *)
[@@deriving show { with_path = false }]

and definition =
  | DLet of rec_flag * pattern * expr (** let [rec] x = y *)
[@@deriving show { with_path = false }]

and program = definition list [@@deriving show { with_path = false }]

let p_ident i = PIdent(i, None)
let p_ident i ty = PIdent(i, ty)
let e_var v = EVar v
let e_constant c = EConst c
let e_var v = EVar v
let e_unary_op op e = EUnaryOp (op, e)
let e_binary_op op e1 e2 = EBinaryOp (op, e1, e2)
let e_app e1 e2 = EApp (e1, e2)
let e_if_else c t e = EIfElse (c, t, e)
let e_fun p e = EFun (p, e)
let e_let_in d e = ELetIn (d, e)
let d_let r p e = DLet (r, p, e)
