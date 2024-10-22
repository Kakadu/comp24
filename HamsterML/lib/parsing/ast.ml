(* default types *)
type dataType =
  | Int of int
  | Float of float
  | Bool of bool
  | Char of char
  | String of string
[@@deriving show]

(* let f (a: Int) (b: Int) = ...*)
type paramType =
  | Int
  | Float
  | Bool
  | Char
  | String
[@@deriving show]

type bop =
  | ADD (** + *)
  | SUB (** - *)
  | MUL (** * *)
  | DIV (** / *)
  | EQ (** = *)
  | NEQ (** != *)
  | GT (** > *)
  | GTE (** >= *)
  | LT (** < *)
  | LTE (** <= *)
  | AND (** && *)
  | OR (** || *)
  | CONCAT (** ^ *)
  | ASSIGN (** =  { let a = 1 } *)
[@@deriving show]

type uop =
  | MINUS (** -1 *)
  | NOT (** not true *)
[@@deriving show]

(* Value is a value (dataType -> dataStructure) *)
type value =
  | Const of dataType
  | VarId of string
  | TypedVarID of string * paramType (** (a: int) *)
  | Wildcard  (* _ *)
  | Tuple of value list (** check types with typecheker *)
  | List of value list
  | ListConcat of value * value (** a :: [b;c]*)
[@@deriving show]

type expr =
  | BinOp of bop * expr * expr
  | UnOp of uop * expr
  | Application of expr * expr
  | Value of value
  | Let of funType * string * value list * expr (** let id a = a *)
  | Fun of value list * expr (** (fun a -> a + 1) *)
  (* TODO: change to make possible for "if a then b" without else clause *)
  | If of expr * expr * expr
  | Match of expr * (value * expr) list
  | LetIn of expr list * expr (** let a = 1 and b = 2 in a + b *)
[@@deriving show]

and funType =
  | Recursive
  | Nonrecursive
[@@deriving show]
