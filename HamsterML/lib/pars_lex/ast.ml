type dataType =
  | Int of int
  | Float of float
  | Bool of bool
  | Char of char
  | String of string

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

type uop =
  | MINUS (** -1 *)
  | NOT (** not true *)

(* Value is a value (dataType -> dataStructure) *)
type value =
  | Const of dataType
  | VarId of string
  | TypedVarID of string * dataType (** (a: int) *)
  | Wildcard
  | Tuple of value list (** check types with typecheker *)
  | List of value list
  | ListConcat of value * value (** a :: [b;c]*)

type expr =
  | BinOp of bop * expr * expr
  | UnOp of uop * expr
  | Application of expr * expr
  | Value of value
  | Let of funType * string * value list * expr (** let id a = a *)
  | Fun of value list * expr (** (fun a -> a + 1) *)
  | If of expr * expr * expr
  | Match of expr * (value * expr) list
  | LetIn of expr list * expr (** let a = 1 and b = 2 in a + b *)

and funType =
  | Recursive
  | Nonrecursive
