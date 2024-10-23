type id = string [@@deriving show]

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
  | PInt
  | PFloat
  | PBool
  | PChar
  | PString
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

type uop = NOT (** not true *) [@@deriving show]

(* Value is a value (dataType -> dataStructure) *)
type value =
  | Const of dataType
  | VarId of id
  | TypedVarID of id * paramType (* (a: int) *)
  | Wildcard (* _ *)
  | Tuple of value list (* (1, 2, 3) *)
  | List of value list (* [1; 2; 3] *)
  | ListConcat of value * value (* a :: [b;c]*)
[@@deriving show]

type expr =
  | BinOp of bop * expr * expr
  | UnOp of uop * expr
  | Application of expr * expr
  | Value of value
  | Let of funType * id * value list * expr (* let id a = a *)
  | Fun of value list * expr (* (fun a -> a + 1) *)
  | If of expr * expr * expr option (* if a = b then c (else d) *)
  | Match of expr * (value * expr) list
  | LetIn of expr list * expr (* let a = 1 and b = 2 in a + b *)
[@@deriving show]

and funType =
  | Recursive
  | Nonrecursive
[@@deriving show]
