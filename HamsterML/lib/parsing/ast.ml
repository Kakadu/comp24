type id = string [@@deriving show]

type value =
  | Int of int
  | Float of float
  | Bool of bool
  | Char of char
  | String of string
  | Unit
[@@deriving show]

(* let f (a: Int) (b: Int) = ...*)
type dataType =
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
[@@deriving show]

type uop = NOT (** not true *) [@@deriving show]

type op =
  | Binary of bop
  | Unary of uop
[@@deriving show]

(* Pattern is a value (dataType -> dataStructure) *)
type pattern =
  | Const of value
  | Var of id
  | TypedVar of id * dataType (* (a: int) *)
  | Wildcard (* _ *)
  | Tuple of pattern list (* (1, 2, 3) *)
  | List of pattern list (* [1; 2; 3] *)
  | ListConcat of pattern * pattern
  | Operation of op
[@@deriving show]

type expr =
  | Pattern of pattern
  | Application of expr * expr
  | Constraint of expr * dataType
  | Let of funType * bind list * expr option (* let f = ... and g = ... [in ...] *)
  | Fun of args * expr (* (fun a -> a + 1) *)
  | If of expr * expr * expr option (* if a = b then c (else d) *)
  | Match of expr * case list
[@@deriving show]

and args = pattern list
and bind = pattern * args * expr
and case = pattern * expr

and funType =
  | Recursive
  | Nonrecursive
[@@deriving show]

type prog = expr list [@@deriving show]