type id = string [@@deriving show]

(* default types *)
type dataType =
  | Int of int
  | Float of float
  | Bool of bool
  | Char of char
  | String of string
  | Unit
[@@deriving show]

(* let f (a: Int) (b: Int) = ...*)
type paramType =
  | PInt
  | PFloat
  | PBool
  | PChar
  | PString (* | Poly of string (a: `a) (b: `b) *)
[@@deriving show]

type bop =
  | ADD (** + *)
  | SUB (** - *)
  | MUL (** * *)
  | DIV (** / *)
  | EQ (** = *)
  | ID_EQ
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

(* Pattern is a value (dataType -> dataStructure) *)
type pattern =
  | Const of dataType
  | VarId of id
  | TypedVarID of id * paramType (* (a: int) *)
  | Wildcard (* _ *)
  | Tuple of expr list (* (1, 2, 3) *)
  | List of expr list (* [1; 2; 3] *)
  | ListConcat of expr * pattern (* a :: [b;c]*)
[@@deriving show]

and expr =
  | BinOp of bop * expr * expr
  | UnOp of uop * expr
  | Application of expr * expr
  | Pattern of pattern
  (* let id a = a | the first `pattern` is the pattern that can be used instead of function's name *)
  | Let of funType * pattern * pattern list * expr
  | LetAndIn of expr list * expr option (* let a = 1 and b = 2 in a + b *)
  | Fun of pattern list * expr (* (fun a -> a + 1) *)
  | If of expr * expr * expr option (* if a = b then c (else d) *)
  | Match of expr * (pattern * expr) list
[@@deriving show]

and funType =
  | Recursive
  | Nonrecursive
[@@deriving show]
