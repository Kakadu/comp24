(* Identifier, represented as a string *)
type id = string

(* Pretty-print an identifier *)
val pp_id : Format.formatter -> id -> unit

(* Convert an identifier to a string *)
val show_id : id -> string

(* Possible HamsterML values*)
type value =
  | Int of int (* Integer value *)
  | Bool of bool (* Boolean value *)
  | String of id (* String value *)
  | Unit (* Unit value *)

(* Pretty-print a value *)
val pp_value : Format.formatter -> value -> unit

(* Convert a value to a string *)
val show_value : value -> string

(* Types are used to explicitly specify types (in constraints). *)
type dataType =
  | PInt (* Integer type *)
  | PBool (* Boolean type *)
  | PString (* String type *)

(* Pretty-print a data type *)
val pp_dataType : Format.formatter -> dataType -> unit

(* Convert a data type to a string *)
val show_dataType : dataType -> string

(* Binary operators *)
type bop =
  | ADD (* Addition (+) *)
  | SUB (* Subtraction (-) *)
  | MUL (* Multiplication ( * ) *)
  | DIV (* Division (/) *)
  | EQ (* Equality (=) *)
  | ID_EQ (* Identity equality (==) *)
  | NEQ (* Inequality (!=) *)
  | GT (* Greater than (>) *)
  | GTE (* Greater than or equal (>=) *)
  | LT (* Less than (<) *)
  | LTE (* Less than or equal (<=) *)
  | AND (* Logical AND (&&) *)
  | OR (* Logical OR (||) *)
  | CONCAT (* String concatenation (^) *)

(* Pretty-print a binary operator *)
val pp_bop : Format.formatter -> bop -> unit

(* Convert a binary operator to a string *)
val show_bop : bop -> string

(* Unary operators *)
type uop =
  | NOT (* Logical negation (not) *)
  | UMINUS (* Unary minus (-10) *)
  | UPLUS (* Unary plus (+10) *)

(* Pretty-print a unary operator *)
val pp_uop : Format.formatter -> uop -> unit

(* Convert a unary operator to a string *)
val show_uop : uop -> string

(* Operator type: binary or unary *)
type op =
  | Binary of bop (* A binary operation *)
  | Unary of uop (* An unary operation *)

(* Pretty-print an operator *)
val pp_op : Format.formatter -> op -> unit

(* Convert an operator to a string *)
val show_op : op -> string

(* Pattern constructions *)
type pattern =
  | Const of value (* Constant *)
  | Var of id (* Variable *)
  | Wildcard (* Wildcard (_) *)
  | Tuple of pattern list (* Tuple (1, 2, ...) or 1, 2, 3 *)
  | List of pattern list (* List [1; 2; 3] *)
  | ListConcat of pattern * pattern (* Concatenation of lists *)
  | Constraint of pattern * dataType (* Type constraint (a : Int) *)
  | Operation of op (* Operator pattern, let (+) = ... *)

(* Pretty-print a pattern *)
val pp_pattern : Format.formatter -> pattern -> unit

(* Convert a pattern to a string *)
val show_pattern : pattern -> string

(* Expression constructions *)
type expr =
  | EConst of value (* Constant *)
  | EVar of id (* Variable *)
  | EOperation of op (* Operation (1 + 1 or (+) 1 2) *)
  | ETuple of expr list (* Tuple *)
  | EList of expr list (* List *)
  | EListConcat of expr * expr (* List concatenation *)
  | EConstraint of expr * dataType (* Type constraint (f x : Int) *)
  | Application of expr * expr (* Application (f x) *)
  | Let of funType * bind list * expr option (* Let-binding *)
  | Fun of args * expr (* Function (fun x y -> x + y) *)
  | If of expr * expr * expr option (* If-then-else *)
  | Match of expr * case list (* Pattern matching *)

(* Function arguments as a list of patterns *)
and args = pattern list

(* Function binding: name, arguments and body *)
and bind = pattern * args * expr

(* Match case: pattern and corresponding expression *)
and case = pattern * expr

(* Function definition type *)
and funType =
  | Recursive (* Recursive function *)
  | Nonrecursive (* Non-recursive function *)

(* Pretty-print expressions and related constructs *)
val pp_expr : Format.formatter -> expr -> unit
val show_expr : expr -> string
val pp_args : Format.formatter -> args -> unit
val show_args : args -> string
val pp_bind : Format.formatter -> bind -> unit
val show_bind : bind -> string
val pp_case : Format.formatter -> case -> unit
val show_case : case -> string
val pp_funType : Format.formatter -> funType -> unit
val show_funType : funType -> string

(* A program is a list of expressions *)
type prog = expr list

(* Pretty-print a program *)
val pp_prog : Format.formatter -> prog -> unit

(* Convert a program to a string *)
val show_prog : prog -> string
