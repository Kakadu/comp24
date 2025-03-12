type id = string [@@deriving show]

type value =
  | Int of int
  | Bool of bool
  | String of string
  | Unit
[@@deriving show]

(* let f (a: Int) (b: Int) = ...*)
type dataType =
  | PInt
  | PBool
  | PString
  | PUnit
  | PVar of id
  | PList of dataType
  | PTuple of dataType list
  | PArrow of dataType * dataType
[@@deriving show]

type bop =
  | ADD (* + *)
  | SUB (* - *)
  | MUL (* * *)
  | DIV (* / *)
  | EQ (* = *)
  | ID_EQ (* == *)
  | NEQ (* != *)
  | GT (* > *)
  | GTE (* >= *)
  | LT (* < *)
  | LTE (* <= *)
  | AND (* && *)
  | OR (* || *)
  | CONCAT (* ^ *)
[@@deriving show]

type uop =
  | NOT (** not true *)
  | UMINUS (* -10 *)
  | UPLUS (* +10 *)
[@@deriving show]

type op =
  | Binary of bop
  | Unary of uop
[@@deriving show]

type pattern =
  | Const of value
  | Var of id
  | Wildcard (* _ *)
  | Tuple of pattern * pattern * pattern list (* (1, 2, ...) *)
  | List of pattern list (* [1; 2; 3] *)
  | ListConcat of pattern * pattern (* 1::2::[] *)
  | Constraint of pattern * dataType (* a : Int *)
  | Operation of op (* let (+) x y = ... *)
[@@deriving show]

type expr =
  | EConst of value
  | EVar of id
  | EOperation of op (* 1 + 1 or (+) 1 1 *)
  | ETuple of expr * expr * expr list
  | EList of expr list
  | EListConcat of expr * expr (* 1::2::[] *)
  | EConstraint of expr * dataType (* f x : Int *)
  | Application of expr * expr
  | Let of funType * bind list * expr option (* let f = ... and g = ... [in ...] *)
  | Fun of args * expr (* (fun a -> a + 1) *)
  | If of expr * expr * expr option (* if a = b then c (else d) *)
  | Match of expr * case list
[@@deriving show]

and args = pattern list

(* name args = expr *)
and bind = pattern * args * expr
and case = pattern * expr

and funType =
  | Recursive
  | Nonrecursive
[@@deriving show]

type prog = expr list [@@deriving show]

module BinOperator = struct
  let list : bop list =
    [ ADD; SUB; MUL; DIV; EQ; ID_EQ; NEQ; GT; GTE; LT; LTE; AND; OR; CONCAT ]
  ;;

  let to_string : bop -> string = function
    | ADD -> "+"
    | SUB -> "-"
    | MUL -> "*"
    | DIV -> "/"
    | EQ -> "="
    | ID_EQ -> "=="
    | NEQ -> "!="
    | GT -> ">"
    | GTE -> ">="
    | LT -> "<"
    | LTE -> "<="
    | AND -> "&&"
    | OR -> "||"
    | CONCAT -> "^"
  ;;
end

module UnOperator = struct
  let list : uop list = [ NOT; UMINUS; UPLUS ]

  let to_string : uop -> string = function
    | NOT -> "not"
    | UMINUS -> "-"
    | UPLUS -> "+"
  ;;
end
