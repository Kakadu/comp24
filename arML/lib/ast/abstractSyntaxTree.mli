(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type identifier = Id of string [@@deriving show { with_path = false }]

type constant =
  | CInt of int (** -1, 0, 1, ... *)
  | CBool of bool (** true, false *)
  | CChar of char (** 'a', 'b', 'c', ... *)
  | CString of string (** "Hello world!" *)
  | CUnit (** () *)
[@@deriving show { with_path = false }]

type ground_type_defenition =
  | GTDInt (** Int type (x: int) *)
  | GTDBool (** Bool type (x: bool) *)
  | GTDUnit (** Unit type (x: unit) *)
  | GTDChar (** Char type (x: char) *)
  | GTDString (** String type (x: string) *)
[@@deriving show { with_path = false }]

type type_defenition =
  | TDPolymorphic of identifier (** Polymorphic type (x: 'a) *)
  | TDGround of ground_type_defenition (** Int, Bool, Char, etc type *)
  | TDArrow of type_defenition * type_defenition (** Function type (f: (int -> int)) *)
  | TDTuple of type_defenition * type_defenition * type_defenition list
  (** Tuple type (x: (int, bool)) *)
  | TDList of type_defenition (** List type (x: int list) *)
[@@deriving show { with_path = false }]

type pattern =
  | PAny (** Wildcard: '_' *)
  | PNill (** Empty: '[]' *)
  | PConst of constant (** Any constant: '1', 'true', etc *)
  | PVar of identifier (** A variable pattern: 'x', 'y', etc *)
  | PTuple of pattern * pattern * pattern list (** Tuple of patterns: '(P1, P2, P3)' *)
  | PListConstructor of pattern * pattern (** List construction pattern: 'P1::P2' *)
  | PTyped of pattern * type_defenition (** Assigning a type to a pattern: (P : int) *)
[@@deriving show { with_path = false }]

(** Binding pattern with expression *)
type case = pattern * expression

and expression =
  | EConstant of constant (** Any constant: '1', 'true', etc *)
  | EIdentifier of identifier (** A variable name: 'x', 'y', etc *)
  | EFun of (pattern * pattern list) * expression (** Anonymous function: 'fun x -> x' *)
  | EFunction of case * case list
  (** Anonymous function with one argument and pattern matching: 'function | hd :: tl -> (hd, tl) | _ -> (0, 0)'*)
  | EApplication of expression * expression * expression list (** Application: f x *)
  | EIfThenElse of expression * expression * expression option
  (** if condition then true_branch else false branch (else option)*)
  | ETuple of expression * expression * expression list (** Tuple: '(E1, E2, ..., En)' *)
  | EListConstructor of expression * expression (** List construction: 1 :: 2 :: [] *)
  | EEmptyList (** Empty list: '[]' *)
  | EMatchWith of expression * case * case list
  (** Pattern matching: match x with | y -> y *)
  | ELetIn of case * case list * expression (** Let in: 'let f x = x in f 5 *)
  | ERecLetIn of case * case list * expression
  (** Recursive let in: 'let rec f x = if x = 1 then 1 else x + f (x-1)'*)
  | ETyped of expression * type_defenition
  (** Assigning a type to an expression: (expr : int) *)
[@@deriving show { with_path = false }]

type declaration =
  | DOrdinary of case * case list (** Top-level let-binding: 'let f x = x' *)
  | DRecursive of case * case list
  (** Top-level recursive let-binding: 'let rec f x = ...' *)
[@@deriving show { with_path = false }]

(** The entire parsed code of the program *)
type program = declaration list [@@deriving show { with_path = false }]
