(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type constant =
  | Const_int of int (** Integer literal, e.g. [69] *)
  | Const_char of char (** Character literal, e.g. ['m'] *)
  | Const_string of string (** String literal, e.g. ["something"] *)
  | Const_bool of bool (** Bool literal, [true] or [false] *)
  | Const_unit (** Unit literal, [()] *)
[@@deriving show { with_path = false }]

type typexpr =
  | Type_constructor_param of typexpr * string (** e.g. [int list] *)
  | Type_tuple of typexpr list (** e.g. [int * int] *)
  | Type_single of string (** e.g. [int] *)
  | Type_fun of typexpr list (** e.g. [int -> int] *)
[@@deriving show { with_path = false }]

type rec_flag =
  | Nonrecursive
  | Recursive
[@@deriving show { with_path = false }]

type pattern =
  | Pat_any (** Pattern any [_] *)
  | Pat_var of string (** Var pattern, e.g. [x] *)
  | Pat_type of pattern * typexpr (**Pattern with type, e.g. [x:int]*)
  | Pat_constant of constant (** Constant patterns, e.g. [69], ['m'], ["something"] *)
  | Pat_tuple of pattern list
  (** Pattern for many elements, e.g. [P1, ..., Pn] ([n >= 2]) *)
  | Pat_or of pattern * pattern (** Pattern for one of elements, e.g. [P1 | P2] *)
  | Pat_construct of string * pattern option
  (** [Pat_construct(C, args)] represents:
      - [C]   when [args] is [None]
      - [C P] when [args] is [Some P] *)
[@@deriving show { with_path = false }]

type let_binding =
  | Pat_binding of pattern * expression (** e.g. [let (a, b) = (1, 2)] *)
  | Val_binding of string * pattern list * expression (** e.g. [let f a b = a + b] *)
[@@deriving show { with_path = false }]

and case =
  { left : pattern
  ; right : expression
  }
[@@deriving show { with_path = false }]

and expression =
  | Exp_ident of string (** Identifiers, e.g. [some_var] *)
  | Exp_constant of constant (** Expression constant, e.g. [69], ['m'], ["something"] *)
  | Exp_type of expression * typexpr (** Expression with type, e.g. [69:int] *)
  | Exp_let of rec_flag * let_binding list * expression
  (** [Exp_let(flag, [(P1,E1) ; ... ; (Pn,En)], E)] represents:
      - [let P1 = E1 and ... and Pn = EN in E]     when [flag] is [Nonrecursive]
      - [let rec P1 = E1 and ... and Pn = EN in E] when [flag] is [Recursive].
        Invariant: [n >= 1] *)
  | Exp_fun of pattern list * expression
  (** [Exp_fun ([P1; ...; Pn], E)] represents [fun P1 ... Pn -> E].
      Invariant: [n >= 1] *)
  | Exp_function of case list
  (** [Exp_function([C1; ...; Cn])] represents [function C1 | ... | Cn].
      Invariant: [n >= 1] *)
  | Exp_apply of expression * expression (** [Exp_apply(E0, E1)] represents [E0 E1] *)
  | Exp_match of expression * case list
  (** [match E0 with P1 -> E1 | ... | Pn -> En]. Invariant: [n >= 1] *)
  | Exp_tuple of expression list (** Expressions [(E1, ..., En)]. Invariant: [n >= 2] *)
  | Exp_construct of string * expression option
  (** [Exp_construct(C, exp)] represents:
      - [C]               when [exp] is [None],
      - [C E]             when [exp] is [Some E],
      - [C (E1, ..., En)] when [exp] is [Some (Exp_tuple[E1;...;En])] *)
  | Exp_if of expression * expression * expression option (** [if E1 then E2 else E3] *)
[@@deriving show { with_path = false }]

type structure_item =
  | Str_eval of expression (** [Expression] *)
  | Str_value of rec_flag * let_binding list
  (** [Str_value(flag, [(P1, E1) ; ... ; (Pn, En)])] represents:
      - [let P1 = E1 and ... and Pn = EN]      when [flag] is [Nonrecursive]
      - [let rec P1 = E1 and ... and Pn = EN ] when [flag] is [Recursive].
        Invariant: [n >= 1] *)
[@@deriving show { with_path = false }]

type structure = structure_item list [@@deriving show { with_path = false }]
