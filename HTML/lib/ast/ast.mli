(** Copyright 2024-2025, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type const =
  | CInt of int (** int *)
  | CBool of bool (** bool *)
  | CNil (** [] *)
  | CUnit (** () *)

val equal_const : const -> const -> bool
val pp_const : Format.formatter -> const -> unit
val show_const : const -> string

type base_op =
  | Plus (** plus: + *)
  | Minus (** minus: - *)

val equal_base_op : base_op -> base_op -> bool
val pp_base_op : Format.formatter -> base_op -> unit
val show_base_op : base_op -> string

(** Letters of idents with only letters *)
type ident_letters = string

val equal_ident_letters : ident_letters -> ident_letters -> bool
val pp_ident_letters : Format.formatter -> ident_letters -> unit
val show_ident_letters : ident_letters -> string

(** Letters of idents-operators *)
type ident_op = string

val equal_ident_op : ident_op -> ident_op -> bool
val pp_ident_op : Format.formatter -> ident_op -> unit
val show_ident_op : ident_op -> string

type ident_definable =
  | IdentLetters of ident_letters (** a, b, c, ...*)
  | IdentOp of ident_op (** <, >, =, any ops... ...*)

val equal_ident_definable : ident_definable -> ident_definable -> bool
val pp_ident_definable : Format.formatter -> ident_definable -> unit
val show_ident_definable : ident_definable -> string

type ident =
  | IdentOfDefinable of ident_definable (** definable idents *)
  | IdentOfBaseOp of base_op (** base ops *)

val equal_ident : ident -> ident -> bool
val pp_ident : Format.formatter -> ident -> unit
val show_ident : ident -> string

type rec_flag =
  | Recursive (** Recursive *)
  | Not_recursive (** Not recursive *)

val equal_rec_flag : rec_flag -> rec_flag -> bool
val pp_rec_flag : Format.formatter -> rec_flag -> unit
val show_rec_flag : rec_flag -> string

type ground =
  | GInt (** Int *)
  | GBool (** Bool *)
  | GUnit (** Unit — () *)

val equal_ground : ground -> ground -> bool
val pp_ground : Format.formatter -> ground -> unit
val show_ground : ground -> string

type typ =
  | TVar of ident_letters (** 'a, 'b, ... *)
  | TArr of typ * typ (** 'a -> 'b *)
  | TTuple of typ * typ * typ list (** 'a * 'b *)
  | TList of typ (** 'a list *)
  | TGround of ground (** ground *)

val equal_typ : typ -> typ -> bool
val pp_typ : Format.formatter -> typ -> unit
val show_typ : typ -> string
(** typed element *)
type 'a typed = 'a * typ option

val equal_typed : ('a -> 'a -> bool) -> 'a typed -> 'a typed -> bool
val pp_typed : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a typed -> unit
val show_typed : (Format.formatter -> 'a -> unit) -> 'a typed -> string

type pattern =
  | PId of ident_letters (** x *)
  | PTuple of pattern typed * pattern typed * pattern typed list (** (x, y) *)
  | PList of pattern typed * pattern typed (** x :: xs *)
  | PConst of const (** 3 *)

val equal_pattern : pattern -> pattern -> bool
val pp_pattern : Format.formatter -> pattern -> unit
val show_pattern : pattern -> string

type pattern_or_op =
  | POpPat of pattern (** pattern *)
  | POpOp of ident_op (** custom operator *)


val equal_pattern_or_op : pattern_or_op -> pattern_or_op -> bool
val pp_pattern_or_op : Format.formatter -> pattern_or_op -> unit
val show_pattern_or_op : pattern_or_op -> string

(** branch in match expr *)
type branch = pattern typed * expr typed

and expr =
  | EConst of const (** Const. Examples: 100; true *)
  | EId of ident (** Identifier. Examples: a, b, c *)
  | EFun of pattern typed * expr typed (** Function. Examples: fun x -> x + 1 *)
  | EApp of expr typed * expr typed (** Application. Examples: f (x - 1) *)
  | EIf of expr typed * expr typed * expr typed
  (** If-then-else. Examples: if x >= y then x - y else y - x *)
  | EList of expr typed * expr typed (** Lists. Examples: [1; 2; 3] *)
  | ETuple of expr typed * expr typed * expr typed list (** Tuple. Examples: (1, 2, 3) *)
  | EClsr of decl * expr typed (** Closure. Examples: let inc x = x + 1 in inc 5*)
  | EMatch of expr typed * branch * branch list
  (** Matching. Examples: match l with | hd::tl -> hd | _ -> [] *)

(** let body: pattern and associated expression *)
and let_body = pattern_or_op typed * expr typed

and decl =
  | DLet of rec_flag * let_body (** Let declaration *)
  | DLetMut of rec_flag * let_body * let_body * let_body list
  (** Mutual let declaration *)

val equal_branch : branch -> branch -> bool
val equal_expr : expr -> expr -> bool
val equal_let_body : let_body -> let_body -> bool
val equal_decl : decl -> decl -> bool
val pp_branch : Format.formatter -> branch -> unit
val show_branch : branch -> string
val pp_expr : Format.formatter -> expr -> unit
val show_expr : expr -> string
val pp_let_body : Format.formatter -> let_body -> unit
val show_let_body : let_body -> string
val pp_decl : Format.formatter -> decl -> unit
val show_decl : decl -> string

(** the whole program *)
type prog = decl list

val equal_prog : prog -> prog -> bool
val pp_prog : Format.formatter -> prog -> unit
val show_prog : prog -> string
val ident_letters : ident_letters -> ident_definable
val ident_op : ident_op -> ident_definable
val ident_of_definable : ident_definable -> ident
val ident_of_base_op : base_op -> ident
val tint : typ
val tbool : typ
val tunit : typ
val tarrow : typ -> typ -> typ
val ttuple : typ -> typ -> typ list -> typ
val tlist : typ -> typ
val tvar : ident_letters -> typ
val pid : ident_letters -> pattern
val ptuple : pattern typed -> pattern typed -> pattern typed list -> pattern
val plist : pattern typed -> pattern typed -> pattern
val p_typed : ?typ:typ option -> pattern -> pattern typed
val pop_pat : pattern -> pattern_or_op
val pop_op : ident_op -> pattern_or_op
val pop_typed : ?typ:typ option -> pattern_or_op -> pattern_or_op typed
val pconst : const -> pattern
val econst : const -> expr
val eid : ident -> expr
val efun : pattern typed -> expr typed -> expr
val eapp : expr typed -> expr typed -> expr
val eif : expr typed -> expr typed -> expr typed -> expr
val elist : expr typed -> expr typed -> expr
val etuple : expr typed -> expr typed -> expr typed list -> expr
val eclsr : decl -> expr typed -> expr
val ematch : expr typed -> branch -> branch list -> expr
val e_typed : ?typ:typ option -> expr -> expr typed
val dlet : rec_flag -> let_body -> decl
val dletmut : rec_flag -> let_body -> let_body -> let_body list -> decl
val prog : decl list -> prog
