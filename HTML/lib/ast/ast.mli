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

type ident = string

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
  | TVar of string (** 'a, 'b, ... *)
  | TArr of typ * typ (** 'a -> 'b *)
  | TTuple of typ list (** 'a * 'b *)
  | TList of typ (** 'a list *)
  | TGround of ground (** ground *)

val equal_typ : typ -> typ -> bool
val pp_typ : Format.formatter -> typ -> unit
val show_typ : typ -> string

type pattern =
  | PId of ident (** x *)
  | PTuple of pattern_typed list (** (x, y) *)
  | PList of pattern_typed * pattern_typed (** x :: xs *)
  | PConst of const (** 3 *)

and pattern_typed = pattern * typ option

val equal_pattern : pattern -> pattern -> bool
val equal_pattern_typed : pattern_typed -> pattern_typed -> bool
val pp_pattern : Format.formatter -> pattern -> unit
val show_pattern : pattern -> string
val pp_pattern_typed : Format.formatter -> pattern_typed -> unit
val show_pattern_typed : pattern_typed -> string

type expr =
  | EConst of const (** Const. Examples: 100; true *)
  | EId of ident (** Identifier. Examples: a, b, c *)
  | EFun of pattern_typed * expr (** Function. Examples: fun x -> x + 1 *)
  | EApp of expr * expr (** Application. Examples: f (x - 1) *)
  | EIf of expr * expr * expr
  (** If-then-else. Examples: if x >= y then x - y else y - x *)
  | EList of expr * expr (** Lists. Examples: [1; 2; 3] *)
  | ETuple of expr list (** Tuple. Examples: (1, 2, 3) *)
  | EClsr of decl * expr (** Closure. Examples: let inc x = x + 1 in inc 5*)
  | EMatch of expr * (pattern_typed * expr) list
  (** Matching. Examples: match l with | hd::tl -> hd | _ -> [] *)

and decl =
  | DLet of rec_flag * ident * expr * typ option (** Let declarations *)
  | DLetMut of rec_flag * (ident * expr * typ option) list

val equal_expr : expr -> expr -> bool
val equal_decl : decl -> decl -> bool
val pp_expr : Format.formatter -> expr -> unit
val show_expr : expr -> string
val pp_decl : Format.formatter -> decl -> unit
val show_decl : decl -> string

type prog = decl list

val equal_prog : prog -> prog -> bool
val pp_prog : Format.formatter -> prog -> unit
val show_prog : prog -> string
val tint : typ
val tbool : typ
val tunit : typ
val tarrow : typ -> typ -> typ
val ttuple : typ list -> typ
val tlist : typ -> typ
val tvar : string -> typ
val pid : ident -> pattern
val ptuple : pattern_typed list -> pattern
val plist : pattern_typed -> pattern_typed -> pattern
val p_typed : ?typ:typ option -> pattern -> pattern_typed
val pconst : const -> pattern
val econst : const -> expr
val eid : ident -> expr
val efun : pattern_typed -> expr -> expr
val eapp : expr -> expr -> expr
val eif : expr -> expr -> expr -> expr
val elist : expr -> expr -> expr
val etuple : expr list -> expr
val eclsr : decl -> expr -> expr
val ematch : expr -> (pattern_typed * expr) list -> expr
val dlet : rec_flag -> ident -> expr -> typ option -> decl
val dletmut : rec_flag -> (ident * expr * typ option) list -> decl
val prog : decl list -> prog
