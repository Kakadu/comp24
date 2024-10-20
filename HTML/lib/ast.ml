(** Copyright 2024-2025, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type const =
  | CInt of int (** int *)
  | CBool of bool (** bool *)
  | CNil (** [] *)
  | CUnit (*** () *)
[@@deriving eq, show { with_path = false }]

type ident = string [@@deriving eq, show { with_path = false }]

type rec_flag =
  | Recursive (** Recursive *)
  | Not_recursive (** Not recursive *)
[@@deriving eq, show { with_path = false }]

type ground =
  | GInt (** Int *)
  | GBool (** Bool *)
  | GUnit (** Unit — () *)
[@@deriving eq, show { with_path = false }]

type typ =
  | TVar of string (** 'a, 'b, ... *)
  | TArr of typ * typ (** 'a -> 'b *)
  | TTuple of typ list (** 'a * 'b *)
  | TList of typ (** 'a list *)
  | TGround of ground (** ground *)
[@@deriving eq, show { with_path = false }]

type pattern =
  | PId of ident (** x *)
  | PTuple of pattern_typed list (** (x, y) *)
  | PList of pattern_typed * pattern_typed (** x :: xs *)
  | PConst of const (** 3 *)
[@@deriving eq, show { with_path = false }]

and pattern_typed = pattern * typ option [@@deriving eq, show { with_path = false }]

type expr =
  | EConst of const (** Const. Examples: 100; true *)
  | EBinop of expr * ident * expr
  (** Binary operation. Examples: 2 + 2; (234 * 234) + 234 *)
  | EUnop of ident * expr (** Unary operation. Examples: -(1); (+b) *)
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
[@@deriving eq, show { with_path = false }]

and decl =
  | DLet of rec_flag * ident * expr * typ option (** Let declarations *)
  | DLetMut of rec_flag * (ident * expr * typ option) list
[@@deriving eq, show { with_path = false }]

type prog = decl list [@@deriving eq, show { with_path = false }]

let tint = TGround GInt
let tbool = TGround GBool
let tunit = TGround GUnit
let tarrow left_type right_type = TArr (left_type, right_type)
let ttuple type_list = TTuple type_list
let tlist typ = TList typ
let tvar n = TVar n
let pid id = PId id
let ptuple p_list = PTuple p_list
let plist hd tl = PList (hd, tl)
let p_typed ?(typ = None) (p : pattern) : pattern_typed = p, typ
let pconst c = PConst c
let econst c = EConst c
let eid i = EId i
let efun pat e = EFun (pat, e)
let eapp f args = EApp (f, args)
let eif e1 e2 e3 = EIf (e1, e2, e3)
let elist hd tl = EList (hd, tl)
let etuple l = ETuple l
let eclsr d e = EClsr (d, e)
let ematch e cl = EMatch (e, cl)
let dlet rf i e_let typ = DLet (rf, i, e_let, typ)
let dletmut rec_flag let_list = DLetMut (rec_flag, let_list)
let prog (d_l : decl list) : prog = d_l
