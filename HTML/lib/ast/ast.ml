(** Copyright 2024-2025, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type const =
  | CInt of int (** int *)
  | CBool of bool (** bool *)
  | CNil (** [] *)
  | CUnit (** () *)
[@@deriving eq, show { with_path = false }]

type base_op =
  | Plus (** plus: + *)
  | Minus (** minus: - *)
[@@deriving eq, show { with_path = false }]

(** Letters of idents with only letters *)
type ident_letters = string [@@deriving eq, show { with_path = false }]

(** Letters of idents-operators *)
type ident_op = string [@@deriving eq, show { with_path = false }]

type ident_definable =
  | IdentLetters of ident_letters (** a, b, c, ...*)
  | IdentOp of ident_op (** <, >, =, any ops... ...*)
[@@deriving eq, show { with_path = false }]

type ident =
  | IdentOfDefinable of ident_definable (** definable idents *)
  | IdentOfBaseOp of base_op (** base ops *)
[@@deriving eq, show { with_path = false }]

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
  | TVar of ident_letters (** 'a, 'b, ... *)
  | TArr of typ * typ (** 'a -> 'b *)
  | TTuple of typ * typ * typ list (** 'a * 'b *)
  | TList of typ (** 'a list *)
  | TGround of ground (** ground *)
[@@deriving eq, show { with_path = false }]

type 'a typed = 'a * typ [@@deriving eq, show { with_path = false }]

type pattern =
  | PId of ident_letters (** x *)
  | PTuple of pattern * pattern * pattern list (** (x, y) *)
  | PList of pattern * pattern (** x :: xs *)
  | PConst of const (** 3 *)
  | PConstraint of pattern typed
[@@deriving eq, show { with_path = false }]

type pattern_or_op =
  | POpPat of pattern (** pattern *)
  | POpOp of ident_op (** custom operator *)
  | POrOpConstraint of pattern_or_op typed
[@@deriving eq, show { with_path = false }]

(** branch in match expr *)
type branch = pattern * expr [@@deriving eq, show { with_path = false }]

and expr =
  | EConst of const (** Const. Examples: 100; true *)
  | EId of ident (** Identifier. Examples: a, b, c *)
  | EFun of pattern * expr (** Function. Examples: fun x -> x + 1 *)
  | EApp of expr * expr (** Application. Examples: f (x - 1) *)
  | EIf of expr * expr * expr
  (** If-then-else. Examples: if x >= y then x - y else y - x *)
  | EList of expr * expr (** Lists. Examples: [1; 2; 3] *)
  | ETuple of expr * expr * expr list (** Tuple. Examples: (1, 2, 3) *)
  | EClsr of decl * expr (** Closure. Examples: let inc x = x + 1 in inc 5*)
  | EMatch of expr * branch * branch list
  | EConstraint of expr typed
  (** Matching. Examples: match l with | hd::tl -> hd | _ -> [] *)
[@@deriving eq, show { with_path = false }]

(** let body: pattern and associated expression *)
and let_body = pattern_or_op * expr [@@deriving eq, show { with_path = false }]

and decl =
  | DLet of rec_flag * let_body (** Let declaration *)
  | DLetMut of rec_flag * let_body * let_body * let_body list
  (** Mutual let declaration *)
[@@deriving eq, show { with_path = false }]

(** the whole program *)
type prog = decl list [@@deriving eq, show { with_path = false }]

let ident_letters (s : ident_letters) = IdentLetters s
let ident_op (s : ident_op) = IdentOp s
let ident_of_definable s = IdentOfDefinable s
let ident_of_base_op b = IdentOfBaseOp b
let tint = TGround GInt
let tbool = TGround GBool
let tunit = TGround GUnit
let tarrow left_type right_type = TArr (left_type, right_type)
let ttuple t1 t2 type_list = TTuple (t1, t2, type_list)
let tlist typ = TList typ
let tvar n = TVar n
let pid (id : ident_letters) = PId id
let ptuple p1 p2 p_list = PTuple (p1, p2, p_list)
let plist hd tl = PList (hd, tl)

let p_typed ?(typ = None) (p : pattern) : pattern =
  match typ with
  | Some typ -> PConstraint (p, typ)
  | None -> p
;;

let pop_pat p = POpPat p
let pop_op p = POpOp p

let pop_typed ?(typ = None) (pop : pattern_or_op) : pattern_or_op =
  match typ with
  | Some typ -> POrOpConstraint (pop, typ)
  | None -> pop
;;

let pconst c = PConst c
let econst c = EConst c
let eid i = EId i
let efun pat e = EFun (pat, e)
let eapp f args = EApp (f, args)
let eif e1 e2 e3 = EIf (e1, e2, e3)
let elist hd tl = EList (hd, tl)
let etuple e1 e2 l = ETuple (e1, e2, l)
let eclsr d e = EClsr (d, e)
let ematch e pair cl = EMatch (e, pair, cl)

let e_typed ?(typ = None) e : expr =
  match typ with
  | Some typ -> EConstraint (e, typ)
  | None -> e
;;

let dlet rf let_body = DLet (rf, let_body)
let dletmut rec_flag fst snd tl = DLetMut (rec_flag, fst, snd, tl)
let prog (d_l : decl list) : prog = d_l
