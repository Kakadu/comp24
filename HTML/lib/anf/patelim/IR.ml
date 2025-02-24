open AstLib.Ast
open Common.Base_bin_ops

type expr_no_pm =
  | NoPMEConst of const (** Const. Examples: 100; true *)
  | NoPMEId of ident (** Identifier. Examples: a, b, c *)
  | NoPMEFun of pattern * expr_no_pm (** Function. Examples: fun x -> x + 1 *)
  | NoPMEApp of expr_no_pm * expr_no_pm (** Application. Examples: f (x - 1) *)
  | NoPMEIf of expr_no_pm * expr_no_pm * expr_no_pm
  (** If-then-else. Examples: if x >= y then x - y else y - x *)
  | NoPMEList of expr_no_pm * expr_no_pm (** Lists. Examples: [1; 2; 3] *)
  | NoPMETuple of expr_no_pm * expr_no_pm * expr_no_pm list
  (** Tuple. Examples: (1, 2, 3) *)
  | NoPMEClsr of decl_no_pm * expr_no_pm
  (** Closure. Examples: let inc x = x + 1 in inc 5*)
  | NoPMEBinOp of bin_op * expr_no_pm * expr_no_pm
  (** Since operators can be redefined, these binary operations have standard semantics. *)
  | NoPMEConstraint of expr_no_pm typed
[@@deriving eq, show { with_path = false }]

and let_body_no_pm = pattern_or_op * expr_no_pm
[@@deriving eq, show { with_path = false }]

and decl_no_pm =
  | NoPMDLet of rec_flag * let_body_no_pm (** Let declaration *)
  | NoPMDLetMut of rec_flag * let_body_no_pm * let_body_no_pm * let_body_no_pm list
  (** Mutual let declaration *)
[@@deriving eq, show { with_path = false }]

let econst c = NoPMEConst c
let eid i = NoPMEId i
let efun pat e = NoPMEFun (pat, e)
let eapp f args = NoPMEApp (f, args)
let eif e1 e2 e3 = NoPMEIf (e1, e2, e3)
let elist hd tl = NoPMEList (hd, tl)
let etuple e1 e2 l = NoPMETuple (e1, e2, l)
let eclsr d e = NoPMEClsr (d, e)
let ebin_op bop e1 e2 = NoPMEBinOp (bop, e1, e2)

let e_typed ?(typ = None) e : expr_no_pm =
  match typ with
  | Some typ -> NoPMEConstraint (e, typ)
  | None -> e
;;

let dlet rf let_body_no_pm = NoPMDLet (rf, let_body_no_pm)
let dletmut rec_flag fst snd tl = NoPMDLetMut (rec_flag, fst, snd, tl)
