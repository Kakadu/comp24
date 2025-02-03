open Ast

type lexpr =
  | LEConst of const (** 123, true, "string" *)
  | LEVar of id (** x *)
  | LEIf of lexpr * lexpr * lexpr (** if e1 then e2 else e3 *)
  | EMatch of lexpr * lcase list (** match e with p1 -> e1 |...| pn -> en *)
  | ELet of rec_flag * binding list * lexpr (** let x = e1 in e2 *)
  | ETuple of lexpr list (** a, b, c *)
  | ECons of lexpr * lexpr (** x :: xs | [x1; x2]*)
  | EApply of lexpr * lexpr (** f e *)
[@@deriving show { with_path = false }]


and lcase = pattern * lexpr [@@deriving show { with_path = false }]

and lbinding = pattern * lexpr [@@deriving show { with_path = false }]

type lstr_item =
  | LSEval of lexpr (** Some expression *)
  | LSValue of rec_flag * binding list (** let [rec] p1 = e1 and p2 = e2 and ... *)
[@@deriving show { with_path = false }]