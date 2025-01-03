type ident = string [@@deriving show]
type type_id = int [@@deriving show]

type typ =
  | Typ_int
  | Typ_bool
  | Typ_var of type_id
  | Typ_list of typ
  | Typ_tuple of typ * typ * typ list
  | Typ_fun of typ * typ
  | Typ_unit
[@@deriving show]

let tfun l r = Typ_fun (l, r)

type const =
  | Const_int of int
  | Const_bool of bool
[@@deriving show]

type pattern =
  | Pat_const of const
  | Pat_var of ident
  | Pat_cons of pattern * pattern
  | Pat_nil
  | Pat_tuple of pattern * pattern * pattern list
  | Pat_wildcard
  | Pat_constrained of pattern * typ
  | Pat_unit
[@@deriving show]

let pconst c = Pat_const c
let pvar v = Pat_var v
let pcons x xs = Pat_cons (x, xs)
let pnil = Pat_nil
let ptuple a b rest = Pat_tuple (a, b, rest)

type rec_flag =
  | Recursive
  | NonRecursive
[@@deriving show]

type expr =
  | Expr_const of const
  | Expr_var of ident
  | Expr_cons of expr * expr
  | Expr_nil
  | Expr_tuple of expr * expr * expr list
  | Expr_let of rec_flag * binding * expr
  | Expr_ite of expr * expr * expr
  | Expr_fun of pattern * expr
  | Expr_match of expr * case list
  | Expr_app of expr * expr
  | Expr_constrained of expr * typ
  | Expr_unit
[@@deriving show]

and binding = pattern * expr [@@deriving show]
and case = pattern * expr [@@deriving show]

type structure_item = Str_value of rec_flag * binding list [@@deriving show]
type structure = structure_item list [@@deriving show]

let eapp func args =
  Base.List.fold_left args ~init:func ~f:(fun acc arg -> Expr_app (acc, arg))
;;

let econs x y = Expr_cons (x, y)
let enil = Expr_nil
let etuple fst snd rest = Expr_tuple (fst, snd, rest)
let eite c e t = Expr_ite (c, e, t)
let efun p body = Expr_fun (p, body)

let elet ?(rec_flag = NonRecursive) (pattern, binding) where =
  Expr_let (rec_flag, (pattern, binding), where)
;;

let ematch expr bindings = Expr_match (expr, bindings)
let eland x y = eapp (Expr_var "&&") [ x; y ]
let elor x y = eapp (Expr_var "||") [ x; y ]
let add x y = eapp (Expr_var "+") [ x; y ]
let mul x y = eapp (Expr_var "*") [ x; y ]
let div x y = eapp (Expr_var "/") [ x; y ]
let sub x y = eapp (Expr_var "-") [ x; y ]
let eqq x y = eapp (Expr_var "=") [ x; y ]
let ge x y = eapp (Expr_var ">") [ x; y ]
let le x y = eapp (Expr_var "<") [ x; y ]
let geq x y = eapp (Expr_var ">=") [ x; y ]
let leq x y = eapp (Expr_var "<=") [ x; y ]
