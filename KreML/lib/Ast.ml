(** Abstract syntax tree for KreML and helper functions*)


(** Represents an identifier of variable or function*)
type ident = string

type const =
    | Const_int of int (** [Const_int] represents integer constants like 42, 1337 *)
    | Const_bool of bool (** [Const_bool] represents boolean constants {true, false} *)
    | Const_string of string (** [Const_string] represents string literals like "42", "John Doe" *)

type binary_op = 
    | Add | Sub | Mul | Div | Rem
    | Eq | Ge | Geq | Le | Leq
    | Cons

type unary_op = Minus | Negate

type op

type typ_constr =
    | Tconstr_any
    | Tconstr_int 
    | Tconstr_bool
    | Tconstr_string
    | Tconstr_arrow of typ_constr * typ_constr

type pattern =
    | Pat_const of const
    | Pat_var of ident
    | Pat_cons of pattern * pattern
    | Pat_tuple of pattern list
    | Pat_constraint of typ_constr
    | Pat_any

type rec_flag = Recursive | NonRecursive


type expr =
    | Expr_const of const
    | Expr_var of ident
    | Expr_tuple of expr * expr * expr list
    | Expr_let of rec_flag * binding list * expr
    | Expr_ite of expr * expr * expr
    | Expr_fun of expr list * expr
    | Expr_match of expr * case list
    | Expr_app of expr * expr list

and binding = pattern * expr

and case = pattern * expr

let eapp f args = Expr_app(f, args)

let eite c e t = Expr_ite(c, e, t)
let elet ?(rec_flag = NonRecursive) pattern binding where =
     Expr_let(rec_flag, [pattern, binding], where)
let add x y = eapp (Expr_var "+") [x; y]
let mul x y = eapp (Expr_var "*") [x; y]
let sub x y = eapp (Expr_var "-") [x; y]
let eqq x y = eapp (Expr_var "=") [x; y]
let ge x y = eapp (Expr_var ">") [x; y]
let le x y = eapp (Expr_var "<") [x; y]
let geq x y = eapp (Expr_var ">=") [x; y]
let leq x y = eapp (Expr_var "<=") [x; y]
