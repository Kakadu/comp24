type ident = Id of string
[@@deriving show]

type const =
    | Const_int of int
    | Const_bool of bool
    | Const_string of string
[@@deriving show]


type pattern =
    | Pat_const of const
    | Pat_var of ident
    | Pat_cons of pattern * pattern
    | Pat_tuple of pattern * pattern * pattern list
    | Pat_wildcard
[@@deriving show]

let pconst c = Pat_const c
let pvar v = Pat_var v
let pcons x xs = Pat_cons(x, xs)
let ptuple a b rest = Pat_tuple(a, b, rest)

type rec_flag = Recursive | NonRecursive
[@@deriving show]


type expr =
    | Expr_const of const
    | Expr_var of ident
    | Expr_cons of (expr * expr) option
    | Expr_tuple of expr * expr * expr list
    | Expr_let of rec_flag * binding * expr
    | Expr_ite of expr * expr * expr
    | Expr_fun of pattern * expr
    | Expr_match of expr * case list
    | Expr_app of expr * fun_args
[@@deriving show]


and fun_args = expr * expr list
[@@deriving show]

and binding = pattern * expr
[@@deriving show]

and case = pattern * expr
[@@deriving show]

type structure_item =
| Str_value of rec_flag * binding list



type structure = structure_item list

let eapp f args = Expr_app(f, args)

let econs x y = Expr_cons(Some(x, y))
let enil = Expr_cons None
let etuple fst snd rest = Expr_tuple(fst, snd, rest)
let eite c e t = Expr_ite(c, e, t)
let efun p body = Expr_fun(p, body)
let elet ?(rec_flag = NonRecursive) (pattern, binding) where =
     Expr_let(rec_flag, (pattern, binding), where)
let ematch expr bindings = Expr_match(expr, bindings)
let eland x y = eapp (Expr_var (Id "&&")) (x, [y])
let elor x y = eapp (Expr_var (Id "||")) (x, [y])
let add x y = eapp (Expr_var (Id "+")) (x, [y])
let mul x y = eapp (Expr_var (Id "*")) (x, [y])
let div x y = eapp (Expr_var (Id "/")) (x, [y])
let sub x y = eapp (Expr_var (Id "-")) (x, [y])
let eqq x y = eapp (Expr_var (Id "=")) (x, [y])
let ge x y = eapp (Expr_var (Id ">")) (x, [y])
let le x y = eapp (Expr_var (Id "<")) (x, [y])
let geq x y = eapp (Expr_var (Id ">=")) (x, [y])
let leq x y = eapp (Expr_var (Id "<=")) (x, [y])
