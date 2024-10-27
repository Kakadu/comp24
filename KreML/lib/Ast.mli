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
    | Tconstr_any (** [_] *)
    | Tconstr_int (** Integer type constraint *)
    | Tconstr_bool (** Boolean type constraint *)
    | Tconstr_string (** String type constraint *)
    | Tconstr_arrow of typ_constr * typ_constr (** Function type constraint *)

type pattern =
    | Pat_const of const (** [Pat_const] corresponds to putting in constant values in pattern matching, e.g. {[match n with 0 -> ...]} *)
    | Pat_var of ident (** [Pat_var] coressponds to patterns which are variable identifiers, e.g. {[match n with  | ... | x -> ...]} *)
    | Pat_cons of pattern * pattern (** [Pat_cons] corresponds to [::] constructor of [list] data structure *)
    | Pat_tuple of pattern list (** [Pat_tuple] corresponds to n-tuples like (a, b, c) *)
    | Pat_constraint of typ_constr (** Pat_constraint corresponds to constraints {[fun (x : int)]}*)
    | Pat_any (** [Wildcard] represents a _ pattern*)

type rec_flag = Recursive | NonRecursive


type expr =
    | Expr_const of const (** Expresssion constants like [1], [true], ["asd"] *)
    | Expr_var of ident (** Variables like [x] *)
    | Expr_tuple of expr * expr * expr list
    | Expr_let of rec_flag * binding list * expr (** {[let rec? x1 = ident = expr1 and ... in expr2]} *)
    | Expr_ite of expr * expr * expr (** {[if cond then expr1 else expr2]}*)
    | Expr_fun of expr   list * expr(** [Expr_fun] corresponds to anonymous function. {[EFun([p_1, ..., p_n)], body} represents the [fun p_1 ... p_n -> e] expression*)
    | Expr_match of expr * case list (** [Expr_match] corresponds to match expressions like {[match x with | 0 -> 0 | 1 -> 1 | n -> n]} *)
    | Expr_app of expr * expr list (** {[Expr_app(f, [a, b, c])]} corresponds to function application [f a b c] *)

and binding = pattern * expr

and case = pattern * expr

val eapp : expr -> expr list -> expr

val eite : expr -> expr -> expr -> expr
val elet : ?rec_flag: rec_flag -> pattern -> expr -> expr -> expr

val add : expr -> expr -> expr
val mul : expr -> expr -> expr
val sub : expr -> expr -> expr
val eqq : expr -> expr -> expr
val ge : expr -> expr -> expr
val le : expr -> expr -> expr
val geq : expr -> expr -> expr
val leq : expr -> expr -> expr