(** Abstract syntax tree for KreML and helper functions*)

(** Represents an identifier of variable or function*)
type ident = Id of string
[@@deriving show]


type const =
    | Const_int of int (** [Const_int] represents integer constants like 42, 1337 *)
    | Const_bool of bool (** [Const_bool] represents boolean constants {true, false} *)
    | Const_string of string (** [Const_string] represents string literals like "42", "John Doe" *)
[@@deriving show]


type pattern =
    | Pat_const of const (** [Pat_const] corresponds to putting in constant values in pattern matching, e.g. {[match n with 0 -> ...]} *)
    | Pat_var of ident (** [Pat_var] coressponds to patterns which are variable identifiers, e.g. {[match n with  | ... | x -> ...]} *)
    | Pat_cons of pattern * pattern (** [Pat_cons] corresponds to [::] constructor of [list] data structure *)
    | Pat_tuple of pattern * pattern * pattern list (** [Pat_tuple] corresponds to n-tuples like (a, b, c) *)
    | Pat_wildcard (** [Pat_wildcard] represents a _ pattern*)
[@@deriving show]


val pconst : const -> pattern
val pvar : ident -> pattern
val pcons : pattern -> pattern -> pattern
val ptuple : pattern -> pattern -> pattern list -> pattern

type rec_flag = Recursive | NonRecursive
[@@deriving show]

type expr =
    | Expr_const of const (** Expresssion constants like [1], [true], ["asd"] *)
    | Expr_var of ident (** Variables like [x] *)
    | Expr_cons of (expr * expr) option (** Lists like [1; 2] are represented with [Some(1, Some(2, None))],
                                   constructions like [1::xs] are represented with [Some (1, xs)] *)
    | Expr_tuple of expr * expr * expr list
    | Expr_let of rec_flag * binding * expr (** {[let rec? x1 = expr1 in expr2]} *)
    | Expr_ite of expr * expr * expr (** {[if cond then expr1 else expr2]}*)
    | Expr_fun of pattern * expr (** [Expr_fun corresponds to anonymous function. {[Expr_fun(p, body)]} represents the {[fun p -> body]} *)
    | Expr_match of expr * case list (** [Expr_match] corresponds to match expressions like {[match x with | 0 -> 0 | 1 -> 1 | n -> n]} *)
    | Expr_app of expr * fun_args (** {[Expr_app(f, a, [b, c, d])]} corresponds to function application [f a b c d] *)
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

val eapp : expr -> fun_args -> expr

val econs : expr -> expr -> expr
val enil : expr
val etuple : expr -> expr -> expr list -> expr
val eite : expr -> expr -> expr -> expr
val efun : pattern -> expr -> expr
val elet : ?rec_flag: rec_flag -> (pattern * expr) -> expr -> expr
val ematch: expr -> binding list -> expr

val eland : expr -> expr -> expr
val elor : expr -> expr -> expr

val add : expr -> expr -> expr
val mul : expr -> expr -> expr
val div: expr -> expr -> expr
val sub : expr -> expr -> expr
val eqq : expr -> expr -> expr
val ge : expr -> expr -> expr
val le : expr -> expr -> expr
val geq : expr -> expr -> expr
val leq : expr -> expr -> expr
