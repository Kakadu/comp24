type id = string

val equal_id : id -> id -> bool
val pp_id : Format.formatter -> id -> unit
val show_id : id -> string

type rec_flag =
  | Rec
  | NonRec

val pp_rec_flag : Format.formatter -> rec_flag -> unit
val show_rec_flag : rec_flag -> string

type constant =
  | CInt of int
  | CBool of bool
  | CUnit
  | CNil

val pp_constant : Format.formatter -> constant -> unit
val show_constant : constant -> string

type type_ann =
  | TAInt
  | TABool
  | TAUnit
  | TATuple of type_ann list
  | TAFun of type_ann * type_ann
  | TAList of type_ann

val pp_type_ann : Format.formatter -> type_ann -> unit
val show_type_ann : type_ann -> string

type pattern =
  | PConst of constant
  | PWild
  | PIdent of id
  | PTuple of pattern list
  | PList of pattern list
  | PCons of pattern * pattern
  | PAnn of pattern * type_ann

val pp_pattern : Format.formatter -> pattern -> unit
val show_pattern : pattern -> string

type expr =
  | EConst of constant
  | EVar of id
  | EApp of expr * expr
  | EIfElse of expr * expr * expr
  | EFun of pattern list * expr
  | ELetIn of definition * expr
  | ETuple of expr list
  | EList of expr list
  | EMatch of expr * (pattern * expr) list

and definition = DLet of rec_flag * pattern * expr

val pp_expr : Format.formatter -> expr -> unit
val show_expr : expr -> string
val pp_definition : Format.formatter -> definition -> unit
val show_definition : definition -> string

type program = definition list

val pp_program : Format.formatter -> program -> unit
val show_program : program -> string
val p_const : constant -> pattern
val p_wild : pattern
val p_ident : id -> pattern
val p_tuple : pattern list -> pattern
val p_list : pattern list -> pattern
val p_cons : pattern -> pattern -> pattern
val p_ann : pattern -> type_ann -> pattern
val e_const : constant -> expr
val e_var : id -> expr
val e_app : expr -> expr -> expr
val e_if_else : expr -> expr -> expr -> expr
val e_fun : pattern list -> expr -> expr
val e_let_in : definition -> expr -> expr
val e_tuple : expr list -> expr
val e_list : expr list -> expr
val e_match : expr -> (pattern * expr) list -> expr
val d_let : pattern -> expr -> definition
val d_let_rec : pattern -> expr -> definition
val d_let_flag : rec_flag -> pattern -> expr -> definition
