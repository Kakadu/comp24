module COUNTERMONAD : sig
  val return : 'a -> 'b -> 'b * 'a
  val ( >>= ) : ('a -> 'b * 'c) -> ('c -> 'b -> 'd) -> 'a -> 'd
  val ( let* ) : ('a -> 'b * 'c) -> ('c -> 'b -> 'd) -> 'a -> 'd
  val get : 'a -> 'a * 'a
  val put : 'a -> 'b -> 'a * unit
  val run : 'a -> 'a
end

type bindings = Args_body of Ast.pattern list * Ast.expr

type ll_expr =
  | LLConstant of Ast.const
  | LLIdentifier of string
  | LLIfThenElse of ll_expr * ll_expr * ll_expr
  | LLApplication of ll_expr * ll_expr
  | LLConstraint of ll_expr * Ast.type_annot
  | LLTuple of ll_expr list
  | LLMatch of ll_expr * (Ast.pattern * ll_expr) list
  | LLLetIn of Ast.rec_flag * Ast.pattern * ll_expr * ll_expr

type ll_declaration =
  | LLDSingleLet of Ast.rec_flag * ll_let
  | LLDMutualRecDecl of Ast.rec_flag * ll_let list

and ll_let = LLLet of Ast.pattern * Ast.pattern list * ll_expr

val get_new_num : int -> int * int

val collect_bindings_from_pat
  :  Ast.pattern
  -> (string, Base.String.comparator_witness) Base.Set.t

val new_name : (string, 'a) Base.Set.t -> int -> int * string
val collect_function_arguments : Ast.expr -> bindings

val init_env
  :  (string, Base.String.comparator_witness) Base.Set.t
  -> Ast.str_item list
  -> (string, Base.String.comparator_witness) Base.Set.t

val prog_lift : Ast.str_item list -> ll_declaration list * int
val lift_ast : Ast.str_item list -> ll_declaration list
