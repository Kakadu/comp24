module COUNTERMONAD : sig
  val return : 'a -> 'b -> 'b * 'a
  val ( >>= ) : ('a -> 'b * 'c) -> ('c -> 'b -> 'd) -> 'a -> 'd
  val ( let* ) : ('a -> 'b * 'c) -> ('c -> 'b -> 'd) -> 'a -> 'd
  val get : 'a -> 'a * 'a
  val put : 'a -> 'b -> 'a * unit
  val run : 'a -> 'a
end

type bindings = Args_body of Ast.pattern list * Ast.expr

val get_new_num : int -> int * int

val collect_bindings_from_pat
  :  Ast.pattern
  -> (string, Base.String.comparator_witness) Base.Set.t

val new_name : (string, 'a) Base.Set.t -> int -> int * string
val collect_function_arguments : Ast.pattern list -> Ast.expr -> bindings

val init_env
  :  (string, Base.String.comparator_witness) Base.Set.t
  -> Ast.str_item list
  -> (string, Base.String.comparator_witness) Base.Set.t

val prog_lift : Ast.str_item list -> L_lifting_ast.ll_declaration list * int
val lift_ast : Ast.str_item list -> L_lifting_ast.ll_declaration list
