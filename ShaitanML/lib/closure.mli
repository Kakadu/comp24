val find_unbound_vars : Ast.expr -> (string, Base.String.comparator_witness) Base.Set.t

val close_function_scope
  :  'a
  -> 'b
  -> 'c
  -> ('a -> 'b -> 'c -> Ast.expr -> Ast.expr)
  -> Ast.expr
  -> Ast.expr

val find_global_vars : Ast.pattern -> (string, Base.String.comparator_witness) Base.Set.t
val ops_set : (string, Base.String.comparator_witness) Base.Set.t

val transform_ast
  :  (string, Base.String.comparator_witness) Base.Set.t
  -> Ast.str_item
  -> Ast.str_item

val convert_all_ast : Ast.str_item list -> Ast.str_item list
val run_tests : Ast.str_item list -> unit
