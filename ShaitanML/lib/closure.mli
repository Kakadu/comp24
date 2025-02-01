val unbound_identifiers : Ast.expr -> (string, Base.String.comparator_witness) Base.Set.t

val close_function
  :  'a
  -> 'b
  -> 'c
  -> ('a -> 'b -> 'c -> Ast.expr -> Ast.expr)
  -> Ast.expr
  -> Ast.expr

val get_global_names : Ast.pattern -> (string, Base.String.comparator_witness) Base.Set.t

val convert_binding
  :  (string, Base.String.comparator_witness) Base.Set.t
  -> Ast.str_item
  -> Ast.str_item

val convert_ast : Ast.str_item list -> Ast.str_item list
