val pp_const : Format.formatter -> Ast.const -> unit
val pp_ground_typ : Format.formatter -> Ast.ground -> unit
val pp_ident_definable : Format.formatter -> Ast.ident_definable -> unit
val pp_ident : Format.formatter -> Ast.ident -> unit
val pp_typ : Format.formatter -> Ast.typ -> unit

val pp_typed
  :  Format.formatter
  -> (Format.formatter -> 'a -> unit)
  -> 'a * Ast.typ
  -> unit

val pp_pattern : Format.formatter -> Ast.pattern -> unit
val pp_pattern_or_op : Format.formatter -> Ast.pattern_or_op -> unit
val pp_rec_flag : Format.formatter -> Ast.rec_flag -> unit
val pp_expr : Format.formatter -> Ast.expr -> unit
val pp_let_expr : Format.formatter -> Ast.expr -> unit
val pp_decl : Format.formatter -> Ast.decl -> unit
val pp_prog : Format.formatter -> Ast.decl list -> unit
