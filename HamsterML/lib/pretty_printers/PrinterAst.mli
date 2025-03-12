(* Prog pretty printer *)
val pretty_print_prog : Ast.expr list -> string

(* Expr pretty printer *)
val pretty_print_expr : Ast.expr -> string

(* Pattern pretty printer *)
val pretty_print_pattern : Ast.pattern -> string

(* Operation pretty printer *)
val pretty_print_op : Ast.op -> string
