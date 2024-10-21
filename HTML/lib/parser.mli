val is_space : char -> bool
val parse_space : string Angstrom.t
val parse_space1 : string Angstrom.t
val parse_token : 'a Angstrom.t -> 'a Angstrom.t
val parse_token1 : 'a Angstrom.t -> 'a Angstrom.t
val parse_stoken : string -> string Angstrom.t
val parse_stoken1 : string -> string Angstrom.t
val parse_next_stoken : string -> string Angstrom.t
val parse_parens : 'a Angstrom.t -> 'a Angstrom.t
val parse_parens_or_non : 'a Angstrom.t -> 'a Angstrom.t
val is_digit : char -> bool
val is_lower : char -> bool
val is_upper : char -> bool
val is_letter : char -> bool
val is_ident_char : char -> bool
val is_sign : char -> bool
val keywords : string list
val is_keyword : string -> bool
val ( =?*> ) : string -> 'a Angstrom.t -> 'a Angstrom.t
val ( =?>>| ) : string -> 'a -> 'a Angstrom.t
val parse_int : Ast.const Angstrom.t
val parse_bool : Ast.const Angstrom.t
val parse_unit : Ast.const Angstrom.t
val parse_const : Ast.const Angstrom.t
val parse_const_expr : Ast.expr Angstrom.t
val prohibited_ops : string list
val first_unop_strings : string list
val suffix_unop_strings : string list
val base_unops : string list
val first_binop_strings : string list
val suffix_binop_strings : string list
val base_binops : string list
val parse_op : string list -> string list -> string list -> string Angstrom.t
val parse_unary_op : string Angstrom.t
val parse_binary_op : string Angstrom.t
val parse_op : Ast.ident_or_op Angstrom.t

type priority_group =
  { group : string list
  ; left_associative : bool
  }

val first_priority_group : priority_group
val second_priority_group : priority_group
val third_priority_group : priority_group
val fourth_priority_group : priority_group
val fifth_priority_group : priority_group
val priority_groups : priority_group list
val get_priority_group : string -> priority_group option
val parse_tuple : ?sep:string -> 'a Angstrom.t -> ('a list -> 'b) -> 'b Angstrom.t
val parse_tuple_expr : Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val parse_branching : Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val chainl1 : 'a Angstrom.t -> ('a -> 'a -> 'a) Angstrom.t -> 'a Angstrom.t
val chainr1 : 'a Angstrom.t -> ('a -> 'a -> 'a) Angstrom.t -> 'a Angstrom.t
val binop_binder : string list -> (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val get_chain : Ast.expr Angstrom.t -> priority_group -> Ast.expr Angstrom.t
val parse_un_op_app : Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val parse_bin_op_app : Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val parse_list_semicolon : 'a Angstrom.t -> ('a -> 'b -> 'b) -> 'b -> 'b Angstrom.t
val parse_list_expr : Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val parse_letters : string Angstrom.t
val parse_identifier : string Angstrom.t
val is_operator : string -> bool
val parse_identifier_expr : Ast.expr Angstrom.t
val parse_ground_type : Ast.typ Angstrom.t
val parse_generic_type : Ast.typ Angstrom.t
val parse_base_types : Ast.typ Angstrom.t
val parse_tuple_type : Ast.typ Angstrom.t -> Ast.typ Angstrom.t
val parse_arrow_type : Ast.typ Angstrom.t -> Ast.typ Angstrom.t
val parse_list_type : Ast.typ Angstrom.t -> Ast.typ Angstrom.t
val parse_type : Ast.typ Angstrom.t
val parse_get_explicit_typ : Ast.typ option Angstrom.t
val parse_pat_typed : Ast.pattern Angstrom.t -> Ast.pattern_typed Angstrom.t
val parse_ident_pat : Ast.pattern Angstrom.t
val parse_list_pat_semicolon : Ast.pattern Angstrom.t -> Ast.pattern Angstrom.t
val parse_list_pat_colons : Ast.pattern Angstrom.t -> Ast.pattern Angstrom.t
val parse_tuple_pat : Ast.pattern Angstrom.t -> Ast.pattern Angstrom.t
val parse_base_pats : Ast.pattern Angstrom.t
val parse_lrf_pats : Ast.pattern Angstrom.t -> Ast.pattern Angstrom.t
val parse_pattern_typed : Ast.pattern_typed Angstrom.t
val parse_params : Ast.pattern_typed list Angstrom.t
val parse_fun_decl : Ast.expr Angstrom.t -> (Ast.expr * Ast.typ option) Angstrom.t
val parse_fun_anon_expr : Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val parse_match : Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val parse_rec_flag : Ast.rec_flag Angstrom.t

val parse_let_body
  :  Ast.expr Angstrom.t
  -> (string * Ast.expr * Ast.typ option) Angstrom.t

val parse_closure : Ast.decl Angstrom.t -> Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val parse_let_decl : Ast.expr Angstrom.t -> Ast.decl Angstrom.t
val parse_expr : Ast.expr Angstrom.t
val parse_decls : Ast.prog Angstrom.t
val parse_program : string -> (Ast.prog, string) result
