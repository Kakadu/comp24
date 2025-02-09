val is_space : char -> bool
val pe_space : string Angstrom.t
val pe_space1 : string Angstrom.t
val pe_token : 'a Angstrom.t -> 'a Angstrom.t
val pe_token1 : 'a Angstrom.t -> 'a Angstrom.t
val pe_stoken : string -> string Angstrom.t
val pe_stoken1 : string -> string Angstrom.t
val pe_next_stoken : string -> string Angstrom.t
val pe_parens : 'a Angstrom.t -> 'a Angstrom.t
val pe_parens_or_non : 'a Angstrom.t -> 'a Angstrom.t
val is_digit : char -> bool
val is_lower : char -> bool
val is_upper : char -> bool
val is_letter : char -> bool
val is_ident_char : char -> bool
val keywords : string list
val is_keyword : string -> bool
val ( =?*> ) : string -> 'a Angstrom.t -> 'a Angstrom.t
val ( =?>>| ) : string -> 'a -> 'a Angstrom.t
val pe_int : AstLib.Ast.const Angstrom.t
val pe_bool : AstLib.Ast.const Angstrom.t
val pe_unit : AstLib.Ast.const Angstrom.t
val pe_const : AstLib.Ast.const Angstrom.t
val pe_const_expr : AstLib.Ast.expr Angstrom.t
val pe_op : string list -> string list -> string list -> string Angstrom.t
val pe_unary_op : string Angstrom.t
val pe_binary_op : string Angstrom.t
val pe_any_op : string Angstrom.t
val convert_op_to_ident : string -> AstLib.Ast.ident
val pe_op : string Angstrom.t

type priority_group =
  { group : string list
  ; left_associative : bool
  }

val first_priority_group : priority_group
val second_priority_group : priority_group
val third_priority_group : priority_group
val fourth_priority_group : priority_group
val fifth_priority_group : priority_group
val pe_assert_token_is_not_ahead : string -> bool Angstrom.t

val pe_tuple
  :  ?delim:string
  -> 'a Angstrom.t
  -> ('a -> 'a -> 'a list -> 'b)
  -> 'b Angstrom.t

val pe_value_or_tuple
  :  ?delim:string
  -> ('a Angstrom.t -> 'a Angstrom.t)
  -> 'a Angstrom.t
  -> 'a Angstrom.t

val pe_list_semicolon : 'a Angstrom.t -> ('a -> 'b -> 'b) -> 'b -> 'b Angstrom.t
val chainl1 : 'a Angstrom.t -> ('a -> 'a -> 'a) Angstrom.t -> 'a Angstrom.t
val chainr1 : 'a Angstrom.t -> ('a -> 'a -> 'a) Angstrom.t -> 'a Angstrom.t
val pe_letters : string Angstrom.t
val pe_identifier_letters : string Angstrom.t
val pe_identifier_definable : AstLib.Ast.ident_definable Angstrom.t
val pe_identifier_expr : AstLib.Ast.expr Angstrom.t
val pe_base_expr : AstLib.Ast.expr Angstrom.t
val pe_ground_type : AstLib.Ast.typ Angstrom.t
val pe_generic_type : AstLib.Ast.typ Angstrom.t
val pe_base_types : AstLib.Ast.typ Angstrom.t
val pe_tuple_type : AstLib.Ast.typ Angstrom.t -> AstLib.Ast.typ Angstrom.t
val pe_value_or_tuple_type : AstLib.Ast.typ Angstrom.t -> AstLib.Ast.typ Angstrom.t
val pe_arrow_type : AstLib.Ast.typ Angstrom.t -> AstLib.Ast.typ Angstrom.t
val pe_list_type : AstLib.Ast.typ Angstrom.t -> AstLib.Ast.typ Angstrom.t
val pe_type : AstLib.Ast.typ Angstrom.t
val pe_get_explicit_typ : AstLib.Ast.typ option Angstrom.t
val pe_typed : 'a Angstrom.t -> ('a * AstLib.Ast.typ option) Angstrom.t

val rollback_not_list
  :  (AstLib.Ast.pattern * 'a) Angstrom.t
  -> AstLib.Ast.pattern Angstrom.t

val pe_list_pat_semicolon : AstLib.Ast.pattern Angstrom.t -> AstLib.Ast.pattern Angstrom.t
val delim_list_pat_colons : string
val pe_list_pat_colons : AstLib.Ast.pattern Angstrom.t -> AstLib.Ast.pattern Angstrom.t
val pe_base_pat : AstLib.Ast.pattern Angstrom.t
val pe_lrf_pats : AstLib.Ast.pattern Angstrom.t -> AstLib.Ast.pattern Angstrom.t
val pe_tuple_pat : AstLib.Ast.pattern Angstrom.t -> AstLib.Ast.pattern Angstrom.t
val pe_value_or_tuple_pat : AstLib.Ast.pattern Angstrom.t -> AstLib.Ast.pattern Angstrom.t
val pe_pattern : AstLib.Ast.pattern Angstrom.t
val pe_pattern_typed : (AstLib.Ast.pattern * AstLib.Ast.typ option) Angstrom.t
val pe_tuple_expr : AstLib.Ast.expr Angstrom.t -> AstLib.Ast.expr Angstrom.t
val pe_value_or_tuple_expr : AstLib.Ast.expr Angstrom.t -> AstLib.Ast.expr Angstrom.t
val pe_branching : AstLib.Ast.expr Angstrom.t -> AstLib.Ast.expr Angstrom.t

val get_typed_ebinop_applier
  :  string
  -> AstLib.Ast.expr_typed
  -> AstLib.Ast.expr_typed
  -> AstLib.Ast.expr_typed

val binop_binder
  :  string list
  -> (AstLib.Ast.expr_typed -> AstLib.Ast.expr_typed -> AstLib.Ast.expr_typed) Angstrom.t

val get_chain
  :  AstLib.Ast.expr_typed Angstrom.t
  -> priority_group
  -> AstLib.Ast.expr_typed Angstrom.t

val pe_typed_if_in_parens : AstLib.Ast.expr Angstrom.t -> AstLib.Ast.expr_typed Angstrom.t
val rollback_not_app : ('a * 'b option) Angstrom.t -> 'a Angstrom.t
val pe_bin_op_app : AstLib.Ast.expr Angstrom.t -> AstLib.Ast.expr Angstrom.t
val pe_list_expr_semicolon : AstLib.Ast.expr Angstrom.t -> AstLib.Ast.expr Angstrom.t
val pe_params : (AstLib.Ast.pattern * AstLib.Ast.typ option) list Angstrom.t

val pe_fun_let_decl
  :  AstLib.Ast.expr Angstrom.t
  -> (AstLib.Ast.typ option * (AstLib.Ast.expr * AstLib.Ast.typ option)) Angstrom.t

val pe_fun_anon_expr : AstLib.Ast.expr Angstrom.t -> AstLib.Ast.expr Angstrom.t
val pe_match : AstLib.Ast.expr Angstrom.t -> AstLib.Ast.expr Angstrom.t
val pe_rec_flag : AstLib.Ast.rec_flag Angstrom.t

val pe_let_body
  :  AstLib.Ast.expr Angstrom.t
  -> ((AstLib.Ast.pattern_or_op * AstLib.Ast.typ option)
     * (AstLib.Ast.expr * AstLib.Ast.typ option))
       Angstrom.t

val pe_closure
  :  AstLib.Ast.decl Angstrom.t
  -> AstLib.Ast.expr Angstrom.t
  -> AstLib.Ast.expr Angstrom.t

val pe_let_decl : AstLib.Ast.expr Angstrom.t -> AstLib.Ast.decl Angstrom.t
val pe_expr : AstLib.Ast.expr Angstrom.t
val pe_decls : AstLib.Ast.prog Angstrom.t
val parse_program : string -> (AstLib.Ast.prog, string) result
