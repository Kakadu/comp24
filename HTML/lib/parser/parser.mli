(** Copyright 2024-2025, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

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
val keywords : string list
val is_keyword : string -> bool
val ( =?*> ) : string -> 'a Angstrom.t -> 'a Angstrom.t
val ( =?>>| ) : string -> 'a -> 'a Angstrom.t
val parse_int : AstLib.Ast.const Angstrom.t
val parse_bool : AstLib.Ast.const Angstrom.t
val parse_unit : AstLib.Ast.const Angstrom.t
val parse_const : AstLib.Ast.const Angstrom.t
val parse_const_expr : AstLib.Ast.expr Angstrom.t
val parse_op : string list -> string list -> string list -> string Angstrom.t
val parse_unary_op : string Angstrom.t
val parse_binary_op : string Angstrom.t
val parse_any_op : string Angstrom.t
val convert_op_to_ident : string -> AstLib.Ast.ident
val parse_op : AstLib.Ast.ident Angstrom.t
val parse_op : string Angstrom.t

type priority_group =
  { group : string list
  ; left_associative : bool
  }

val first_priority_group : priority_group
val second_priority_group : priority_group
val third_priority_group : priority_group
val fourth_priority_group : priority_group
val fifth_priority_group : priority_group
val parse_tuple : ?sep:string -> 'a Angstrom.t -> ('a list -> 'b) -> 'b Angstrom.t
val parse_tuple_expr : AstLib.Ast.expr Angstrom.t -> AstLib.Ast.expr Angstrom.t
val parse_branching : AstLib.Ast.expr Angstrom.t -> AstLib.Ast.expr Angstrom.t
val chainl1 : 'a Angstrom.t -> ('a -> 'a -> 'a) Angstrom.t -> 'a Angstrom.t
val chainr1 : 'a Angstrom.t -> ('a -> 'a -> 'a) Angstrom.t -> 'a Angstrom.t

val binop_binder
  :  string list
  -> (AstLib.Ast.expr -> AstLib.Ast.expr -> AstLib.Ast.expr) Angstrom.t

val get_chain : AstLib.Ast.expr Angstrom.t -> priority_group -> AstLib.Ast.expr Angstrom.t
val parse_un_op_app : AstLib.Ast.expr Angstrom.t -> AstLib.Ast.expr Angstrom.t
val parse_bin_op_app : AstLib.Ast.expr Angstrom.t -> AstLib.Ast.expr Angstrom.t
val parse_list_semicolon : 'a Angstrom.t -> ('a -> 'b -> 'b) -> 'b -> 'b Angstrom.t
val parse_list_expr : AstLib.Ast.expr Angstrom.t -> AstLib.Ast.expr Angstrom.t
val parse_letters : string Angstrom.t
val parse_identifier_letters : string Angstrom.t
val parse_identifier_definable : AstLib.Ast.ident_definable Angstrom.t
val parse_identifier_expr : AstLib.Ast.expr Angstrom.t
val parse_ground_type : AstLib.Ast.typ Angstrom.t
val parse_generic_type : AstLib.Ast.typ Angstrom.t
val parse_base_types : AstLib.Ast.typ Angstrom.t
val parse_tuple_type : AstLib.Ast.typ Angstrom.t -> AstLib.Ast.typ Angstrom.t
val parse_arrow_type : AstLib.Ast.typ Angstrom.t -> AstLib.Ast.typ Angstrom.t
val parse_list_type : AstLib.Ast.typ Angstrom.t -> AstLib.Ast.typ Angstrom.t
val parse_type : AstLib.Ast.typ Angstrom.t
val parse_get_explicit_typ : AstLib.Ast.typ option Angstrom.t
val parse_pat_typed : AstLib.Ast.pattern Angstrom.t -> AstLib.Ast.pattern_typed Angstrom.t
val parse_ident_pat : AstLib.Ast.pattern Angstrom.t

val parse_list_pat_semicolon
  :  AstLib.Ast.pattern Angstrom.t
  -> AstLib.Ast.pattern Angstrom.t

val parse_list_pat_colons : AstLib.Ast.pattern Angstrom.t -> AstLib.Ast.pattern Angstrom.t
val parse_tuple_pat : AstLib.Ast.pattern Angstrom.t -> AstLib.Ast.pattern Angstrom.t
val parse_base_pats : AstLib.Ast.pattern Angstrom.t
val parse_lrf_pats : AstLib.Ast.pattern Angstrom.t -> AstLib.Ast.pattern Angstrom.t
val parse_pattern_typed : AstLib.Ast.pattern_typed Angstrom.t
val parse_params : AstLib.Ast.pattern_typed list Angstrom.t

val parse_fun_decl
  :  AstLib.Ast.expr Angstrom.t
  -> (AstLib.Ast.expr * AstLib.Ast.typ option) Angstrom.t

val parse_fun_anon_expr : AstLib.Ast.expr Angstrom.t -> AstLib.Ast.expr Angstrom.t
val parse_match : AstLib.Ast.expr Angstrom.t -> AstLib.Ast.expr Angstrom.t
val parse_rec_flag : AstLib.Ast.rec_flag Angstrom.t

val parse_let_body
  :  AstLib.Ast.expr Angstrom.t
  -> (AstLib.Ast.ident_definable * AstLib.Ast.expr * AstLib.Ast.typ option) Angstrom.t

val parse_closure
  :  AstLib.Ast.decl Angstrom.t
  -> AstLib.Ast.expr Angstrom.t
  -> AstLib.Ast.expr Angstrom.t

val parse_let_decl : AstLib.Ast.expr Angstrom.t -> AstLib.Ast.decl Angstrom.t
val parse_expr : AstLib.Ast.expr Angstrom.t
val parse_decls : AstLib.Ast.prog Angstrom.t
val parse_program : string -> (AstLib.Ast.prog, string) result
