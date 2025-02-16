(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val cint : int -> Ast.const
val cbool : bool -> Ast.const
val cnil : Ast.const
val cunit : Ast.const

(*===================== core_type =====================*)

val ptint : Ast.core_type
val ptbool : Ast.core_type
val ptunit : Ast.core_type
val ptlist : Ast.core_type -> Ast.core_type
val ptvar : string -> Ast.core_type
val pttuple : Ast.core_type list -> Ast.core_type
val ptarrow : Ast.core_type -> Ast.core_type -> Ast.core_type

(*===================== pattern =====================*)

val pconst : Ast.const -> Ast.pattern
val pvar : string -> Ast.pattern
val pcons : Ast.pattern -> Ast.pattern -> Ast.pattern
val pany : Ast.pattern
val ptuple : Ast.pattern list -> Ast.pattern
val pconstraint : Ast.pattern -> Ast.core_type -> Ast.pattern

(*===================== expression =====================*)

val etype : Ast.expression -> Ast.core_type -> Ast.expression
val econst : Ast.const -> Ast.expression
val eval : string -> Ast.expression
val eapp : Ast.expression -> Ast.expression -> Ast.expression
val ematch : Ast.expression -> (Ast.pattern * Ast.expression) list -> Ast.expression
val efun : Ast.pattern -> Ast.expression -> Ast.expression
val ebinop : Ast.expression -> Ast.expression -> Ast.expression -> Ast.expression
val eunop : Ast.expression -> Ast.expression -> Ast.expression
val eite : Ast.expression -> Ast.expression -> Ast.expression -> Ast.expression
val elet : Ast.decl -> Ast.expression -> Ast.expression
val etuple : Ast.expression list -> Ast.expression
val econs : Ast.expression -> Ast.expression -> Ast.expression
val evalue_binding : Ast.pattern -> Ast.expression -> Ast.value_binding
val edecl : Ast.rec_flag -> Ast.value_binding list -> Ast.decl
val streval : Ast.expression -> Ast.structure_item
val strval : Ast.decl -> Ast.structure_item