(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

module type MONADERROR = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val error : string -> 'a t
end

module Result : sig
  type 'a t = ('a, string) result

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val error : string -> 'a t
end

module Mapper : functor (M : MONADERROR) -> sig
  val map1 : ('a -> 'b M.t) -> 'a list -> 'b list M.t
  val sexpr_of_declaration : Ast.let_declaration -> Sexplib0.Sexp.t
  val sexpr_of_declarations : Ast.let_declaration list -> Sexplib0.Sexp.t list
  val let_declaration_of_sexpr : Sexplib0.Sexp.t -> Ast.let_declaration M.t
  val declarations_of_sexpr : Sexplib0.Sexp.t list -> Ast.let_declaration list M.t
end

val show_sexp : Sexplib0.Sexp.t -> string
val sexpr_of_ast_test_parse : string -> unit
