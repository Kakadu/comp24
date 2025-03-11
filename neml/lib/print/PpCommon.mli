[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open LMisc
open LAst
open PPrint

val parens : document -> document
val infixl : document -> document -> document -> document
val infixr : document -> document -> document -> document

val pp_id : Id.t -> document
val pp_const : Const.t -> document

val plet :
     (Pat.t -> document)
  -> (Expr.t -> document)
  -> Expr.rec_flag
  -> Expr.value_binding List1.t
  -> Expr.t option
  -> document

module PrecedencePrinter : functor
  (Prec : sig
     type t

     val min : int
     val to_enum : t -> int
     val parens : document -> document
   end)
  -> sig
  type 'a t

  val run : document t -> document
  val runf : ('a -> document t) -> 'a -> document

  val return : Prec.t -> document -> document t

  val rprefix : Prec.t -> (document -> document) -> document t -> document t

  val rinfixl :
       Prec.t
    -> (document -> document -> document)
    -> document t
    -> document t
    -> document t

  val rinfixr :
       Prec.t
    -> (document -> document -> document)
    -> document t
    -> document t
    -> document t

  val rinfixn :
    Prec.t -> (document list -> document) -> document t list -> document t
end
