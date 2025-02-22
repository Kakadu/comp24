(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Ast_test_utils : sig
  open Common.Errors
  open Common.Ast

  val ( let* )
    :  ('a, error) result
    -> ('a -> (structure_item list, error) result)
    -> (structure_item list, error) result

  val print_error : error -> unit
  val print_result : ('a -> unit) -> ('a, error) result -> unit
  val ( let*! ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
end
