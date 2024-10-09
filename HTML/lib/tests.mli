(** Copyright 2024-2025, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module ParsingTests : sig
  val parse_test : string -> HTML_lib.Ast.decl list -> bool
end

module InferenceTests : sig
  val infer_test : string -> unit
end
