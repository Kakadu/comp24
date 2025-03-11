(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Parser runner *)

(** [parse_expression input] parses an expression from the input string. *)
val parse_expression : string -> unit

(** [parse_program input] parses a declarations from the input string. *)
val parse_program : string -> unit

(* -------------- *)

(* Inferencer runner *)

(** [inference_expression input] infers types for the expression from the input string. *)
val inference_expression : string -> unit

(** [inference_program input] infers types for the declarations from the input string. *)
val inference_program : string -> unit

(* -------------- *)

(* Closure conversion *)

(** [closure_conversion_expression input] performs closure conversion on the expression. *)
val closure_conversion_expression : string -> unit

(** [closure_conversion_program input] performs closure conversion on the declaration list. *)
val closure_conversion_program : string -> unit

(* -------------- *)

(* Lambda lifting *)

(** [lambda_lifting_program input] transforms a program by lifting all nested functions
    to the top level. *)
val lambda_lifting_program : string -> unit

(* -------------- *)
(* Pattern-matching elimination *)

val eliminate_pm_program : string -> unit

(* -------------- *)

(* Alpha conversion *)

val alpha_conversion_program : string -> unit

(* -------------- *)

(* ANF conversion *)

val convert_to_anf_and_back_types : string -> unit
val anf_conversion_program : string -> unit

(* -------------- *)
