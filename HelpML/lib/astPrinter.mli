open Ast
open Format

(** Pretty-prints a constant to a formatter *)
val pp_const : formatter -> const -> unit

(** Pretty-prints a binary operator to a formatter *)
val pp_bin_op : formatter -> bin_op -> unit

(** Pretty-prints a pattern to a formatter *)
val pp_pat : formatter -> pat -> unit

(** Pretty-prints a list of patterns to a formatter *)
val pp_patterns : formatter -> pat list -> unit

(** Pretty-prints a recursive flag to a formatter *)
val pp_rec_flag : formatter -> bool -> unit

(** Pretty-prints an expression to a formatter *)
val pp_expr : formatter -> expr -> unit

(** Pretty-prints a binding to a formatter *)
val pp_binding : formatter -> binding -> unit