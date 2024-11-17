open Ast
open Typ

val run_prog_inference : binding list -> ((id * typ) list, err) result