(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Updates the list of names by adding a new name if it is not already present.  
    Ensures that the list remains unique. *)
val update_name_list : string -> string list -> string list

(** Infers the type of a declaration within the given type environment.  
    Also updates with declaration name the list of processed names.  
    Returns an updated type environment and updated name list or a type error. *)
val infer_declaration : 
  TypeEnv.t -> 
  string list -> 
  Ast.declaration -> 
  (TypeEnv.t * string list, TypeErrors.error) Common.StateResultMonad.t
