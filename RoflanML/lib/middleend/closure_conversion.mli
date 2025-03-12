open Base
open Ast

val close : decl -> (string, String.comparator_witness) Set.t -> decl
val close_program : decl list -> decl list
