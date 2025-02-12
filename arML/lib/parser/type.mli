
open Ast.AbstractSyntaxTree

(* Type definition dispatch *)
type type_dispatch = {
  parse_ground_type : type_defenition Angstrom.t;
  parse_polymorphic_type : type_defenition Angstrom.t;
  parse_tuple_type : type_dispatch -> type_defenition Angstrom.t;
  parse_list_type : type_dispatch -> type_defenition Angstrom.t;
  parse_arrow_type : type_dispatch -> type_defenition Angstrom.t;
}

(* Main type definition parser *)
val parse_type : type_defenition Angstrom.t
