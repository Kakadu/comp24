(** Copyright 2024-2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Llvm

type predefined =
  { name : string
  ; t : string
  ; alt_name : string
  ; llvm_t : lltype -> lltype -> lltype
  }

(*llvm*)
let bin_op_type ret_t args_t = function_type ret_t [| args_t; args_t |]
let un_op_type ret_t args_t = function_type ret_t [| args_t |]

(** vars *)
let var_nothing = "__nothing"

(** default operators *)
let op_plus =
  { name = "( + )"; t = "int -> int -> int"; alt_name = "op_plus"; llvm_t = bin_op_type }
;;

let op_minus =
  { name = "( - )"; t = "int -> int -> int"; alt_name = "op_minus"; llvm_t = bin_op_type }
;;

let op_mult =
  { name = "( * )"; t = "int -> int -> int"; alt_name = "op_mult"; llvm_t = bin_op_type }
;;

let op_div =
  { name = "( / )"; t = "int -> int -> int"; alt_name = "op_div"; llvm_t = bin_op_type }
;;

let op_neg =
  { name = "( ~- )"; t = "int -> int"; alt_name = "op_neg"; llvm_t = un_op_type }
;;

let op_pos =
  { name = "( ~+ )"; t = "int -> int"; alt_name = "op_pos"; llvm_t = un_op_type }
;;

let op_not =
  { name = "not"; t = "bool -> bool"; alt_name = "op_not"; llvm_t = un_op_type }
;;

let op_gt =
  { name = "( > )"; t = "'a -> 'a -> bool"; alt_name = "op_gt"; llvm_t = bin_op_type }
;;

let op_ge =
  { name = "( >= )"; t = "'a -> 'a -> bool"; alt_name = "op_ge"; llvm_t = bin_op_type }
;;

let op_lt =
  { name = "( < )"; t = "'a -> 'a -> bool"; alt_name = "op_lt"; llvm_t = bin_op_type }
;;

let op_le =
  { name = "( <= )"; t = "'a -> 'a -> bool"; alt_name = "op_le"; llvm_t = bin_op_type }
;;

let op_eq =
  { name = "( = )"; t = "'a -> 'a -> bool"; alt_name = "op_eq"; llvm_t = bin_op_type }
;;

let op_neq =
  { name = "( <> )"; t = "'a -> 'a -> bool"; alt_name = "op_neq"; llvm_t = bin_op_type }
;;

let op_or =
  { name = "( || )"
  ; t = "bool -> bool -> bool"
  ; alt_name = "op_or"
  ; llvm_t = bin_op_type
  }
;;

let op_and =
  { name = "( && )"
  ; t = "bool -> bool -> bool"
  ; alt_name = "op_and"
  ; llvm_t = bin_op_type
  }
;;

let op_phys_eq =
  { name = "( == )"
  ; t = "'a -> 'a -> bool"
  ; alt_name = "op_phys_eq"
  ; llvm_t = bin_op_type
  }
;;

let print_int =
  { name = "print_int"; t = "int -> unit"; alt_name = "print_int"; llvm_t = un_op_type }
;;

let print_string =
  { name = "print_string"
  ; t = "string -> unit"
  ; alt_name = "print_string"
  ; llvm_t = un_op_type
  }
;;

(** special pattern remover *)
let disassemble_constructor =
  { name = "disassemble"; t = "'a -> 'b"; alt_name = "disassemble"; llvm_t = un_op_type }
;;

let get_from_tuple =
  { name = "get_from_tuple"
  ; t = "'a -> int -> 'c"
  ; alt_name = "get_from_tuple"
  ; llvm_t = bin_op_type
  }
;;

let same_cons =
  { name = "same_cons"
  ; t = "'a -> string -> bool"
  ; alt_name = "same_cons"
  ; llvm_t = bin_op_type
  }
;;

let exception_ =
  { name = "exception"; t = "string -> 'a"; alt_name = "exception"; llvm_t = un_op_type }
;;

(** List with all ops*)
let predefine_operators =
  [ op_plus
  ; op_minus
  ; op_mult
  ; op_div
  ; op_neg
  ; op_pos
  ; op_not
  ; op_gt
  ; op_ge
  ; op_lt
  ; op_le
  ; op_eq
  ; op_neq
  ; op_or
  ; op_and
  ; op_phys_eq
  ; print_int
  ; print_string
  ; disassemble_constructor
  ; get_from_tuple
  ; same_cons
  ; exception_
  ]
;;
