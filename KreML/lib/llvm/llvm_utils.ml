(** Copyright 2024-2025, CursedML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llvm

let block_terminates_with_unreachable block =
  match Llvm.block_terminator block with
  | Some terminator ->
    (match Llvm.instr_opcode terminator with
     | Llvm.Opcode.Unreachable -> true
     | _ -> false)
  | None -> false
;;

let cast_firstclass_value expected value int_type ptr_type builder =
  if expected = ptr_type
  then build_inttoptr value int_type "" builder
  else build_ptrtoint value ptr_type "" builder
;;
