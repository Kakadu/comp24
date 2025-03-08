(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Pe_ast
open Anf_ast
open Common
open Common.MonadCounter

let const_to_immexpr = function
  | Pe_Cint n -> ImmInt n
  | Pe_CBool v -> ImmBool v
;;

let rec to_immexpr = function
  | Pe_EConst c -> return ([], const_to_immexpr c)
  | Pe_EIdentifier name -> return ([], imm_id name)
  | e ->
    let* fresh = fresh >>| get_id in
    let* binds1, e = to_cexp e in
    return (binds1 @ [ fresh, e ], imm_id fresh)

and to_cexp = function
  | Pe_EIdentifier name -> return ([], cimmexpr @@ imm_id name)
  | Pe_EConst c -> return ([], cimmexpr @@ const_to_immexpr c)
;;
