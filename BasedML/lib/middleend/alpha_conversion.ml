(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast

type context =
  { name_mapping : (string, string * int, Base.String.comparator_witness) Base.Map.t
  ; reserved_names : (string, Base.String.comparator_witness) Base.Set.t
  }

let rec generate_unique_name id ctx counter =
  let new_name = Printf.sprintf "%s_arg_%d" id counter in
  if Base.Set.mem
       (Base.Set.union ctx.reserved_names (Base.Set.singleton (module Base.String) id))
       new_name
  then generate_unique_name id ctx (counter + 1)
  else new_name
;;

let rec alpha_convert_expr ctx = function
  | EConstant const -> EConstant const
  | _ -> failwith "unimplemented yet"
;;
