(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Runtime
module Env = Map.Make (String)

let func_in_runtime_env runtime name =
  let runtime_name = name_to_runtime_mapping name in
  match RuntimeEnv.find_opt runtime_name runtime with
  | Some value -> value
  | None -> failwith "The name was not found in the environment"
;;

let get_func_info runtime name =
  match RuntimeEnv.find_opt name runtime, List.assoc_opt name runtime_functions with
  | Some llval, Some lltyp -> llval, lltyp
  | _ -> failwith "Error: unbound name (runtime func)"
;;

let get_func_value runtime env name =
  match Env.find_opt name env with
  | Some value -> value
  | None -> func_in_runtime_env runtime name
;;
