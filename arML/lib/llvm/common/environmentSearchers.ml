(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Runtime

let get_func_info runtime name =
  match RuntimeEnv.find_opt name runtime, List.assoc_opt name runtime_functions with
  | Some llval, Some lltyp -> llval, lltyp
  | _ -> failwith "Error: unbound name (runtime func)"
;;
