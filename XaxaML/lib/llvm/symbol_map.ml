(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open! Llvm
open! Common_llvm

(* Единая таблица символов для всех значений *)
let symbols : (string, llvalue) Hashtbl.t = Hashtbl.create 10

(* Отдельная таблица для хранения типов функций *)
let function_types : (string, lltype) Hashtbl.t = Hashtbl.create 10

let lookup_symbol name =
  match Hashtbl.find_opt symbols (map_stdlib_to_runtime name) with
  | Some v -> v
  | None -> failwith (Printf.sprintf "Symbol not found: %s" name)
;;

let lookup_function_type name =
  match Hashtbl.find_opt function_types (map_stdlib_to_runtime name) with
  | Some t -> t
  | None -> failwith (Printf.sprintf "Function type not found: %s" name)
;;

let add_symbol name value = Hashtbl.add symbols (map_stdlib_to_runtime name) value

let add_function_type name ftype =
  Hashtbl.add function_types (map_stdlib_to_runtime name) ftype
;;
