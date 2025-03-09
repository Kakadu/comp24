(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree
open UnpackFunctions
open Common.IdentifierStructs
open Common.IdentifierSearcher

let rec generate_path_to_name_in_pattern name = function
  | PVar v when v = name -> [ unpack_value ]
  | PTuple (p1, p2, ps) ->
    let paths =
      List.mapi
        (fun i p ->
          match generate_path_to_name_in_pattern name p with
          | [] -> None
          | path -> Some (i, path))
        (p1 :: p2 :: ps)
    in
    (match List.find_map Fun.id paths with
     | Some (i, path) -> unpack_tuple i :: path
     | None -> [])
  | PListConstructor (l, r) ->
    (match generate_path_to_name_in_pattern name l with
     | [] -> unpack_list_tail :: generate_path_to_name_in_pattern name r
     | path -> unpack_list_head :: path)
  | PTyped (p, _) -> generate_path_to_name_in_pattern name p
  | _ -> []
;;

let generate_declarations_from_case (p, expr) =
  let unpack_expr name (p, expr) =
    generate_path_to_name_in_pattern name p
    |> List.fold_left (fun acc unpack_fun -> unpack_fun acc) expr
  in
  get_pattern_identifiers p
  |> IdentifierSet.elements
  |> List.map (fun name -> name, unpack_expr name (p, expr))
;;
