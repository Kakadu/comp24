[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open PPrint

open LAst
open PpCommon

let pp_stritem : StrItem.t -> document =
  let open StrItem in
  function
  | Eval expr ->
      PpExpr.pp expr
  | Let (rec_flag, bindings) ->
      plet PpPat.pp PpExpr.pp rec_flag bindings None
  | Type {id; params; variants} ->
      let id = pp_id id in

      let params =
        match Set.to_list params with
        | [] ->
            empty
        | [var] ->
            PpTy.pp_var var ^^ space
        | vars ->
            let params = List.map vars ~f:PpTy.pp_var in
            parens (flow (comma ^^ break 1) params) ^^ space
      in

      let variants =
        let variants =
          List.map variants ~f:(fun {id; arg} ->
              group @@ pp_id id
              ^^ optional
                   (fun ty -> string " of" ^^ nest 2 (break 1 ^^ PpTy.pp ty))
                   arg )
        in
        let doc =
          string " ="
          ^^ group
               ( break 1
               ^^ ifflat empty (string "| ")
               ^^ separate (break 1 ^^ string "| ") variants )
        in
        if List.is_empty variants then empty else doc
      in

      group @@ string "type" ^^ group (break 1) ^^ params ^^ id ^^ variants

let pp_structure (str : structure) : document =
  let open PPrint in
  let str = List.map str ~f:pp_stritem in
  flow (string ";;" ^^ hardline) str
