[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open PPrint

open LMisc
open LAst
open PpCommon

module Prec = struct
  type t = Or | Tuple | List | Construct | Highest [@@deriving enum]
  let parens = parens
end

let pp : Pat.t -> document =
  let open Pat in
  let open PrecedencePrinter (Prec) in
  let rec p = function
    | Any ->
        return Prec.Highest (char '_')
    | Var id ->
        return Prec.Highest (pp_id id)
    | Const x ->
        return Prec.Highest (pp_const x)
    | Construct (I "::", Some (Tuple (l, r, []))) ->
        rinfixr Prec.List (infixr (string ":: ")) (p l) (p r)
    | Construct (id, None) ->
        return Prec.Construct (pp_id id)
    | Construct (id, Some arg) ->
        rinfixr Prec.Construct (infixr empty)
          (return Prec.Highest (pp_id id))
          (p arg)
    | Tuple list2 ->
        let op docs = group @@ flow (comma ^^ break 1) docs in
        rinfixn Prec.Tuple op (List.map (List2.to_list list2) ~f:p)
    | Or (x, y) ->
        rinfixl Prec.Or (infixl (string "| ")) (p x) (p y)
    | Constraint (pat, ty) ->
        let doc = parens @@ runf p pat ^/^ string ": " ^^ PpTy.pp ty in
        return Prec.Highest doc
  in

  runf p
