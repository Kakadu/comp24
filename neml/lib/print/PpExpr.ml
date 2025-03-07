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
  type t = Open | Seq | Tuple | List | Apply | Highest [@@deriving enum]
  let parens = parens
end

let pcase left right =
  group @@ left ^^ string " ->" ^^ group (nest 2 (break 1 ^^ right))

let pp : Expr.t -> document =
  let open Expr in
  let open PrecedencePrinter (Prec) in
  let rec p = function
    | Id id ->
        return Prec.Highest (pp_id id)
    | Const x ->
        return Prec.Highest (pp_const x)
    | Let (rec_flag, bindings, expr) ->
        let doc = plet PpPat.pp (runf p) rec_flag bindings (Some expr) in
        return Prec.Open doc
    | Fun (args, expr) ->
        let args = List.map (List1.to_list args) ~f:PpPat.pp in
        let expr = runf p expr in

        let doc =
          group @@ string "fun"
          ^^ group (break 1)
          ^^ flow (break 1) args
          ^^ string " ->"
          ^^ nest 2 (break 1 ^^ expr)
        in
        return Prec.Open doc
    | Function cases ->
        let cases =
          List.map (List1.to_list cases) ~f:(fun {pat; expr} ->
              pcase (PpPat.pp pat) (runf p expr) )
        in

        let doc =
          group @@ string "function"
          ^/^ ifflat empty (string "| ")
          ^^ separate (break 1 ^^ string "| ") cases
        in
        return Prec.Open doc
    | Match (expr, cases) ->
        let expr = runf p expr in
        let cases =
          List.map (List1.to_list cases) ~f:(fun {pat; expr} ->
              pcase (PpPat.pp pat) (runf p expr) )
        in

        let doc =
          group @@ string "match"
          ^^ nest 2 (break 1 ^^ expr)
          ^/^ string "with"
          ^^ group
               ( break 1
               ^^ ifflat empty (string "| ")
               ^^ separate (break 1 ^^ string "| ") cases )
        in
        return Prec.Open doc
    | Apply (expr, arg) ->
        let op expr arg = group @@ expr ^^ group (nest 2 (break 1 ^^ arg)) in
        rinfixl Prec.Apply op (p expr) (p arg)
    | If (if_, then_, else_) ->
        let if_ = runf p if_ in
        let then_ = runf p then_ in

        let else_ = Option.map ~f:(runf p) else_ in
        let else_ =
          optional
            (fun else_ ->
              group @@ break 1 ^^ string "else"
              ^^ group (nest 2 (break 1 ^^ else_)) )
            else_
        in

        let doc =
          group @@ string "if"
          ^^ group (nest 2 (break 1 ^^ if_))
          ^/^ string "then"
          ^^ group (nest 2 (break 1 ^^ then_))
          ^^ else_
        in
        return Prec.Open doc
    | Seq list2 ->
        let op docs = group @@ flow (semi ^^ break 1) docs in
        rinfixn Prec.Seq op (List.map (List2.to_list list2) ~f:p)
    | Tuple list2 ->
        let op docs = group @@ flow (comma ^^ break 1) docs in
        rinfixn Prec.Tuple op (List.map (List2.to_list list2) ~f:p)
    | Construct (id, None) ->
        return Prec.Apply (pp_id id)
    | Construct (I "::", Some (Tuple (l, r, []))) ->
        rinfixr Prec.List (infixr (string ":: ")) (p l) (p r)
    | Construct (id, Some arg) ->
        rinfixl Prec.Apply (infixl empty)
          (return Prec.Highest (pp_id id))
          (p arg)
    | Constraint (expr, ty) ->
        let doc = parens @@ runf p expr ^/^ string ": " ^^ PpTy.pp ty in
        return Prec.Highest doc
  in

  runf p
