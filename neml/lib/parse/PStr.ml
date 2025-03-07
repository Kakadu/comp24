[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

(* https://ocaml.org/manual/5.2/modules.html#s:module-expr *)

open! Base
open Angstrom

open LAst
open LTypes
open PCommon

(**
  [let P1 = E1 and P2 = E2 and ...]
  [let rec ValId1 PArg1 = E1 and P1 = E2 and ...]
*)
let plet =
  let* rec_flag, bindings = plet PExpr.p PPat.p PTy.p in
  opt @@ spaced (string "in")
  >>= function
  | None ->
      return (StrItem.Let (rec_flag, bindings))
  | Some _ ->
      PExpr.p >>| fun expr -> StrItem.Eval (Expr.Let (rec_flag, bindings, expr))

let pty_params_ =
  let pmultiple = parens @@ sep_by1 (ws *> char ',') (ws *> pty_var) in
  let psingle = pty_var >>| List.return in
  psingle <|> pmultiple <|> return []

(** [Foo of string], [Bar of int] *)
let pconstruct_decl_ =
  let* id = pconstruct_id in
  let* arg =
    opt @@ spaced (string "of")
    >>= function None -> return None | Some _ -> PTy.p >>| Option.some
  in
  return StrItem.{id; arg}

(** [type foo = Foo of string | Bar of int] *)
let pty_decl =
  let* params =
    string "type" *> ws1 *> pty_params_ >>| Set.of_list (module Var)
  in
  let* id = ws *> pty_con_id in
  let* variants =
    option []
    @@ ws *> string "=" *> ws
       *> opt (string "|")
       *> sep_by1 (ws *> char '|') (ws *> pconstruct_decl_)
  in
  return (StrItem.Type {id; params; variants})

let peval = PExpr.p >>| fun expr -> StrItem.Eval expr

let pstr_item = ws *> choice [plet; pty_decl; peval]

let p : structure t =
  let sep = ws <* string ";;" <|> ws1 in
  sep_by sep pstr_item <* ws <* opt (string ";;")
