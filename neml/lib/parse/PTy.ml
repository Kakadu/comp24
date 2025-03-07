[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

(* https://ocaml.org/manual/5.2/types.html#s:typexpr *)

open! Base
open Angstrom

open LTypes
open PCommon

(** 'a, 'b *)
let pvar = pty_var >>| fun id -> Ty.Var id

(** ('k, 'v) map *)
let pmulti_args_app pty =
  let* args = parens @@ sep_by1 (ws *> char ',') pty in
  let* id = ws *> pty_con_id in
  return (Ty.Con (id, args))

let poprnd pty =
  ws
  *> choice
       [ (pty_con_id >>| fun id -> Ty.Con (id, []))
       ; pvar
       ; pmulti_args_app pty
       ; parens pty ]

(**
  int list
  ('k, 'v) map option option
*)
let papp pty =
  let* arg = poprnd pty in
  let rec pcons acc =
    option acc (ws1 *> pty_con_id >>= fun id -> pcons (Ty.Con (id, [acc])))
  in
  pcons arg

let table =
  let ptuple = ws *> string "*" >>| fun _ list2 -> Ty.Tuple list2 in
  let parr = ws *> string "->" >>| fun _ lhs rhs -> Ty.Arr (lhs, rhs) in

  [[InfixN ptuple]; [InfixR parr]]

let p : Ty.t t = fix (fun pty -> poperators ~table ~poprnd:(papp pty))
