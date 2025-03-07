[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

(* https://ocaml.org/manual/5.2/patterns.html *)

open! Base
open Angstrom

open LMisc
open LAst
open PCommon

let pvar = pvalue_id >>| function I "_" -> Pat.Any | id -> Pat.Var id
let pconst = pconst >>| fun const -> Pat.Const const

(** [Cons (hd, tl)] *)
let pconstruct poprnd =
  let* id = pconstruct_id in
  let* arg = opt poprnd in
  return (Pat.Construct (id, arg))

(** [a; b; c] *)
let plist ppat =
  let pelements =
    sep_by1 (ws *> char ';') ppat
    >>| List.fold_right
          ~init:(Pat.Construct (Id.I "[]", None))
          ~f:(fun pat acc ->
            Pat.Construct (Id.I "::", Some (Pat.Tuple (pat, acc, []))) )
  in
  char '[' *> pelements <* ws <* opt (char ';') <* ws <* char ']'

(**
  [ (pat) ]
  [ (pat: ty) ]
*)
let pparens ppat =
  let p =
    let* pat = ppat in
    opt (ws *> char ':')
    >>= function
    | None -> return pat | Some _ -> PTy.p >>| fun ty -> Pat.Constraint (pat, ty)
  in
  parens p

let poprnd ppat =
  fix (fun poprnd ->
      ws *> choice [pvar; pconst; pconstruct poprnd; plist ppat; pparens ppat] )

(* ======= Operators ======= *)

let table =
  let plist =
    ws *> string "::"
    >>| fun _ lhs rhs ->
    Pat.Construct (Id.I "::", Some (Pat.Tuple (lhs, rhs, [])))
  in
  let ptuple = ws *> string "," >>| fun _ list2 -> Pat.Tuple list2 in
  let por = ws *> string "|" >>| fun _ lhs rhs -> Pat.Or (lhs, rhs) in

  [[InfixR plist]; [InfixN ptuple]; [InfixL por]]

let p : Pat.t t = fix (fun ppat -> poperators ~table ~poprnd:(poprnd ppat))
