[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

open LMisc
open MCommon

(** Middleend optimizations *)

(** Groups chained functions *)
let group_funs =
  let to_idset (l : Id.t List1.t) : IdSet.t =
    List1.to_list l |> Set.of_list (module Id)
  in

  let rec f : MSimpl.t -> MSimpl.t = function
    | Fun (Nonrec, args0, Fun (Nonrec, args1, body)) ->
        (* fun ARGS0 -> fun ARGS1 -> BODY
           -> fun ARGS0 .. ARGS1 -> BODY *)
        let args0 = List1.to_list args0 in
        let args1 = List1.to_list args1 in
        let args = List.concat [args0; args1] |> List1.of_list_exn in
        f (Fun (Nonrec, args, body))
    | Apply (Fun (Nonrec, args0, Apply (Fun (Nonrec, args1, body), s1)), s2)
      when Set.is_empty @@ Set.inter (to_idset args0) (MSimpl.free s1) ->
        (* (fun ARGS0 -> (fun ARGS1 -> BODY) S1) S2
           -> (fun ARGS0 .. ARGS1 BODY) S2 S1 *)
        let args0 = List1.to_list args0 in
        let args1 = List1.to_list args1 in
        let args = List.concat [args0; args1] |> List1.of_list_exn in
        f (Apply (Apply (Fun (Nonrec, args, body), s2), s1))
    | Fun (recf, args, sim) ->
        Fun (recf, args, f sim)
    | Apply (sim1, sim2) ->
        Apply (f sim1, f sim2)
    | (Id _ | Const _) as sim ->
        sim
    | If (scond, sthen, selse) ->
        If (f scond, f sthen, f selse)
    | Seq sims ->
        Seq (List2.map sims ~f)
  in
  f

let opt = group_funs
