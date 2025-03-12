[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

open LMisc
open LAst

open MCommon

(** C(losure)Less IR *)

type cl =
  | Id of IdTagged.t
  | Const of Const.t
  | Apply of cl * cl
  | If of cl * cl * cl
  | Seq of cl List2.t

type def = cl FuncDef.t
type t = def list * cl

let rec to_expr : cl -> Expr.t = function
  | Id (id, _) ->
      Id id
  | Const const ->
      Const.to_expr const
  | Apply (cl1, cl2) ->
      Apply (to_expr cl1, to_expr cl2)
  | If (ccond, cthen, celse) ->
      If (to_expr ccond, to_expr cthen, Some (to_expr celse))
  | Seq cls ->
      Seq (List2.map cls ~f:to_expr)

let to_structure ((defs, cl) : t) : structure =
  List.fold_right defs
    ~init:[Eval (to_expr cl)]
    ~f:(fun def acc -> FuncDef.to_stritem to_expr def :: acc)

let subst ~(from : IdTagged.t) ~(to_ : IdTagged.t) : cl -> cl =
  let rec f = function
    | Id id when IdTagged.equal id from ->
        Id to_
    | Apply (cl1, cl2) ->
        Apply (f cl1, f cl2)
    | Seq cls ->
        Seq (List2.map cls ~f)
    | If (ccond, cthen, celse) ->
        If (f ccond, f cthen, f celse)
    | (Id _ | Const _) as cl ->
        cl
  in
  f

(** Converts Simpl to C(losure)Less IR.
    Performs closure conversion and lambda lifting *)
let from_simpl (globals : IdSet.t) (sim : MSimpl.t) : t =
  let cnt = ref (-1) in
  let defs : def list ref = ref [] in

  (* lifts lambda to the top level with the new generated name *)
  let define ~(recf : MSimpl.rec_flag) ~(args : Id.t List1.t) ~(body : cl) :
      IdTagged.t =
    let fresh : IdTagged.t =
      cnt := !cnt + 1 ;
      (I ("f" ^ Int.to_string !cnt), Gen)
    in

    let def : def =
      match recf with
      | Nonrec ->
          Func {recf= Nonrec; id= fresh; args; body}
      | Rec id_rec ->
          Func
            { recf= Rec
            ; id= fresh
            ; args
            ; body= subst ~from:(id_rec, User) ~to_:fresh body }
    in

    defs := def :: !defs ;
    fresh
  in

  let rec f : MSimpl.t -> cl = function
    | Fun (recf, args, sim) ->
        let args = List1.to_list args in
        let bound =
          (match recf with Nonrec -> args | Rec id_rec -> id_rec :: args)
          |> IdSet.of_list |> Set.union globals
        in
        let free = Set.diff (MSimpl.free sim) bound |> Set.to_list in

        let id_func =
          define ~recf
            ~args:(List.concat [free; args] |> List1.of_list_exn)
            ~body:(f sim)
        in
        List.fold free ~init:(Id id_func) ~f:(fun acc id ->
            Apply (acc, Id (id, User)) )
    | Id id ->
        Id (id, User)
    | Apply (sim1, sim2) ->
        Apply (f sim1, f sim2)
    | If (scond, sthen, selse) ->
        If (f scond, f sthen, f selse)
    | Seq sims ->
        Seq (List2.map sims ~f)
    | Const const ->
        Const const
  in

  let cl = f sim in
  (List.rev !defs, cl)
