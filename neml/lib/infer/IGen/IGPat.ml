[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

open LMisc
open LAst
open LTypes

open IGCommon
open IGMonad

let rec gen : Pat.t -> (As.t * Bounds.t * Ty.t) IGMonad.t = function
  | Var id ->
      let* var = fresh in
      return (As.empty, Bounds.single id var, Ty.Var var)
  | Any ->
      let* var = fresh in
      return (As.empty, Bounds.empty, Ty.Var var)
  | Const const ->
      return (As.empty, Bounds.empty, typeof_const const)
  | Tuple pats ->
      let* asm, bounds, tys = gen_many ~dir:`Right (List2.to_list pats) in
      return (asm, bounds, Ty.Tuple (List2.of_list_exn tys))
  | Constraint (pat, ty) ->
      let* asm, bounds, ty_pat = gen pat in
      let* () = cs [ty_pat == ty] in
      return (asm, bounds, ty_pat)
  | Construct (id, arg) ->
      let* var = fresh in
      let as_con = As.single id (VarSet.single var) in
      let ty_con = Ty.Var var in

      let* ty_res = fresh >>| fun var -> Ty.Var var in
      let* as_arg, bounds_arg =
        match arg with
        | None ->
            let* () = cs [ty_con == ty_res] in
            return (As.empty, Bounds.empty)
        | Some arg ->
            let* as_arg, bounds_arg, ty_arg = gen arg in
            let* () = cs [ty_con == Arr (ty_arg, ty_res)] in
            return (as_arg, bounds_arg)
      in

      return (as_con ++ as_arg, bounds_arg, ty_res)
  | Or _ ->
      fail (NotImplemented "or patterns")

and gen_many :
       dir:[`Left | `Right]
    -> Pat.t list
    -> (As.t * Bounds.t * Ty.t list) IGMonad.t =
 fun ~dir l ->
  fold ~dir l ~init:(As.empty, Bounds.empty, [])
    ~f:(fun (as_acc, bounds_acc, tys_acc) pat ->
      let* as_pat, bounds_pat, ty_pat = gen pat in
      let* bounds = Bounds.merge bounds_acc bounds_pat in
      return (as_acc ++ as_pat, bounds, ty_pat :: tys_acc) )
