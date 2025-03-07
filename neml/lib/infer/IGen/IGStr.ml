[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

open LMisc
open LAst
open LTypes

open ICommon
open IGCommon
open IGMonad

(** Check for type arity mismatches and unbound type variables & types *)
let check_type (deftys : DefTys.t) (params : VarSet.t) : Ty.t -> unit t =
  let rec f : Ty.t -> unit t = function
    | Var var when Set.mem params var ->
        return ()
    | Var var ->
        fail (UnboundTypeVariable var)
    | Arr (ty1, ty2) ->
        many [ty1; ty2]
    | Tuple tys ->
        many (List2.to_list tys)
    | Con (id, args) ->
        let* arity =
          Map.find deftys id
          |> Option.value_map ~default:(fail (UnboundType id)) ~f:return
        in
        if List.length args = arity then many args
        else fail (TypeArityMismatch id)
  and many (tys : Ty.t list) : unit t =
    fold tys ~dir:`Left ~init:() ~f:(fun () -> f)
  in
  f

let gen (deftys : DefTys.t) :
    StrItem.t -> (DefTys.t * As.t * Bounds.t * Ty.t option) t = function
  | Eval expr ->
      let* asm, ty = IGExpr.gen expr in
      return (deftys, asm, Bounds.empty, Some ty)
  | Let (recf, bindings) ->
      let* asm, bounds = IGExpr.gen_let recf (List1.to_list bindings) in
      return (deftys, asm, bounds, None)
  | Type {id; params; variants} ->
      let deftys = Map.set deftys ~key:id ~data:(Set.length params) in
      let defined : Ty.t =
        Con (id, List.map (Set.to_list params) ~f:(fun var -> Ty.Var var))
      in

      let* bounds =
        fold ~dir:`Left variants ~init:Bounds.empty
          ~f:(fun acc {id= id_con; arg} ->
            let* var = fresh in

            let* () =
              match arg with
              | None ->
                  cs [Var var == defined]
              | Some arg ->
                  let* () = check_type deftys params arg in
                  cs [Var var == Arr (arg, defined)]
            in

            return (Map.set acc ~key:id_con ~data:var) )
      in

      return (deftys, As.empty, bounds, None)
