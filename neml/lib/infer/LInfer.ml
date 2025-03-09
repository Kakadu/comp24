[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

open LMisc
open LAst
open LTypes

open ICommon

module IError = IError

let debug = debug

(** Rename vars in type to a, b, ... z, t1, t2, ...*)
let rename (ty : Ty.t) : Ty.t =
  let next cnt : Ty.t =
    let () = cnt := !cnt + 1 in

    let a = Char.to_int 'a' in
    let z = Char.to_int 'z' in
    let code = !cnt + a in

    if code <= z then Var (V (Char.of_int_exn code |> Char.to_string))
    else Var (V ("t" ^ Int.to_string !cnt))
  in

  let open ISolve in
  let sub = ref Sub.empty in

  let cnt = ref (-1) in
  let rec traverse : Ty.t -> unit = function
    | Var var when not (Map.mem !sub var) ->
        sub := Map.set !sub ~key:var ~data:(next cnt)
    | Var _ ->
        ()
    | Arr (ty1, ty2) ->
        traverse ty1 ; traverse ty2
    | Tuple tys ->
        List.iter (List2.to_list tys) ~f:traverse
    | Con (_, tys) ->
        List.iter tys ~f:traverse
  in
  traverse ty ;

  ISolve.Sub.apply !sub ty

module Env = struct
  (** Type environment contains info about defined types
      & identifiers' bindings to their types *)
  type t = {types: DefTys.t; bounds: (Id.t, Ty.t, Id.comparator_witness) Map.t}

  let empty : t = {types= DefTys.empty; bounds= Map.empty (module Id)}
end

type output = {ty: Ty.t option; env: Env.t; bounds: Id.t list}

let infer (env : Env.t) (item : StrItem.t) : (output, IError.t) Result.t =
  let open Result in
  let ( let* ) = ( >>= ) in

  (* generate type constraints *)
  let* {defined_types= new_types; assumptions= asm; bounds; constraints= cs; ty}
      =
    IGen.gen env.types item
  in

  (* persist newly defined types in environment *)
  let env = {env with types= new_types} in

  (* everything left in assumptions are unknown identifiers.
     try to find them in environment and assign respective type
     by adding ExplInst constraints *)
  let* cs =
    Map.fold asm ~init:(return cs) ~f:(fun ~key:id ~data:vars acc ->
        let* (acc : ConSet.t) = acc in

        let* sc =
          Map.find env.bounds id
          |> Option.value_map ~default:(fail (IError.UnboundVariable id))
               ~f:(fun ty -> return (generalize VarSet.empty ty) )
        in
        Set.fold vars ~init:acc ~f:(fun acc var ->
            Set.add acc (ExplInst (Var var, sc)) )
        |> return )
  in

  (* solve type constraints *)
  let* sub = ISolve.solve cs in
  let close ty = ISolve.Sub.apply sub ty |> rename in

  (* apply substitution & add new bounds to type environment *)
  let new_bounds =
    Map.fold bounds ~init:env.bounds ~f:(fun ~key ~data:var ->
        Map.set ~key ~data:(close (Var var)) )
  in
  let env = {env with bounds= new_bounds} in
  let ty = Option.map ty ~f:close in

  return {ty; env; bounds= Map.keys bounds}
