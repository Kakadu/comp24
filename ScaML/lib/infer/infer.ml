(** Copyright 2024-2025, Danil Slinchuk, Julia Kononova *)

(** SPDX-License-Identifier: MIT *)

(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

module TyError = TyError

open! Base
open Monads.Std
open Types
open LTypes

open Constraints
open Gen
open Solve

let close_over ty =
  let open Monad.State in
  let fresh =
    let* cnt = get () in
    let* () = put (cnt + 1) in
    (* try to use english letter. if out of bounds then "t{cnt}" *)
    let var =
      let default () = Var.Var ("t" ^ Int.to_string cnt) in
      (* 'a' = 97 *)
      match Char.of_int (cnt + 96) with
      | None ->
          default ()
      | Some ch when Char.( > ) ch 'z' ->
          default ()
      | Some ch ->
          Var.Var (Char.to_string ch)
    in
    return var
  in
  (* construct set of new variables
     and substitution that maps vars to new ones *)
  let m =
    VarSet.fold_right (Ty.vars ty)
      ~init:(return (VarSet.empty, Sub.empty))
      ~f:(fun var acc ->
        let* fresh_var = fresh in
        let single = Sub.singleton var (Ty_var fresh_var) in
        let* set, sub = acc in
        return (VarSet.add set fresh_var, Sub.compose single sub) )
  in
  let new_vars, subst = fst @@ Monad.State.run m 1 in
  (* quantify all type variables *)
  Scheme.Forall (new_vars, Sub.apply subst ty)

let infer_structure_item env str_item =
  let ( let* ) x f = Result.bind x ~f in
  let fail = Result.fail in
  let return = Result.return in

  (* let types_arity = Env.get_types_arity env in *)
  let* asm, bound_vars, ty_res, gen_cs, con_assumpt, _ = gen str_item in

  (* set arity in environment for newly defined types *)
  (* let env = *)
  (*   Env.set_types_arity env *)
  (*     (List.fold defined_types ~init:types_arity ~f:(fun acc {id; arity} -> *)
  (*          Map.set acc ~key:id ~data:arity ) ) *)
  (* in *)

  (* create new constrainsts based on type environment *)
  let* env_cs =
    Assumptions.fold asm ~init:(return ConstrSet.empty)
      ~f:(fun ~key:id ~data:vars acc ->
        let* acc = acc in

        (* try to find ident in type environment *)
        let* sc =
          Env.find env id
          |> Option.value_map
               ~default:(fail @@ TyError.UnboundVariable id)
               ~f:return
        in

        (* check that constructors are applied *)
        let* () =
          let mismatch = fail @@ TyError.ConstructorArityMismatch id in
          let assert_eq ar1 ar2 =
            if ConArityAssumpt.equal_arity ar1 ar2 then return () else mismatch
          in

          match ConArityAssumpt.find con_assumpt id with
          | None ->
              (* not a constructor *)
              return ()
          | Some arity -> (
            match sc with
            | Forall (_, Ty_con (_, _)) ->
                assert_eq arity NoArgs
            | Forall (_, Ty_arr (_, Ty_con (_, _))) ->
                assert_eq arity SomeArgs
            | _ ->
                mismatch )
        in

        (* add new constraints based on scheme from Env *)
        let new_cs =
          ConstrSet.of_list
          @@ List.map vars ~f:(fun var ->
                 Constr.ExplInstConstr (Ty_var var, sc) )
        in
        return @@ ConstrSet.union acc new_cs )
  in

  (* solve constraints *)
  let* sub = solve @@ ConstrSet.union gen_cs env_cs in
  let env = Sub.apply_to_env sub env in

  (* add new bounds to type environment *)
  let env =
    BoundVars.fold bound_vars ~init:env
      ~f:(fun ~key:id ~data:tv (acc : Env.t) ->
        let ty = Sub.apply sub (Ty_var tv) in
        let ty = close_over ty in
        Env.set acc ~key:id ~data:ty )
  in

  let ty_res, env =
    match ty_res with
    | None ->
        (None, env)
    | Some ty ->
        let ty = Sub.apply sub ty in
        let ty = close_over ty in
        (Some ty, env)
  in
  return (env, BoundVars.idents bound_vars, ty_res)
