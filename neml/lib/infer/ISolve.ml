[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

open LMisc
open LTypes

open ICommon

module Sub = struct
  (** Substitution maps type variables to types *)
  type t = (Var.t, Ty.t, Var.comparator_witness) Map.t

  let empty : t = Map.empty (module Var)
  let single x y : t = Map.singleton (module Var) x y

  let apply (sub : t) : Ty.t -> Ty.t =
    let rec f = function
      | Ty.Var var as ty ->
          Map.find sub var |> Option.value ~default:ty
      | Arr (ty1, ty2) ->
          Arr (f ty1, f ty2)
      | Tuple tys ->
          Tuple (List2.map tys ~f)
      | Con (id, args) ->
          Con (id, List.map args ~f)
    in
    f

  let compose s1 s2 =
    Map.merge_skewed
      ~combine:(fun ~key:_ _ v2 -> v2)
      (Map.map s2 ~f:(apply s1))
      s1

  let apply_sc : t -> Sc.t -> Sc.t =
   fun sub (Forall (quantified, ty)) ->
    (* remove quantified vars from substitution *)
    let sub = Set.fold quantified ~init:sub ~f:Map.remove in
    Forall (quantified, apply sub ty)

  let apply_varset : t -> VarSet.t -> VarSet.t =
   fun sub ->
    (* construct new varset by adding all vars occuring in types
       on the right hand side of respective substitutions *)
    Set.fold ~init:VarSet.empty ~f:(fun acc var ->
        let vars =
          Map.find sub var
          |> Option.value_map ~default:(VarSet.single var) ~f:Ty.vars
        in
        Set.union acc vars )

  let apply_conset : t -> ConSet.t -> ConSet.t =
   fun sub ->
    let f : Con.t -> Con.t = function
      | TyEq (ty1, ty2) ->
          TyEq (apply sub ty1, apply sub ty2)
      | ImplInst (ty1, bound, ty2) ->
          ImplInst (apply sub ty1, apply_varset sub bound, apply sub ty2)
      | ExplInst (ty, sc) ->
          ExplInst (apply sub ty, apply_sc sub sc)
    in
    Set.map (module Con) ~f
end

module Monad : sig
  include MONAD

  val run : 'a t -> ('a, IError.t) Result.t
  val fail : IError.t -> 'a t

  val fresh : Var.t t
  (** Generate fresh type variable *)
end = struct
  type state = {counter: int}

  include
    MakeSEMonad
      (struct
        type t = state
      end)
      (IError)

  let run m = run m {counter= 0} |> snd

  let fresh =
    let* {counter} = get in
    let* () = put {counter= counter + 1} in
    return (Var.V ("solve" ^ Int.to_string counter))
end

open Monad

let occurs_check var ty = Set.mem (Ty.vars ty) var

(** Tries to construct substitution that's
    when applied to both types would yield unified type *)
let rec unify (ty1 : Ty.t) (ty2 : Ty.t) : Sub.t t =
  match (ty1, ty2) with
  | _, _ when Ty.equal ty1 ty2 ->
      return Sub.empty
  | Var var, ty | ty, Var var ->
      if occurs_check var ty then fail (OccursIn (var, ty))
      else return (Sub.single var ty)
  | Arr (l1, r1), Arr (l2, r2) ->
      unify_many [l1; r1] [l2; r2]
  | Tuple tys1, Tuple tys2 ->
      unify_many (List2.to_list tys1) (List2.to_list tys2)
  | Con (id1, tys1), Con (id2, tys2) when Id.equal id1 id2 ->
      unify_many tys1 tys2
  | _ ->
      fail (UnificationFail (ty1, ty2))

and unify_many (tys1 : Ty.t list) (tys2 : Ty.t list) =
  List.fold2 tys1 tys2 ~init:(return Sub.empty) ~f:(fun acc ty1 ty2 ->
      let* acc = acc in
      let* sub = unify (Sub.apply acc ty1) (Sub.apply acc ty2) in
      return (Sub.compose acc sub) )
  |> function
  | Unequal_lengths -> fail (UnificationMismatch (tys1, tys2)) | Ok x -> x

let instantiate : Sc.t -> Ty.t t =
 fun (Forall (quantified, ty)) ->
  let* sub =
    Set.fold quantified ~init:(return Sub.empty) ~f:(fun acc qvar ->
        let* acc = acc in
        let* var = fresh in
        return (Sub.compose acc (Sub.single qvar (Var var))) )
  in
  return (Sub.apply sub ty)

let activevars : ConSet.t -> VarSet.t =
  let f : Con.t -> VarSet.t = function
    | TyEq (ty1, ty2) ->
        Set.union (Ty.vars ty1) (Ty.vars ty2)
    | ImplInst (ty1, bound, ty2) ->
        Set.union (Ty.vars ty1) (Set.inter bound (Ty.vars ty2))
    | ExplInst (ty, sc) ->
        Set.union (Ty.vars ty) (Sc.vars sc)
  in
  Set.fold ~init:VarSet.empty ~f:(fun acc con -> Set.union acc (f con))

let rec solve_cs (cs : ConSet.t) : Sub.t t =
  (* find next constraint that should be solved *)
  let next_solvable : (Con.t * ConSet.t) option =
    Set.find_map cs ~f:(fun (con : Con.t) ->
        let rest = Set.remove cs con in
        let ok = Some (con, rest) in
        let no = None in

        (* enforce order in which constraints must be solved *)
        match con with
        | TyEq _ | ExplInst _ ->
            ok
        | ImplInst (_, bound, ty2)
        (* solve ImplInst only after all other constraints on ty2 are solved *)
          when Set.is_empty
               @@ Set.inter (activevars rest) (Set.diff (Ty.vars ty2) bound) ->
            ok
        | ImplInst _ ->
            no )
  in
  match next_solvable with
  | Some (con, rest) ->
      (* found some constraint to solve *)
      solve_con con rest
  | None ->
      (* no constraint left to solve.
         that can only happen if constraint set is empty.
         otherwise there's bug in the algo *)
      assert (Set.is_empty cs) ;
      return Sub.empty

and solve_con (con : Con.t) (rest : ConSet.t) : Sub.t t =
  if !debug then Stdlib.Format.printf "solving %a\n" Con.pp con ;
  match con with
  | TyEq (ty1, ty2) ->
      let* sub1 = unify ty1 ty2 in
      let* sub2 = solve_cs (Sub.apply_conset sub1 rest) in
      return (Sub.compose sub2 sub1)
  | ImplInst (ty1, bound, ty2) ->
      solve_cs (Set.add rest (ExplInst (ty1, generalize bound ty2)))
  | ExplInst (ty, sc) ->
      let* inst = instantiate sc in
      solve_cs (Set.add rest (TyEq (ty, inst)))

let solve (cs : ConSet.t) : (Sub.t, IError.t) Result.t = run (solve_cs cs)
