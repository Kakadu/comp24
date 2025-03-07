[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

open LMisc
open LAst
open LTypes

open ICommon

module IGMonad : sig
  include MONAD

  val run : 'a t -> ConSet.t * ('a, IError.t) Result.t
  val fail : IError.t -> 'a t

  val fresh : Var.t t
  (** Generate fresh type variable *)

  val cs : Con.t list -> unit t
  (** Save new type constraints *)

  val bound_vars : VarSet.t t
  (** Get current bound type variables *)

  val extend_vars : VarSet.t -> 'a t -> 'a t
  (** Run computation with extended bound vars *)

  val fold :
       'a list
    -> dir:[`Left | `Right]
    -> init:'acc
    -> f:('acc -> 'a -> 'acc t)
    -> 'acc t
end = struct
  type state = {counter: int; conset: ConSet.t; bound_vars: VarSet.t}

  include
    MakeSEMonad
      (struct
        type t = state
      end)
      (IError)

  let run m =
    let {conset; _}, x =
      run m {counter= 0; conset= ConSet.empty; bound_vars= VarSet.empty}
    in
    (conset, x)

  let fresh =
    let* st = get in
    let* () = put {st with counter= st.counter + 1} in
    return (Var.V ("gen" ^ Int.to_string st.counter))

  let cs new_cs =
    let* st = get in
    put {st with conset= Set.union st.conset (Set.of_list (module Con) new_cs)}

  let bound_vars =
    let* {bound_vars; _} = get in
    return bound_vars

  let extend_vars vars m =
    let* st = get in
    let* () = put {st with bound_vars= Set.union st.bound_vars vars} in

    let* x = m in
    let* new_st = get in
    let* () = put {new_st with bound_vars= st.bound_vars} in
    return x

  let fold l ~dir ~init ~f =
    let init = return init in
    let f acc x =
      let* acc = acc in
      f acc x
    in
    match dir with
    | `Left ->
        List.fold_left l ~init ~f
    | `Right ->
        List.fold_right l ~init ~f:(fun acc x -> f x acc)
end

module As = struct
  (**
    Assumptions about identifiers.
    Maps identifiers to a set of type variables
    that represent identifier's supposed type
  *)
  type t = (Id.t, VarSet.t, Id.comparator_witness) Map.t

  let empty : t = Map.empty (module Id)
  let single x y : t = Map.singleton (module Id) x y
  let merge = Map.merge_skewed ~combine:(fun ~key:_ -> Set.union)
end

module Bounds = struct
  (** Represents identifiers bound in patterns *)
  type t = (Id.t, Var.t, Id.comparator_witness) Map.t

  let empty : t = Map.empty (module Id)
  let single x y : t = Map.singleton (module Id) x y

  exception Rebound of Id.t
  let merge m1 m2 =
    let open IGMonad in
    try
      return
      @@ Map.merge_skewed m1 m2 ~combine:(fun ~key:id _ _ ->
             raise (Rebound id) )
    with Rebound id -> fail (PatVarBoundSeveralTimes id)
end

let typeof_const : Const.t -> Ty.t = function
  | Int _ ->
      Ty.int
  | Char _ ->
      Ty.char
  | String _ ->
      Ty.string

let ( == ) t1 t2 = Con.TyEq (t1, t2)
let ( ++ ) = As.merge
let ( -- ) asm = List.fold ~init:asm ~f:Map.remove
