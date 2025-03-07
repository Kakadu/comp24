[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open LMisc
open LAst
open LTypes

let debug = ref false

module IError = struct
  type t =
    | UnboundVariable of Id.t
        (** Failed to find a variable in the environment *)
    | UnificationFail of Ty.t * Ty.t  (** Failed to unify two types *)
    | UnificationMismatch of Ty.t list * Ty.t list
        (** Lists of types to unify have different lengths *)
    | UnboundType of Id.t
        (** Type is unbound in type declaration.
            E.g. `type bar = Bar of foo` (foo is unbound) *)
    | UnboundTypeVariable of Var.t
        (** Type variable is unbound in type declaration.
            E.g. `type bar = Bar of 'a` ('a is unbound) *)
    | TypeArityMismatch of Id.t
        (** Type expects more/less arguments than applied.
            E.g. `type foo = Foo` (foo expects 0 arguments)
           `type bar = Bar of int foo` *)
    | OccursIn of Var.t * Ty.t  (** Type variable occurs in a type *)
    | PatVarBoundSeveralTimes of Id.t
        (** Pattern(s) bound the same variable several times. E.g. `let x, x = ...` *)
    | NotVarLHSRec of Pat.t
        (** The left hand side of the recursive binding is not a var.
            E.g. `let rec _ = ...` *)
    | NotAllowedRHSRec of Expr.t
        (** The expression is not allowed on the right-hand side of `let rec'.
            E.g. `let rec x = x + 1` *)
    | NotImplemented of string  (** Too bad something's not done *)
  [@@deriving show {with_path= false}]
end

module Sc = struct
  (** Type with universally quantified type variables *)
  type t = Forall of VarSet.t * Ty.t
  [@@deriving show {with_path= false}, ord, sexp_of]

  let vars : t -> VarSet.t =
   fun (Forall (quantified, ty)) -> Set.diff (Ty.vars ty) quantified
end

let generalize (bound : VarSet.t) (ty : Ty.t) : Sc.t =
  Forall (Set.diff (Ty.vars ty) bound, ty)

module Con = struct
  module T = struct
    (** Type constraints *)
    type t =
      | TyEq of Ty.t * Ty.t  (** Reflects that types should be unified *)
      | ImplInst of Ty.t * VarSet.t * Ty.t
          (** States that t1 should be an instance of the type scheme
            that is obtained by generalizing type t2 with respect
            to the set of monomorphic type variables M *)
      | ExplInst of Ty.t * Sc.t
          (** States that ty has to be a generic instance of the type scheme *)
    [@@deriving show {with_path= false}, ord, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

module ConSet = struct
  (** Set of type constraints *)
  type t = (Con.t, Con.comparator_witness) Set.t

  let empty : t = Set.empty (module Con)
  let single x : t = Set.singleton (module Con) x
end

module DefTys = struct
  (** Holds defined types' names and their arity *)
  type t = (Id.t, Int.t, Id.comparator_witness) Map.t

  let empty : t = Map.empty (module Id)
  let single x y : t = Map.singleton (module Id) x y
end
