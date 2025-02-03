open Base
module Format = Stdlib.Format

type name = string
type var_id = int

(* Inferred types *)
type inf_type = 
  | TInt
  | TFloat
  | TBool
  | TChar
  | TString
  | TUnit
  | TList of inf_type (* 'a list *)
  | TTuple of inf_type list (* ('a, 'b, ...) *)
  | TArrow of inf_type * inf_type (* 'a -> 'a *)
  | TVar of var_id

type error =
  | Variable_not_found
  | Unification_failed of inf_type * inf_type

module VarSet = struct
  (** Set of variable ids *)
  include Stdlib.Set.Make (Int)
  
  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

type scheme = Scheme of VarSet.t * inf_type (* ∀α. α -> α *)

module R = struct
  (** Module for creating fresh variables *)
  type 'a t = var_id -> var_id * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Error x
    | Ok a -> f a last
  ;;

  let fail e st = st, Result.fail e
  let return x last = last, Result.return x
  let bind x f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun x f st ->
    match x st with
    | st, Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.Error e
  ;;

  module Syntax = struct
    let ( let* ) = bind
    let ( >>= ) = bind
  end

  module RMap = struct
    let fold_left map ~init ~f =
      Map.fold map ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f acc (key, data))
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let get_fresh : int t = fun last -> last, Result.Ok last
  let run m = snd (m 0)
end

module Type = struct
  (** Basic work with types *)
  type t = inf_type

  let rec occurs_in (id: var_id) = function
  (** Does the variable occur in the passed type *)
    | TBool | TInt | TFloat | TChar | TString | TUnit -> false
    | TList x -> occurs_in id x
    | TTuple tl -> List.fold tl ~init:false ~f:(fun acc t -> acc || occurs_in id t)
    | TArrow (x, y) -> occurs_in id x || occurs_in id y
    | TVar v_id -> id = v_id
  ;;

  let free_vars t =
    (** Collect free variables in the passed type *)
    let rec helper acc = function
      | TBool | TInt | TFloat | TChar | TString | TUnit -> acc
      | TVar v -> VarSet.add v acc
      | TArrow (x, y) -> helper (helper acc x) y
      | TList x -> helper acc x
      | TTuple tl -> List.fold tl ~init:acc ~f:helper
    in
    helper VarSet.empty t
  ;;
end

module Subst = struct
  (** Substitution logic *)
  open R
  open R.Syntax

  (* var_id => inf_type (substitute it) *)
  type t = (var_id, inf_type, Int.comparator_witness) Map.t

  let empty : t = Map.empty (module Int)
  let singleton k v = Map.singleton (module Int) k v

  let singleton_checked id t =
    if Type.occurs_in id t
    then Variable_not_found |> R.fail
    else singleton id t |> R.return
  ;;

  let find key s = Map.find s key
  let remove k s = Map.remove s k

  let apply t s =
    (** Apply a substitution to type, return new type *)
    let rec helper x = match x with
      | TVar v -> let find_type = find v s in 
        (match find_type with
          | Some ft -> ft
          | _ -> x)
      | TArrow (x, e) -> TArrow (helper x, helper e)
      | TList x -> TList (helper x)
      | TTuple tl -> TTuple (List.map ~f:helper tl)
      | _ -> x
    in
    helper t
  ;;

  let rec unify t1 t2 =
    match t1, t2 with
    (* Returns the substitutions needed to unify types *)
    | TBool, TBool | TInt, TInt | TFloat, TFloat | TChar, TChar | TString, TString | TUnit, TUnit -> return empty
    | TVar x, TVar y when x = y -> return empty
    | TVar x, (_ as y) | (_ as y), TVar x -> singleton_checked x y
    | TArrow (x, xs), TArrow (y, ys) ->
      let* s1 = unify x y in
      let* s2 = unify (apply xs s1) (apply ys s1) in
      compose s1 s2
    | TList x, TList y -> unify x y
    | TTuple xs, TTuple ys -> let unify_lists l1 l2 =
      let subs =
        List.fold2 l1 l2 ~init:(return empty) ~f:(fun subs a b ->
          let* subs = subs in
          let sa = apply a subs  in
          let sb = apply b subs in
          let* sub1 = unify sa sb in
          compose subs sub1)
      in
      match subs with
      | Ok res -> res
      | Unequal_lengths -> fail (Unification_failed (t1, t2))
    in unify_lists xs ys
    | _ -> fail (Unification_failed (t1, t2)) 

  and extend (id, t) s =
    (** Add new substitution *)
    match find id s with
    | Some t2 ->
      let* unified = unify t t2 in
      compose s unified
    | None ->
      let t = apply t s in
      let* new_s = singleton_checked id t in
      Map.fold s ~init:(return new_s) ~f:(fun ~key ~data acc ->
        let* acc = acc in
        let data = apply data acc in
        let* _ = singleton_checked key data in
        Map.set acc ~key ~data |> return)

  and compose before after =
    (** Combine two substitutions into one *)
    Map.fold after ~init:(return before) ~f:(fun ~key ~data acc ->
      let* acc = acc in
      extend (key, data) acc)
  ;;
  
  let compose_all ss =  
    List.fold ss ~init:(return empty) ~f:(fun acc s ->
      let* acc = acc in
      compose acc s)
  ;;
end