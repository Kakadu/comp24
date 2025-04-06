(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Me_ast
open Base

module StateMonad : sig
  include Base.Monad.Infix

  val return : 'a -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
    val fold_right : 'a list -> init:'b t -> f:('a -> 'b -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold_left
      :  ('a, 'b, 'c) Base.Map.t
      -> init:'d t
      -> f:('a -> 'b -> 'd -> 'd t)
      -> 'd t
  end

  val fresh : int t
  val run : 'a t -> 'a
end = struct
  type 'a t = int -> int * 'a (* State and Result monad composition *)

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f s ->
    let s', v' = m s in
    f v' s'
  ;;

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun m f s ->
    let s', x = m s in
    s', f x
  ;;

  let return v last = last, v
  let bind x ~f = x >>= f
  let fresh last = last + 1, last (* Get new state *)
  let ( let* ) x f = bind x ~f (* Syntax sugar for bind *)

  module RMap = struct
    (* Classic map folding. *)
    let fold_left mp ~init ~f =
      Base.Map.fold mp ~init ~f:(fun ~key ~data acc ->
        let* acc = acc in
        f key data acc)
    ;;
  end

  module RList = struct
    (* Classic list folding. *)
    let fold_left lt ~init ~f =
      Base.List.fold_left lt ~init ~f:(fun acc item ->
        let* acc = acc in
        f acc item)
    ;;

    let fold_right lt ~init ~f =
      Base.List.fold_right lt ~init ~f:(fun item acc ->
        let* acc = acc in
        f item acc)
    ;;
  end

  (* Run and get the internal value. *)
  let run m = snd (m 0)
end

let get_new_id n name = Base.String.concat [ name; "_ll"; Int.to_string n ]

open StateMonad

(* добавляем в списочек свободные переменные *)
let rec free_vars expr bound =
  match expr with
  | Me_EUnit | Me_ENill | Me_EConst _ -> []
  | Me_EIdentifier x -> if Set.mem bound x then [] else [x]
  | Me_EIf (e1, e2, e3) ->
    free_vars e1 bound @ free_vars e2 bound @ free_vars e3 bound
  | Me_EFun (args, body) ->
    let bound' = List.fold_left ~f:Set.add ~init:bound args in
    free_vars body bound'
  | Me_EApp (e1, e2) -> free_vars e1 bound @ free_vars e2 bound
  | Me_ELet (_, name, e1, e2) ->
    let fv1 = free_vars e1 bound in
    let fv2 = free_vars e2 (Set.add bound name) in
     fv1 @ fv2   
  | Me_ECons (e1, e2) -> free_vars e1 bound @ free_vars e2 bound
  | Me_ETuple lst -> List.fold_left lst ~init:[] ~f:(fun acc e ->  acc @ free_vars e bound)
;;




let rec ll_expr expr =
  match expr with
  | Me_EUnit | Me_ENill | Me_EConst _ | Me_EIdentifier _ -> return ([], expr)

  | Me_EIf (e1, e2, e3) ->
    let* (defs1, e1') = ll_expr e1 in
    let* (defs2, e2') = ll_expr e2 in
    let* (defs3, e3') = ll_expr e3 in
    return (defs1 @ defs2 @ defs3, Me_EIf (e1', e2', e3'))

  | Me_EApp (e1, e2) ->
    let* (defs1, e1') = ll_expr e1 in
    let* (defs2, e2') = ll_expr e2 in
    return (defs1 @ defs2, Me_EApp (e1', e2'))

  | Me_ECons (e1, e2) ->
    let* (defs1, e1') = ll_expr e1 in
    let* (defs2, e2') = ll_expr e2 in
    return (defs1 @ defs2, Me_ECons (e1', e2'))

  | Me_ETuple lst ->
    let* results =
      RList.fold_left lst ~init:(return ([], [])) ~f:(fun (acc_defs, acc_exprs) e ->
        let* (defs, e') = ll_expr e in
        return (acc_defs @ defs, acc_exprs @ [ e' ]))
    in
    let defs, exprs = results in
    return (defs, Me_ETuple exprs)

  | Me_ELet (flag, name, e1, e2) ->
    let* (defs1, e1') = ll_expr e1 in
    let* (defs2, e2') = ll_expr e2 in
    return (defs1 @ defs2, Me_ELet (flag, name, e1', e2'))

  | _ -> failwith "not impl"
;;

