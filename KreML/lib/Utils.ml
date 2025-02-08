(** Copyright 2024-2025, CursedML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

let internalfail = failwith
let unreachable () = internalfail "Reached unreachable by assumption code"

module type Monad = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

module type STATE = sig
  include Monad

  type state

  val get : state t
  val put : state -> state t
  val run : 'a t -> state -> state * 'a
end

module State (S : sig
    type t
  end) : STATE with type state = S.t = struct
  type state = S.t
  type 'a t = state -> state * 'a

  let return v st = st, v

  let ( >>= ) m f st =
    let s1, v = m st in
    f v s1
  ;;

  let ( let* ) = ( >>= )
  let get s = s, s
  let put s v = s, v
  let run m = m
end

module Counter = State (struct
    type t = int
  end)

let fresh_num : int Counter.t =
  let open Counter in
  get >>= fun curr -> put (curr + 1) >>= return
;;

let fresh_name base : string Counter.t =
  let open Counter in
  get >>= fun curr -> put (curr + 1) >>= fun u -> Format.sprintf "%s_%i" base u |> return
;;

(* Ast utils *)

let zip_idents_with_exprs p e =
  let rec helper acc p e =
    match p, e with
    | Pat_const _, _ ->
      internalfail "it should be forbidden, this function is not used in match with"
    | Pat_constrained (p, _), _ -> helper acc p e
    | _, Expr_constrained (e, _) -> helper acc p e
    | Pat_var id, e -> (id, e) :: acc
    | Pat_cons (px, pxs), Expr_cons (ex, exs) -> helper (helper acc px ex) pxs exs
    | Pat_cons (px, pxs), e ->
      let head = eapp (evar "getfield") [ Expr_const (Const_int 0); e ] in
      let acc = helper acc px head in
      let tail = eapp (evar "getfield") [ Expr_const (Const_int 1); e ] in
      helper acc pxs tail
    | Pat_tuple (pfst, psnd, prest), Expr_tuple (efst, esnd, erest) ->
      List.fold_left2 helper acc (pfst :: psnd :: prest) (efst :: esnd :: erest)
    | Pat_tuple (pfst, psnd, prest), e ->
      let ps = pfst :: psnd :: prest in
      let es =
        Base.List.range 0 (List.length ps) ~stop:`exclusive
        |> List.map (fun i -> eapp (evar "getfield") [ Expr_const (Const_int i); e ])
      in
      List.fold_left2 helper acc ps es
    | p, e ->
      internalfail
      @@ Format.asprintf "unexpected p, e %a, %a" Ast.pp_pattern p Ast.pp_expr e
  in
  (* call is expected to be in type checked contxext *)
  helper [] p e |> List.rev
;;

(* List utils *)

let list_take count list =
  let rec helper count acc list =
    if count = 0
    then acc
    else (
      match list with
      | [] -> internalfail @@ Format.sprintf "List must have at least %i elements" count
      | x :: xs -> helper (count - 1) (x :: acc) xs)
  in
  helper count [] list |> List.rev
;;
