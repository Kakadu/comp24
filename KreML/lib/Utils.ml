(** Copyright 2024-2025, KreML Compiler Commutnity *)

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
  let run m st = m st
end

module Counter = State (struct
    type t = int
  end)

let fresh_num : int Counter.t =
  let open Counter in
  get >>= fun curr -> put (curr + 1) >>= fun u -> return u
;;

let fresh_name base : string Counter.t =
  let open Counter in
  get >>= fun curr -> put (curr + 1) >>= fun u -> Format.sprintf "%s_%i" base u |> return
;;

(* Ast utils *)

let zip_idents_with_exprs p e =
  let rec helper acc p e =
    match p, e with
    | Pat_const _, _ -> internalfail "runtime check"
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

module FunArgsState = State (struct
    type t = (string, pattern list, Base.String.comparator_witness) Base.Map.t

    (* let lookup_exn : t -> string -> int = fun state id -> Base.Map.find_exn state id
    let empty : t = Base.Map.empty (module Base.String) *)
  end)

let count_args_with_state e =
  let open FunArgsState in
  let rec count_args e =
    let count_args e = count_args (return e) in
    let* e = e in
    match e with
    | Expr_const _ | Expr_tuple _ -> return []
    | Expr_constrained (e, _) -> count_args e
    | Expr_fun (p, e) ->
      let* acc = count_args e in
      return (p :: acc)
    | Expr_app (f, _) ->
      let* args = count_args f in
      return (List.rev args |> List.tl |> List.rev)
    | Expr_cons _ -> return []
    | Expr_ite (_, t, e) ->
      let* _ = count_args t in
      count_args e
    | Expr_match (_, cases) -> List.hd cases |> snd |> count_args
    | Expr_let (_, (p, e), scope) ->
      let zipped = zip_idents_with_exprs p e in
      let* _ =
        List.fold_left
          (fun acc (id, e) ->
            let* () = acc in
            let* state = get in
            let* c = count_args e in
            let state = Base.Map.set state ~key:id ~data:c in
            let* _ = put state in
            return ())
          (return ())
          zipped
      in
      count_args scope
    | Expr_var id ->
      let* state = get in
      Base.Map.find_exn state id |> return
  in
  let state = Base.Map.empty (module Base.String) in
  let count = count_args (return e) in
  run count state
;;

let count_args e = count_args_with_state e |> snd
