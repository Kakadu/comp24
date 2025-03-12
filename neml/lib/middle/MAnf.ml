[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

open LMisc
open LAst

open MCommon

(** ANF IR *)

type imm = Id of IdTagged.t | Const of Const.t
type cmplx = Imm of imm | Apply of imm * imm List1.t
type anf =
  | Let of IdTagged.t * cmplx * anf
  | Cmplx of cmplx
  | If of imm * anf * anf

type def = anf FuncDef.t
type t = def list * anf

let imm_to_expr : imm -> Expr.t = function
  | Id (id, _) ->
      Id id
  | Const const ->
      Const.to_expr const

let cmplx_to_expr : cmplx -> Expr.t = function
  | Imm imm ->
      imm_to_expr imm
  | Apply (ifun, iargs) ->
      List.fold_left (List1.to_list iargs) ~init:(imm_to_expr ifun)
        ~f:(fun acc iarg -> Apply (acc, imm_to_expr iarg) )

let rec to_expr : anf -> Expr.t = function
  | Let ((id, _), cmplx, anf) ->
      Let
        ( Nonrec
        , [Expr.{pat= Var id; expr= cmplx_to_expr cmplx}] |> List1.of_list_exn
        , to_expr anf )
  | Cmplx cmplx ->
      cmplx_to_expr cmplx
  | If (icond, ithen, ielse) ->
      If (imm_to_expr icond, to_expr ithen, Some (to_expr ielse))

let to_structure ((defs, cl) : t) : structure =
  List.fold_right defs
    ~init:[Eval (to_expr cl)]
    ~f:(fun def acc -> FuncDef.to_stritem to_expr def :: acc)

let from_cl (cl : MCLess.cl) : anf =
  (* ((Ef E0) E1) E2 -> Ef, [E2; E1; E0] *)
  let rec group_apps : MCLess.cl -> MCLess.cl * MCLess.cl list = function
    | Apply (c1, arg) ->
        let cl, args = group_apps c1 in
        (cl, arg :: args)
    | cl ->
        (cl, [])
  in

  let cnt = ref (-1) in
  let fresh () : IdTagged.t =
    cnt := !cnt + 1 ;
    (I ("v" ^ Int.to_string !cnt), Gen)
  in

  let ( let* ) = ( @@ ) in
  let rec f (cl : MCLess.cl) (k : imm -> anf) =
    match cl with
    | Id id ->
        k (Id id)
    | Const const ->
        k (Const const)
    | Apply (cfun, carg) ->
        let cfun, cargs = group_apps cfun in
        let* ifun = f cfun in

        let f cl acc iargs = f cl (fun iarg -> acc (iarg :: iargs)) in
        let init iargs : anf =
          let app = Apply (ifun, List1.of_list_exn iargs) in

          let fresh = fresh () in
          match k (Id fresh) with
          (* do not yield `Let`s of the kind `let x = E in x *)
          | Cmplx (Imm (Id id)) when IdTagged.equal fresh id ->
              Cmplx app
          | anf ->
              Let (fresh, app, anf)
        in

        List.fold_right (carg :: cargs) ~init ~f []
    | If (ccond, cthen, celse) ->
        let* icond = f ccond in
        let athen = f cthen k in
        let aelse = f celse k in
        If (icond, athen, aelse)
    | Seq cls ->
        List.fold_right (List2.to_list cls) ~init:k
          ~f:(fun x k _ -> f x k)
          (Const Unit)
  in
  f cl (fun imm -> Cmplx (Imm imm))

(** Converts CLess to ANF IR *)
let from_cless ((defs, cl) : MCLess.t) : t =
  let defs =
    List.fold_right defs ~init:[] ~f:(fun (Func def) acc : def list ->
        Func {def with body= from_cl def.body} :: acc )
  in
  (defs, from_cl cl)
