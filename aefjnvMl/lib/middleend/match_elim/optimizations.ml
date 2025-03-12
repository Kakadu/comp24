(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(*
   PLANS:
   - not true ---> false
   - not false ---> true
*)

open Me_ast
open Common.Ast

let bin_app o_ l_ r_ = MExp_apply (MExp_apply (o_, l_), r_)
let bool_const v = MExp_constant (Const_bool v)

let eval_bconst = function
  | MExp_constant (Const_bool x) -> Some x
  | _ -> None
;;

let is_bool_op =
  let open Common.Base_lib.LibF in
  function
  | name when name = get_name op_and -> true
  | name when name = get_name op_or -> true
  | _ -> false
;;

let optimize_bool_op op l r =
  let open Common.Base_lib.LibF in
  let open Option in
  let l' = eval_bconst l in
  let r' = eval_bconst r in
  match l', r' with
  (* (&&) optimizations *)
  | Some false, _ when op = get_name op_and -> some @@ bool_const false
  | Some true, _ when op = get_name op_and -> some r
  (* (||) optimizations *)
  | Some true, _ when op = get_name op_and -> some @@ bool_const true
  | _ -> None
;;

let optimize_vb optimize_me { m_vb_pat; m_vb_expr } =
  { m_vb_pat; m_vb_expr = optimize_me m_vb_expr }
;;

let optimize_decl' optimize_me (MDecl (rflag, vb'list)) =
  let vb_l' = List.map (optimize_vb optimize_me) vb'list in
  MDecl (rflag, vb_l')
;;

let rec optimize_me = function
  | MExp_ifthenelse (cond, then_br, else_br) ->
    let cond' = optimize_me cond in
    let then_br' = optimize_me then_br in
    let else_br' = optimize_me else_br in
    (match eval_bconst cond' with
     | None -> MExp_ifthenelse (cond', then_br', else_br')
     (* [if true then A else B] |---> [A] *)
     | Some true -> then_br'
     (* [if false then A else B] |---> [B] *)
     | Some false -> else_br')
  | MExp_apply (MExp_apply (o, l), r) ->
    let o' = optimize_me o in
    let l' = optimize_me l in
    let r' = optimize_me r in
    let unpack = function
      | Some x -> x
      | None -> bin_app o' l' r'
    in
    (match o' with
     | MExp_ident name when is_bool_op name -> optimize_bool_op name l' r' |> unpack
     | _ -> bin_app o' l' r')
  | MExp_constant c -> MExp_constant c
  | MExp_ident i -> MExp_ident i
  | MExp_tuple me'list -> MExp_tuple (List.map optimize_me me'list)
  | MExp_list (me1, me2) -> MExp_list (optimize_me me1, optimize_me me2)
  | MExp_apply (me1, me2) -> MExp_apply (optimize_me me1, optimize_me me2)
  | MExp_function (var, me) -> MExp_function (var, optimize_me me)
  | MExp_let (decl, me) ->
    let me' = optimize_me me in
    MExp_let (optimize_decl' optimize_me decl, me')
;;

let optimize_decl = optimize_decl' optimize_me
let optimize prog = List.map optimize_decl prog
