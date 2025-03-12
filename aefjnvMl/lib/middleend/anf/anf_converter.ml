(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Match_elimination.Me_ast
open Ll_conversion.Ll_ast
open Anf_ast

let convert_scoped = function
  | Local_id nm | Global_func nm | Global_var nm -> nm
;;

let convert_anf_id = function
  | Id_unit -> raise (Invalid_argument "Illigal case")
  | Id_name nm -> convert_scoped nm
;;

let rec convert_aexpr = function
  | A_let (id, bind, cont) ->
    let bind' = convert_cexpr bind in
    let cont' = convert_aexpr cont in
    LL_let (id, bind', cont')
  | A_cexpr cexpr -> convert_cexpr cexpr

and convert_cexpr = function
  | C_ifthenelse (i, t, e) ->
    LL_ifthenelse (convert_immexpr i, convert_aexpr t, convert_aexpr e)
  | C_tuple c'l -> LL_tuple (List.map convert_immexpr c'l)
  | C_rlist (c1, c'l) ->
    let tl = convert_immexpr c1 in
    List.fold_left
      (fun tl' c ->
        let h' = convert_immexpr c in
        LL_list (h', tl'))
      tl
      c'l
  | C_apply (scoped_nm, arg1, args) ->
    let f = LL_ident (convert_scoped scoped_nm) in
    List.fold_left
      (fun f' c ->
        let arg' = convert_immexpr c in
        LL_apply (f', arg'))
      f
      (arg1 :: args)
  | C_immexpr c -> convert_immexpr c

and convert_immexpr = function
  | Imm_int i -> LL_const (Const_int i)
  | Imm_bool b -> LL_const (Const_bool b)
  | Imm_nil -> LL_const Const_nil
  | Imm_unit -> LL_const Const_unit
  | Imm_id id -> LL_ident (convert_anf_id id)
;;

let convert_anf_fun f =
  { lldec_name = f.adec_name
  ; lldec_args = f.adec_args
  ; lldec_body = convert_aexpr f.adec_body
  }
;;

let convert_anf_decl = function
  | A_GlobalV (id, e) -> LL_GlobalV (id, convert_aexpr e)
  | A_NonrecDecl f -> LL_Decl (Nonrecursive, convert_anf_fun f, [])
  | A_RecDecl (f1, f'l) ->
    LL_Decl (Recursive, convert_anf_fun f1, List.map convert_anf_fun f'l)
;;

let convert_anf_prog_to_ll = List.map convert_anf_decl

let convert_anf_program prog =
  Ll_conversion.Ll_converter.convert_ll_program (convert_anf_prog_to_ll prog)
;;
