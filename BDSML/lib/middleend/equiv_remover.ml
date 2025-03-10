(** Copyright 2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

module RenameBack = Map.Make (String)
open Anf_ast

let get_name env name =
  match RenameBack.find_opt name env with
  | Some n -> n
  | None -> name
;;

let rec aexpr_remover env = function
  | AExp_ident s -> AExp_ident (get_name env s)
  | AExp_tuple l -> AExp_tuple (List.map (aexpr_remover env) l)
  | AExp_construct (name, Some e) -> AExp_construct (name, Some (aexpr_remover env e))
  | _ as e -> e
;;

let rec cexpr_remover env = function
  | CExp_if (i, t, e) ->
    let i = aexpr_remover env i
    and t = lexpr_remover env t
    and e = lexpr_remover env e in
    CExp_if (i, t, e)
  | CExp_apply (name, args) ->
    let name = get_name env name
    and args = List.map (aexpr_remover env) args in
    CExp_apply (name, args)
  | CExp_atom a -> CExp_atom (aexpr_remover env a)

and lexpr_remover env = function
  | LLet_in (name, CExp_atom (AExp_ident name2), next) ->
    let env = RenameBack.add name name2 env in
    lexpr_remover env next
  | LLet_in (name, exp, next) ->
    let exp = cexpr_remover env exp in
    let next = lexpr_remover env next in
    LLet_in (name, exp, next)
  | LComplex exp -> LComplex (cexpr_remover env exp)
;;

let rec abs_remover env = function
  | h :: tl ->
    let helper = function
      | AbsStr_eval e -> AbsStr_eval (lexpr_remover env e) :: abs_remover env tl
      | AbsStr_func (name, args, exp) ->
        AbsStr_func (name, args, lexpr_remover env exp) :: abs_remover env tl
      | AbsStr_value (name, LComplex (CExp_atom (AExp_ident name2))) ->
        let env = RenameBack.add name name2 env in
        abs_remover env tl
      | AbsStr_value (name, exp) ->
        AbsStr_value (name, lexpr_remover env exp) :: abs_remover env tl
      | AbsStr_value_rec l ->
        let l = List.map (fun (name, args, exp) -> name, args, lexpr_remover env exp) l in
        AbsStr_value_rec l :: abs_remover env tl
    in
    helper h
  | _ -> []
;;

(** remove expressions [let a = b], where [a] and [b] just identifiers *)
let equiv_remove prog = abs_remover RenameBack.empty prog
