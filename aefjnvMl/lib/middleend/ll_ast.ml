(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** 
TODO (mutual rec): 
  - mutaual recursion can be only with [rec] (invariant from parser) 
     --> [and] can be only with function (at least one function would be) 
     --> mutaual recursion can be only in ll_decl as global funcs (ll-guarantees)
  p.s. without rec [let a = 2 and b = 10] == [let a = 2;; let b = 10;;] (mb support it)
  
     [
  let a c = 
    let rec a' c' = b + 10
    and b = 11 in
    (a' 10) + c
  ] ==> [
  let rec a' c' = b + 10
    and b = 11;;

  let a c = (a' 10) + c  
  ]

TODO (let in):
  - can't be function -- only  pattern * ll_expr
  [
  let f e = e;;
  
  let a =
    let b = 10 in
    let c = f 1 in
    b + c
  ;;
  ]
*)

open Common.Ast

type ll_expr =
  | LL_Id of ident
  | LL_Const of const
  | LL_App of ll_expr * ll_expr
  | LL_List of ll_expr * ll_expr
  | LL_LetIn of ll_bind * ll_expr
  | LL_If of ll_expr * ll_expr * ll_expr
  | LL_Tuple of ll_expr list

and ll_bind = pattern * ll_expr

type ll_vb = pattern * pattern list * ll_expr
type ll_decl = ll_vb list

(* type me_expr =
   | ME_Id of me_ident
   | ME_Const of Common.Ast.const
   | ME_App of me_expr * me_expr
   | ME_List of me_expr * me_expr
   | ME_LetIn of ll_bind * me_expr
   | ME_If of me_expr * me_expr * me_expr
   | ME_Tuple of me_expr list

   and ll_bind = me_ident * me_expr

   type ll_vb = me_ident * me_ident list * me_expr

   type ll_decl = ll_vb list *)

(* type ident = string [@@deriving show { with_path = false }] *)
