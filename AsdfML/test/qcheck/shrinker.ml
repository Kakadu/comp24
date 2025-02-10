open QCheck
open QCheck.Gen
open Lib
open Ast

module Shrinker = struct
  open QCheck.Iter

  let shrink_pat = function
    | PCons (hd, tl) -> of_list [ hd; tl ]
    | PTuple xs -> of_list xs
    | PList xs -> of_list xs
    | _ -> empty
  ;;

  let rec shrink_pats = function
    | _ :: _ as defs -> Shrink.list ~shrink:shrink_pat defs
    | [] -> empty
  ;;

  let rec shrink_expr = function
    | EApp (f, a) -> of_list [ f; a ]
    | EIfElse (c, t, e) -> of_list [ c; t; e ]
    | EFun (p, e) ->
      let* p = shrink_pats p in
      of_list [ e; EFun (p, e) ]
    | ELetIn (d, e) ->
      let* d = shrink_def d in
      of_list [ e; ELetIn (d, e) ]
    | EList es | ETuple es ->
      let* es = Shrink.list ~shrink:shrink_expr es in
      of_list es
    | EMatch (e, pes) -> of_list (List.map (fun (_p, e) -> e) pes) <+> of_list [ e ]
    | _ -> empty

  and shrink_def = function
    | DLet (f, p, e) ->
      let* p_ = shrink_pat p in
      let* e_ = shrink_expr e in
      of_list [ DLet (f, p_, e_); DLet (f, p_, e); DLet (f, p, e_) ]
  ;;

  let shrink_program = function
    | [] -> empty
    | decls -> Shrink.list ~shrink:shrink_def decls
  ;;
end

let arbitrary_ast = QCheck.make gen_program ~shrink:Shrinker.shrink_program
