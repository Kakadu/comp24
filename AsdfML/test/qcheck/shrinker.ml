open QCheck
open QCheck.Gen
open Lib
open Ast

module Shrinker = struct
  open QCheck.Iter

  let rec shrink_pat = function
    | PCons (hd, tl) ->
      of_list [ hd; tl ]
      <+> (shrink_pat hd >|= fun hd -> p_cons hd tl)
      <+> (shrink_pat tl >|= fun tl -> p_cons hd tl)
    | PTuple xs | PList xs ->
      let* xs = Shrink.list ~shrink:shrink_pat xs in
      of_list xs
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
      let* e_ = shrink_expr e in
      of_list [ EFun (p, e_); e ]
    | ELetIn (d, e) ->
      let* d = shrink_def d in
      let* e_ = shrink_expr e in
      of_list [ ELetIn (d, e_); e ]
    | EList es | ETuple es ->
      of_list es
    | EMatch (e, cases) -> of_list (List.map (fun (_, e) -> e) cases) <+> of_list [ e ]
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
