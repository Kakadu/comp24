(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

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
    | PTuple (hd1, hd2, tl) ->
      let xs = hd1 :: hd2 :: tl in
      Shrink.list ~shrink:shrink_pat xs >>= of_list
    | PList xs -> Shrink.list ~shrink:shrink_pat xs >>= of_list
    | PAnn (p, _) -> return p
    | _ -> empty
  ;;

  let rec shrink_pats = function
    | _ :: _ as pats -> Shrink.list ~shrink:shrink_pat pats
    | [] -> empty
  ;;

  let rec shrink_expr = function
    | EApp (f, a) -> of_list [ f; a ]
    | EIfElse (c, t, e) -> of_list [ c; t; e ]
    | EFun (p, ps, e) ->
      let* p = shrink_pat p in
      let* ps = shrink_pats ps in
      let* e_ = shrink_expr e in
      of_list [ EFun (p, ps, e_); e ]
    | ELetIn (d, e) ->
      let* d = shrink_def d in
      let* e_ = shrink_expr e in
      of_list [ ELetIn (d, e_); e ]
    | EList es -> Shrink.list ~shrink:shrink_expr es >>= of_list
    | ETuple (hd1, hd2, tl) ->
      let es = hd1 :: hd2 :: tl in
      Shrink.list ~shrink:shrink_expr es >>= of_list
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

let arbitrary_ast =
  QCheck.make
    ~print:(fun ast -> Format.asprintf "%a" Pp_ast.pp_program ast)
    gen_program
    ~shrink:Shrinker.shrink_program
;;

let parser_qtests =
  [ QCheck.Test.make ~count:100 arbitrary_ast (fun original ->
      let src = Format.asprintf "%a" Pp_ast.pp_program original in
      let res = Parser.parse_program src in
      match res with
      | Result.Ok parsed when parsed = original -> true
      | Result.Ok parsed ->
        Format.printf "\n\nDifferent AST!\n";
        let print pp o p =
          Format.printf "Original AST \n%a \n\nParsed AST \n%a\n\n" pp o pp p
        in
        print Pp_ast.pp_program original parsed;
        print Ast.pp_program original parsed;
        false
      | Result.Error s ->
        Format.printf "\n\nParser error: %s\n" s;
        false)
  ]
;;

let seed = if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 42;;

QCheck_runner.set_seed seed;;

QCheck_runner.run_tests
  (* ~verbose:true  *)
  (* ~debug_shrink:(Some stdout)  *)
  parser_qtests
