(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Remove_patterns
open Common.MonadCounter
open Base

let empty_bindings = Map.empty (module String)
let get_name i = "#" ^ Int.to_string i

let rec ll_expr bindings = function
  | Rp_e_const _ as orig -> return ([], orig)
  | Rp_e_ident name as orig ->
    (match Map.find bindings name with
     | Some new_name -> return ([], Rp_e_ident new_name)
     | None -> return ([], orig))
  | Rp_e_app (e1, e2) ->
    let* decls1, e1 = ll_expr bindings e1 in
    let* decls2, e2 = ll_expr bindings e2 in
    return (decls1 @ decls2, Rp_e_app (e1, e2))
  | Rp_e_ite (e1, e2, e3) ->
    let* decls1, e1 = ll_expr bindings e1 in
    let* decls2, e2 = ll_expr bindings e2 in
    let* decls3, e3 = ll_expr bindings e3 in
    return (decls1 @ decls2 @ decls3, Rp_e_ite (e1, e2, e3))
  (* only for anonymous functions *)
  | Rp_e_fun (args, body) ->
    let* fresh_name = fresh >>| get_name in
    let* decls, new_body = ll_expr bindings body in
    return
      ( decls @ [ Rp_non_rec (fresh_name, Rp_e_fun (args, new_body)) ]
      , Rp_e_ident fresh_name )
  | Rp_e_cons_list (e1, e2) ->
    let* decls1, e1 = ll_expr bindings e1 in
    let* decls2, e2 = ll_expr bindings e2 in
    return (decls1 @ decls2, Rp_e_cons_list (e1, e2))
  | Rp_e_tuple e_list ->
    let* t = RList.map e_list ~f:(ll_expr bindings) in
    let decls, e_list = List.unzip t in
    return (List.concat decls, Rp_e_tuple e_list)
  | Rp_e_let (Rp_non_rec (name, e1), e2) ->
    let* decls1, e1 = ll_decl_body bindings e1 in
    (match e1 with
     | Rp_e_fun _ ->
       let* fresh_name = fresh >>| get_name in
       let bindings = Map.update bindings name ~f:(fun _ -> fresh_name) in
       let* decls2, e2 = ll_expr bindings e2 in
       return (decls1 @ [ Rp_non_rec (fresh_name, e1) ] @ decls2, e2)
     | _ ->
       let* decls2, e2 = ll_expr bindings e2 in
       return (decls1 @ decls2, Rp_e_let (Rp_non_rec (name, e1), e2)))
  | Rp_e_let (Rp_rec decls, e2) ->
    let* bindings =
      List.fold decls ~init:(return bindings) ~f:(fun acc (name, _) ->
        let* acc = acc in
        let* fresh_name = fresh >>| get_name in
        return @@ Map.update acc name ~f:(fun _ -> fresh_name))
    in
    let* decls1, decl_bodies =
      List.fold
        decls
        ~init:(return ([], []))
        ~f:(fun acc (name, e) ->
          let* decls, decl_bodies = acc in
          let* d, e = ll_decl_body bindings e in
          let new_name = Map.find_exn bindings name in
          return (decls @ d, (new_name, e) :: decl_bodies))
    in
    let decl_bodies = List.rev decl_bodies in
    let* decls2, e2 = ll_expr bindings e2 in
    return (decls1 @ [ Rp_rec decl_bodies ] @ decls2, e2)

and ll_decl_body bindings = function
  | Rp_e_fun (args, body) ->
    let* decls, new_body = ll_expr bindings body in
    return (decls, Rp_e_fun (args, new_body))
  | e ->
    let* decls, e = ll_expr bindings e in
    return (decls, e)
;;

let ll_toplevel = function
  | Rp_non_rec (name, e) ->
    let* decls, new_e = ll_decl_body empty_bindings e in
    return @@ decls @ [ Rp_non_rec (name, new_e) ]
  | Rp_rec decls ->
    let* new_decls, bodies =
      List.fold
        decls
        ~init:(return ([], []))
        ~f:(fun acc (name, e) ->
          let* d1, d2 = acc in
          let* d, e = ll_decl_body empty_bindings e in
          return (d @ d1, (name, e) :: d2))
    in
    let rec_decl = [ Rp_rec (List.rev bodies) ] in
    return @@ new_decls @ rec_decl
;;

let ll_program program =
  let rec helper = function
    | [] -> return []
    | hd :: tl ->
      let* decls1 = ll_toplevel hd in
      let* decls2 = helper tl in
      return @@ decls1 @ decls2
  in
  helper program
;;

let run_lambda_lifting_program init_num p = run (ll_program p) init_num
