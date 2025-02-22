(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base
open Remove_patterns

module R : sig
  type 'a t

  val return : 'a -> 'a t
  val fresh : int t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  include Base.Monad.Infix with type 'a t := 'a t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val run : 'a t -> int -> int * 'a

  module RList : sig
    val map : 'a list -> f:('a -> 'b t) -> 'b list t
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
  end
end = struct
  type 'a t = int -> int * 'a

  let return x var = var, x
  let fresh var = var + 1, var

  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    fun var ->
    let var, x = m var in
    f x var
  ;;

  let ( >>= ) = bind
  let ( let* ) = bind

  let ( >>| ) (m : 'a t) (f : 'a -> 'b) : 'b t =
    fun var ->
    let var, x = m var in
    var, f x
  ;;

  let run m start = m start

  module RList = struct
    let map (xs : 'a list) ~(f : 'a -> 'b t) : 'b list t =
      let* xs =
        List.fold xs ~init:(return []) ~f:(fun acc x ->
          let* acc = acc in
          let* x = f x in
          return (x :: acc))
      in
      return @@ List.rev xs
    ;;

    let fold_left (xs : 'a list) ~(init : 'b t) ~(f : 'b -> 'a -> 'b t) : 'b t =
      List.fold xs ~init ~f:(fun acc x ->
        let* acc = acc in
        f acc x)
    ;;
  end
end

open R
open Common

let get_name i = "#" ^ Int.to_string i

let process_name env bindings name =
  match StrSet.find env name with
  | true ->
    let* fresh = fresh in
    let new_name = get_name fresh in
    return
      (StrSet.add env new_name, Map.update bindings name ~f:(fun _ -> new_name), new_name)
  | false -> return (StrSet.add env name, bindings, name)
;;

let rec ac_expr env bindings = function
  | Rp_e_const _ as orig -> return orig
  | Rp_e_ident name as orig ->
    (match Map.find bindings name with
     | Some new_name -> return @@ Rp_e_ident new_name
     | None -> return orig)
  | Rp_e_cons_list (l, r) ->
    let* l = ac_expr env bindings l in
    let* r = ac_expr env bindings r in
    return @@ Rp_e_cons_list (l, r)
  | Rp_e_app (e1, e2) ->
    let* e1 = ac_expr env bindings e1 in
    let* e2 = ac_expr env bindings e2 in
    return @@ Rp_e_app (e1, e2)
  | Rp_e_ite (e1, e2, e3) ->
    let* e1 = ac_expr env bindings e1 in
    let* e2 = ac_expr env bindings e2 in
    let* e3 = ac_expr env bindings e3 in
    return @@ Rp_e_ite (e1, e2, e3)
  | Rp_e_fun (args, body) ->
    let* args, env, bindings =
      RList.fold_left
        args
        ~init:(return ([], env, bindings))
        ~f:(fun (names, env, bindings) name ->
          let* env, bindings, name = process_name env bindings name in
          return (name :: names, env, bindings))
    in
    let args = List.rev args in
    let* body = ac_expr env bindings body in
    return @@ Rp_e_fun (args, body)
  | Rp_e_tuple e_list ->
    let* e_list = RList.map e_list ~f:(ac_expr env bindings) in
    return @@ Rp_e_tuple e_list
  | Rp_e_let (decl, e) ->
    let* new_env, new_bindings, new_decl = ac_decl env bindings decl in
    let* new_e = ac_expr new_env new_bindings e in
    return @@ Rp_e_let (new_decl, new_e)

and ac_decl env bindings = function
  | Rp_non_rec (name, e) ->
    let* new_env, new_bindings, new_name = process_name env bindings name in
    let* new_e = ac_expr new_env bindings e in
    return (new_env, new_bindings, Rp_non_rec (new_name, new_e))
  | Rp_rec decl_list ->
    let names, exprs = List.unzip decl_list in
    let f1 acc cur_name =
      let* names, env, bindings = acc in
      let* new_env, new_bindings, new_name = process_name env bindings cur_name in
      return (new_name :: names, new_env, new_bindings)
    in
    let* new_names, new_env, new_bindings =
      List.fold names ~init:(return ([], env, bindings)) ~f:f1
    in
    let new_names = List.rev new_names in
    let new_exprs = List.map exprs ~f:(fun e -> ac_expr new_env new_bindings e) in
    let f1 acc name expr =
      let* acc = acc in
      let* expr = expr in
      return ((name, expr) :: acc)
    in
    let* new_decls = List.fold2_exn new_names new_exprs ~init:(return []) ~f:f1 in
    return (new_env, new_bindings, Rp_rec new_decls)
;;

let ac_program program init_env =
  let rec helper last_env last_bindings = function
    | [] -> return []
    | hd :: tl ->
      let* cur_env, cur_bindings, cur_ast = ac_decl last_env last_bindings hd in
      let* other_asts = helper cur_env cur_bindings tl in
      return (cur_ast :: other_asts)
  in
  helper init_env (Map.empty (module String)) program
;;

let run_alpha_conversion_program prog =
  run (ac_program prog (StrSet.of_list Std_names.std_names)) 0
;;
