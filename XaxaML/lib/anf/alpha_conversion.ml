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
  val run : 'a t -> 'a

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

  let run m = snd (m 0)

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

let get_name i = "#" ^ Int.to_string i
let empty_env = Map.empty (module String)

let process_name env name =
  match Map.find env name with
  | Some _ ->
    let* fresh = fresh in
    let new_name = get_name fresh in
    return (new_name, Map.update env name ~f:(fun _ -> new_name))
  | None -> return (name, Map.update env name ~f:(fun _ -> name))
;;

let rec ac_expr env = function
  | Rp_e_const _ as orig -> return orig
  | Rp_e_ident name as orig ->
    (match Map.find env name with
     | Some new_name -> return @@ Rp_e_ident new_name
     | None -> return orig)
  | Rp_e_cons_list (l, r) ->
    let* l = ac_expr env l in
    let* r = ac_expr env r in
    return @@ Rp_e_cons_list (l, r)
  | Rp_e_app (e1, e2) ->
    let* e1 = ac_expr env e1 in
    let* e2 = ac_expr env e2 in
    return @@ Rp_e_app (e1, e2)
  | Rp_e_ite (e1, e2, e3) ->
    let* e1 = ac_expr env e1 in
    let* e2 = ac_expr env e2 in
    let* e3 = ac_expr env e3 in
    return @@ Rp_e_ite (e1, e2, e3)
  | Rp_e_fun (args, body) ->
    let* args, env =
      RList.fold_left
        args
        ~init:(return ([], env))
        ~f:(fun (names, env) name ->
          let* name, env = process_name env name in
          return (name :: names, env))
    in
    let args = List.rev args in
    let* body = ac_expr env body in
    return @@ Rp_e_fun (args, body)
  | Rp_e_tuple e_list ->
    let* e_list = RList.map e_list ~f:(ac_expr env) in
    return @@ Rp_e_tuple e_list
  | Rp_e_let (decl, e) ->
    let* new_env, new_decl = ac_decl env decl in
    let* new_e = ac_expr new_env e in
    return @@ Rp_e_let (new_decl, new_e)

and ac_decl env = function
  | Rp_non_rec (name, e) ->
    let* new_name, new_env = process_name env name in
    let* new_e = ac_expr env e in
    return (new_env, Rp_non_rec (new_name, new_e))
  | Rp_rec decl_list ->
    let names, exprs = List.unzip decl_list in
    let f1 acc cur_name =
      let* names, env = acc in
      let* new_name, new_env = process_name env cur_name in
      return (new_name :: names, new_env)
    in
    let* new_names, new_env = List.fold names ~init:(return ([], env)) ~f:f1 in
    let new_names = List.rev new_names in
    let new_exprs = List.map exprs ~f:(fun e -> ac_expr new_env e) in
    let f1 acc name expr =
      let* acc = acc in
      let* expr = expr in
      return ((name, expr) :: acc)
    in
    let* new_decls = List.fold2_exn new_names new_exprs ~init:(return []) ~f:f1 in
    return (new_env, Rp_rec new_decls)
;;

let ac_toplevel env = function
  | Rp_non_rec (name, e) ->
    let* new_name, new_env = process_name env name in
    let* new_e = ac_expr env e in
    return (new_env, Rp_non_rec (new_name, new_e))
  | Rp_rec decl_list ->
    let names, exprs = List.unzip decl_list in
    let f1 acc cur_name =
      let* names, env = acc in
      let* new_name, new_env = process_name env cur_name in
      return (new_name :: names, new_env)
    in
    let* new_names, new_env = List.fold names ~init:(return ([], env)) ~f:f1 in
    let new_names = List.rev new_names in
    let new_exprs = List.map exprs ~f:(fun e -> ac_expr new_env e) in
    let f1 acc name expr =
      let* acc = acc in
      let* expr = expr in
      return ((name, expr) :: acc)
    in
    let* new_decls = List.fold2_exn new_names new_exprs ~init:(return []) ~f:f1 in
    return (new_env, Rp_rec new_decls)
;;

let ac_program program =
  let rec helper last_env = function
    | [] -> return []
    | hd :: tl ->
      let* cur_env, cur_ast = ac_toplevel last_env hd in
      let* other_asts = helper cur_env tl in
      return (cur_ast :: other_asts)
  in
  helper empty_env program
;;

let run_alpha_conversion_program prog = run (ac_program prog)
