open Base
open Pat_elim_ast
open Common
open Common.MonadCounter

let rec ac_expr env bindings = function
  | PEEConst _ as c -> return c
  | PEEVar x as v ->
    (match StrMap.find bindings x with
     | Some x -> return @@ PEEVar x
     | None -> return v)
  | PEECons (h, t) ->
    let* h = ac_expr env bindings h in
    let* t = ac_expr env bindings t in
    return @@ PEECons (h, t)
  | PEEApp (e1, e2) ->
    let* e1 = ac_expr env bindings e1 in
    let* e2 = ac_expr env bindings e2 in
    return @@ PEEApp (e1, e2)
  | PEEIf (e1, e2, e3) ->
    let* e1 = ac_expr env bindings e1 in
    let* e2 = ac_expr env bindings e2 in
    let* e3 = ac_expr env bindings e3 in
    return @@ PEEIf (e1, e2, e3)
  | PEEFun (args, body) ->
    let* args, env, bindings =
      fold_left
        args
        ~init:(return ([], env, bindings))
        ~f:(fun (names, env, bindings) name ->
          let* env, bindings, name = rename env bindings name in
          return (name :: names, env, bindings))
    in
    let args = List.rev args in
    let* body = ac_expr env bindings body in
    return @@ PEEFun (args, body)
  | PEETuple el ->
    let* e_list = map el ~f:(ac_expr env bindings) in
    return @@ PEETuple e_list
  | PEELet (c, e) ->
    let* env, new_bindings, case = ac_str_item env bindings c in
    let* new_e = ac_expr env new_bindings e in
    return @@ PEELet (case, new_e)

and ac_str_item env bindings = function
  | PENonrec (name, e) ->
    let* env, bindings, name = rename env bindings name in
    let* e = ac_expr env bindings e in
    return (env, bindings, PENonrec (name, e))
  | PERec cl ->
    let ids, exps = List.unzip cl in
    let f1 acc id =
      let* ids, env, bindings = acc in
      let* env, bindings, id = rename env bindings id in
      return (id :: ids, env, bindings)
    in
    let* ids, env, bindings = List.fold ids ~init:(return ([], env, bindings)) ~f:f1 in
    let ids = List.rev ids in
    let exps = List.map exps ~f:(fun e -> ac_expr env bindings e) in
    let f1 acc name expr =
      let* acc = acc in
      let* expr = expr in
      return ((name, expr) :: acc)
    in
    let* decls = List.fold2_exn ids exps ~init:(return []) ~f:f1 in
    return (env, bindings, PERec decls)

and rename env binds name =
  if String.equal name "()"
  then return (env, binds, "()")
  else if StrSet.find env name
  then
    let* fresh = fresh in
    let id = get_id fresh in
    return (StrSet.add env id, StrMap.update binds name ~f:(fun _ -> id), id)
  else return (StrSet.add env name, binds, name)
;;

let ac_structure program env =
  let rec helper env binds = function
    | [] -> return []
    | hd :: tl ->
      let* env, binds, ast = ac_str_item env binds hd in
      let* rest = helper env binds tl in
      return (ast :: rest)
  in
  helper env (Map.empty (module String)) program
;;

let run_ac bindings init prog =
  run (ac_structure prog (StrSet.of_list Common.builtins)) bindings init
;;
