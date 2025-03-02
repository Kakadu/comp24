open Base
open Pat_elim_ast
open Common
open Common.MonadCounter
open StrSet

let rename env binds name =
  if String.equal name "()"
  then return (env, binds, "()")
  else if StrSet.find env name
  then
    let* fresh = fresh in
    let new_name = "a" ^ Int.to_string fresh in
    return (add env new_name, Map.update binds name ~f:(fun _ -> new_name), new_name)
  else return (add env name, binds, name)
;;

let rec ac_expr env bindings = function
  | PEEConst _ as orig -> return orig
  | PEEVar name as orig ->
    (match Map.find bindings name with
     | Some new_name -> return @@ PEEVar new_name
     | None -> return orig)
  | PEECons (l, r) ->
    let* l = ac_expr env bindings l in
    let* r = ac_expr env bindings r in
    return @@ PEECons (l, r)
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
      RList.fold_left
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
    let* e_list = RList.map el ~f:(ac_expr env bindings) in
    return @@ PEETuple e_list
  | PEELet (decl, e) ->
    let* new_env, new_bindings, new_decl = ac_decl env bindings decl in
    let* new_e = ac_expr new_env new_bindings e in
    return @@ PEELet (new_decl, new_e)

and ac_decl env bindings = function
  | PENonrec (name, e) ->
    let* new_env, new_bindings, new_name = rename env bindings name in
    let* new_e = ac_expr new_env bindings e in
    return (new_env, new_bindings, PENonrec (new_name, new_e))
  | PERec decl_list ->
    let names, exprs = List.unzip decl_list in
    let f1 acc cur_name =
      let* names, env, bindings = acc in
      let* new_env, new_bindings, new_name = rename env bindings cur_name in
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
    return (new_env, new_bindings, PERec new_decls)
;;

let ac_structure program env =
  let rec helper env binds = function
    | [] -> return []
    | hd :: tl ->
      let* env, binds, ast = ac_decl env binds hd in
      let* rest = helper env binds tl in
      return (ast :: rest)
  in
  helper env (Map.empty (module String)) program
;;

let run_ac nh init_num prog =
  run (ac_structure prog (StrSet.of_list Common.builtins)) nh init_num
;;
