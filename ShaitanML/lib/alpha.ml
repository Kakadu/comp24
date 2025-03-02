open Base
open Pat_elim_ast
open Common
open Common.MonadCounter

let get_name i = "a" ^ Int.to_string i

let process_name env bindings name =
  if String.equal name "()"
  then return (env, bindings, "()")
  else (
    match StrSet.find env name with
    | true ->
      let* fresh = fresh in
      let new_name = get_name fresh in
      return
        ( StrSet.add env new_name
        , Map.update bindings name ~f:(fun _ -> new_name)
        , new_name )
    | false -> return (StrSet.add env name, bindings, name))
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
          let* env, bindings, name = process_name env bindings name in
          return (name :: names, env, bindings))
    in
    let args = List.rev args in
    let* body = ac_expr env bindings body in
    return @@ PEEFun (args, body)
  | PEETuple e_list ->
    let* e_list = RList.map e_list ~f:(ac_expr env bindings) in
    return @@ PEETuple e_list
  | PEELet (decl, e) ->
    let* new_env, new_bindings, new_decl = ac_decl env bindings decl in
    let* new_e = ac_expr new_env new_bindings e in
    return @@ PEELet (new_decl, new_e)

and ac_decl env bindings = function
  | PENonrec (name, e) ->
    let* new_env, new_bindings, new_name = process_name env bindings name in
    let* new_e = ac_expr new_env bindings e in
    return (new_env, new_bindings, PENonrec (new_name, new_e))
  | PERec decl_list ->
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
    return (new_env, new_bindings, PERec new_decls)
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

let run nh init_num prog =
  run (ac_program prog (StrSet.of_list Common.builtins)) nh init_num
;;
