open Pat_elim_ast
open Common.MonadCounter
open Base
open Common

let rec ll_expr env = function
  | PEEConst _ as v -> return ([], v)
  | PEEVar id as v ->
    (match Map.find env id with
     | Some x -> return ([], PEEVar x)
     | None -> return ([], v))
  | PEEApp (e1, e2) ->
    let* str1, e1 = ll_expr env e1 in
    let* sl2, e2 = ll_expr env e2 in
    return (str1 @ sl2, PEEApp (e1, e2))
  | PEEIf (e1, e2, e3) ->
    let* str1, e1 = ll_expr env e1 in
    let* str2, e2 = ll_expr env e2 in
    let* str3, e3 = ll_expr env e3 in
    return (str1 @ str2 @ str3, PEEIf (e1, e2, e3))
  | PEEFun (args, body) ->
    let* fresh = fresh >>| get_id in
    let env = List.fold args ~init:env ~f:Map.remove in
    let* str, body = ll_expr env body in
    return (str @ [ PENonrec (fresh, PEEFun (args, body)) ], PEEVar fresh)
  | PEECons (e1, e2) ->
    let* str1, e1 = ll_expr env e1 in
    let* str2, e2 = ll_expr env e2 in
    return (str1 @ str2, PEECons (e1, e2))
  | PEETuple e_list ->
    let* t = map e_list ~f:(ll_expr env) in
    let str, el = List.unzip t in
    return (List.concat str, PEETuple el)
  | PEELet (PENonrec (name, e1), e2) ->
    let* str1, e1 = ll_inner env e1 in
    (match e1 with
     | PEEFun _ ->
       let* fresh_name = fresh >>| get_id in
       let bindings = Map.update env name ~f:(fun _ -> fresh_name) in
       let* str2, e2 = ll_expr bindings e2 in
       return (str1 @ [ PENonrec (fresh_name, e1) ] @ str2, e2)
     | _ ->
       let* str2, e2 = ll_expr env e2 in
       return (str1 @ str2, PEELet (PENonrec (name, e1), e2)))
  | PEELet (PERec cl, e) ->
    let* env =
      List.fold cl ~init:(return env) ~f:(fun acc (name, _) ->
        let* acc = acc in
        let* fresh = fresh >>| get_id in
        return @@ Map.update acc name ~f:(fun _ -> fresh))
    in
    let* str1, cl =
      List.fold
        cl
        ~init:(return ([], []))
        ~f:(fun acc (name, e) ->
          let* decls, decl_bodies = acc in
          let* d, e = ll_inner env e in
          let new_name = Map.find_exn env name in
          return (decls @ d, (new_name, e) :: decl_bodies))
    in
    let cl = List.rev cl in
    let* str2, e2 = ll_expr env e in
    return (str1 @ [ PERec cl ] @ str2, e2)

and ll_inner env = function
  | PEEFun (args, body) ->
    let env = List.fold args ~init:env ~f:Map.remove in
    let* str, body = ll_expr env body in
    return (str, PEEFun (args, body))
  | e ->
    let* str, e = ll_expr env e in
    return (str, e)
;;

let ll_str_item = function
  | PENonrec (name, e) ->
    let* str, new_e = ll_inner empty e in
    return @@ str @ [ PENonrec (name, new_e) ]
  | PERec decls ->
    let* str, cl =
      List.fold
        decls
        ~init:(return ([], []))
        ~f:(fun acc (name, e) ->
          let* d1, d2 = acc in
          let* d, e = ll_inner empty e in
          return (d @ d1, (name, e) :: d2))
    in
    let rec_decl = [ PERec (List.rev cl) ] in
    return @@ str @ rec_decl
;;

let ll_structure structure =
  let rec helper = function
    | [] -> return []
    | hd :: tl ->
      let* str1 = ll_str_item hd in
      let* str2 = helper tl in
      return @@ str1 @ str2
  in
  helper structure
;;

let run nh init_num p = run (ll_structure p) nh init_num
