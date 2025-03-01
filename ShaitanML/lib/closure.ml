open Pat_elim_ast
open Common

module BoundVars = struct
  let empty = Base.Map.empty (module Base.String)
  let merge old_map new_map = Base.Map.merge_skewed old_map new_map ~combine:(fun ~key:_ _ v -> v)
end

let rec identify_free_vars bound_set = function
  | PEEConst _ -> StrSet.empty
  | PEEIdent id -> if StrSet.find bound_set id then StrSet.empty else StrSet.singleton id 
  | PEEEif (c, t, e) ->
    List.fold_left StrSet.union StrSet.empty [
      identify_free_vars bound_set c;
      identify_free_vars bound_set t;
      identify_free_vars bound_set e
    ]
  | PEEFun (params, body) ->
    let bound = StrSet.union bound_set (StrSet.of_list params) in
    identify_free_vars bound body
  | PEEApp (f, arg) ->
    StrSet.union 
      (identify_free_vars bound_set f)
      (identify_free_vars bound_set arg)
  | PEELet (decl, next) -> handle_let_vars bound_set decl next
  | PEECons (hd, tl) ->
    StrSet.union
      (identify_free_vars bound_set hd)
      (identify_free_vars bound_set tl)
  | PEETuple exprs ->
    List.fold_left (fun acc e -> 
      StrSet.union acc (identify_free_vars bound_set e)
    ) StrSet.empty exprs

and handle_let_vars bound_set decl next = match decl with
  | PENonrec (name, expr) ->
    StrSet.union
      (identify_free_vars bound_set expr)
      (identify_free_vars (StrSet.add bound_set name) next)
  | PERec bindings ->
    let names = List.map fst bindings in
    let bound = StrSet.union bound_set (StrSet.of_list names) in
    let body_vars = List.fold_left (fun acc (_, e) ->
      StrSet.union acc (identify_free_vars bound e)
    ) StrSet.empty bindings in
    StrSet.union body_vars (identify_free_vars bound next)

let build_function args body free_vars bindings =
  let fn = PEEFun (free_vars @ args, body) in
  List.fold_left (fun acc name ->
    let arg = try Base.Map.find_exn bindings name with _ -> PEEIdent name in 
    PEEApp (acc, arg)
  ) fn free_vars

let rec convert_expr env bindings = function
  | PEEConst _ as e -> e
  | PEEIdent id as e -> (
    match Base.Map.find bindings id with
    | Some expr -> expr
    | None -> e)
  | PEEEif (c, t, e) ->
    PEEEif (
      convert_expr env bindings c,
      convert_expr env bindings t, 
      convert_expr env bindings e)
  | PEEFun (args, body) as expr ->
    let free = identify_free_vars env expr |> StrSet.to_list in
    let converted = convert_expr env BoundVars.empty body in
    build_function args converted free bindings
  | PEEApp (f, arg) -> 
    PEEApp (
      convert_expr env bindings f,
      convert_expr env bindings arg)
  | PEELet (decl, next) -> handle_let_conversion env bindings decl next  
  | PEECons (hd, tl) ->
    PEECons (
      convert_expr env bindings hd,
      convert_expr env bindings tl)
  | PEETuple exprs ->
    PEETuple (List.map (convert_expr env bindings) exprs)

and handle_let_conversion env bindings decl next = match decl with
  | PENonrec (name, expr) ->
    let converted_expr, new_bindings = match expr with
      | PEEFun (params, body) ->
        let free = 
          StrSet.diff 
            (identify_free_vars env body) 
            (StrSet.of_list params) 
          |> StrSet.to_list in
        let converted = convert_expr env BoundVars.empty body in
        let fn = PEEFun (free @ params, converted) in
        let binding = build_function [] (PEEIdent name) free bindings in
        fn, Base.Map.singleton (module Base.String) name binding
      | _ -> 
        convert_expr env bindings expr, BoundVars.empty
    in
    let merged = BoundVars.merge bindings new_bindings in
    PEELet (PENonrec (name, converted_expr),
            convert_expr env merged next)
  | PERec decls ->
    let converted, new_bindings = convert_rec_decls env bindings decls in
    PEELet (PERec converted,
            convert_expr env (BoundVars.merge bindings new_bindings) next)

and convert_rec_decls env bindings decls =
  let names = List.map fst decls in
  let process_decl acc (name, expr) = match expr with
    | PEEFun (params, body) ->
      let exclude = StrSet.union (StrSet.of_list names) (StrSet.of_list params) in
      let free = 
        StrSet.diff (identify_free_vars env body) exclude 
        |> StrSet.to_list in
      let binding = build_function [] (PEEIdent name) free bindings in
      (free :: fst acc, Base.Map.set (snd acc) ~key:name ~data:binding)
    | _ -> ([] :: fst acc, snd acc)
  in
  let free_vars, bindings = 
    List.fold_left process_decl ([], bindings) decls in
  let free_vars = List.rev free_vars in
  let converted = List.map2 (fun (name, expr) free ->
    match expr with
    | PEEFun (params, body) ->
      let converted = convert_expr env BoundVars.empty body in
      name, PEEFun (free @ params, converted)
    | _ -> 
      name, convert_expr env bindings expr
  ) decls free_vars in
  List.rev converted, bindings

let builtin_ops = [
  "( + )"; "( - )"; "( / )"; "( * )";
  "( < )"; "( > )"; "( <= )"; "( >= )";
  "( <> )"; "( = )"; "print_int"
]

let initial_env = List.fold_left (fun acc op -> StrSet.add acc op) StrSet.empty builtin_ops

let convert_top_level env = function
  | PENonrec (name, expr) ->
    let converted, _ = match expr with
      | PEEFun _ -> convert_expr env BoundVars.empty expr, []
      | _ -> convert_expr env BoundVars.empty expr, []
    in
    StrSet.add env name, PENonrec (name, converted)
  | PERec decls ->
    let names = List.map fst decls in
    let converted, _ = convert_rec_decls env BoundVars.empty decls in
    let new_env = List.fold_left (fun acc name -> StrSet.add acc name) env names in
    new_env, PERec converted

let closure_convert prog =
  let rec process env = function
    | [] -> []
    | decl :: rest ->
      let new_env, converted = convert_top_level env decl in
      converted :: process new_env rest
  in
  process initial_env prog