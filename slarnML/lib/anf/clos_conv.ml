(** Copyright 2024-2025, Ivan Shurenkov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(* open Ast *)
open Cc_ast
open Res

let default_fun = List.map (fun (id, _) -> id, id, 0, []) Call_define.default_func

let remove_args id args prt_args =
  if id = "()" then [] else List.filter (fun x -> not (List.mem x args)) prt_args
;;

let get_new_name id cnt = if id = "()" then id else id ^ "_" ^ string_of_int cnt

let rec closure_conversion ?(env = []) ?(prt_args = []) = function
  | Ast.Id id ->
    (match List.find_opt (fun (name, _, _, _) -> name = id) env with
     | None ->
       (* print_string ("Id " ^ id ^ " not found in env\n"); *)
       CId id
     | Some (_, new_name, _, args) ->
       if List.length args > 0
       then CApp (CId new_name, List.map (fun arg -> CId arg) args)
       else CId new_name)
  | Ast.Const const -> CConst const
  | Ast.Not e -> CNot (closure_conversion ~env ~prt_args e)
  | Ast.Or (e1, e2) ->
    COr (closure_conversion ~env ~prt_args e1, closure_conversion ~env ~prt_args e2)
  | Ast.And (e1, e2) ->
    CAnd (closure_conversion ~env ~prt_args e1, closure_conversion ~env ~prt_args e2)
  | Ast.Eq (e1, e2) ->
    CEq (closure_conversion ~env ~prt_args e1, closure_conversion ~env ~prt_args e2)
  | Ast.Gt (e1, e2) ->
    CGt (closure_conversion ~env ~prt_args e1, closure_conversion ~env ~prt_args e2)
  | Ast.Lt (e1, e2) ->
    CLt (closure_conversion ~env ~prt_args e1, closure_conversion ~env ~prt_args e2)
  | Ast.Gte (e1, e2) ->
    CGte (closure_conversion ~env ~prt_args e1, closure_conversion ~env ~prt_args e2)
  | Ast.Lte (e1, e2) ->
    CLte (closure_conversion ~env ~prt_args e1, closure_conversion ~env ~prt_args e2)
  | Ast.Add (e1, e2) ->
    CAdd (closure_conversion ~env ~prt_args e1, closure_conversion ~env ~prt_args e2)
  | Ast.Sub (e1, e2) ->
    CSub (closure_conversion ~env ~prt_args e1, closure_conversion ~env ~prt_args e2)
  | Ast.Mul (e1, e2) ->
    CMul (closure_conversion ~env ~prt_args e1, closure_conversion ~env ~prt_args e2)
  | Ast.Div (e1, e2) ->
    CDiv (closure_conversion ~env ~prt_args e1, closure_conversion ~env ~prt_args e2)
  | Ast.If (cond, then_expr, else_expr) ->
    CIf
      ( closure_conversion ~env ~prt_args cond
      , closure_conversion ~env ~prt_args then_expr
      , closure_conversion ~env ~prt_args else_expr )
  | Ast.Let (decl, body) ->
    let id, args, declared, pre_env =
      match decl with
      | Ast.Decl (id, args) ->
        ( id
        , args
        , (fun id args -> Ast.Decl (id, remove_args id args prt_args @ args))
        , fun _ _ -> [] )
      | Ast.DeclRec (id, args) ->
        ( id
        , args
        , (fun id args -> Ast.DeclRec (id, remove_args id args prt_args @ args))
        , fun new_name cnt -> [ id, new_name, cnt + 1, remove_args id args prt_args ] )
    in
    let env_args = List.map (fun arg -> arg, arg, 0, []) args @ env in
    (match List.find_opt (fun (name, _, _, _) -> name = id) env with
     | None ->
       CLet
         ( declared id args
         , closure_conversion
             ~env:(pre_env id 0 @ env_args)
             ~prt_args:(args @ prt_args)
             body )
     | Some (_, old_name, cnt, _) ->
       let new_name = get_new_name old_name cnt in
       let body_converted =
         closure_conversion
           ~env:(pre_env new_name cnt @ env_args)
           ~prt_args:(args @ prt_args)
           body
       in
       CLet (declared new_name args, body_converted))
  | Ast.LetIn (decl, expr1, expr2) ->
    let id, args, declared, pre_env =
      match decl with
      | Ast.Decl (id, args) ->
        ( id
        , args
        , (fun id args -> Ast.Decl (id, remove_args id args prt_args @ args))
        , fun _ _ -> [] )
      | Ast.DeclRec (id, args) ->
        ( id
        , args
        , (fun id args -> Ast.DeclRec (id, remove_args id args prt_args @ args))
        , fun new_name cnt -> [ id, new_name, cnt + 1, remove_args id args prt_args ] )
    in
    let env_args = List.map (fun arg -> arg, arg, 0, []) args @ env in
    (match List.find_opt (fun (name, _, _, _) -> name = id) env with
     | None ->
       let decl_converted =
         closure_conversion
           ~env:(pre_env id 0 @ env_args)
           ~prt_args:(args @ prt_args)
           expr1
       in
       let expr2_converted =
         closure_conversion
           ~env:((id, id, 0, remove_args id args prt_args) :: env)
           ~prt_args
           expr2
       in
       CLetIn (declared id args, decl_converted, expr2_converted)
     | Some (_, old_name, cnt, _) ->
       let new_name = get_new_name old_name cnt in
       let decl_converted =
         closure_conversion
           ~env:(pre_env new_name cnt @ env_args)
           ~prt_args:(args @ prt_args)
           expr1
       in
       let expr2_converted =
         closure_conversion
           ~env:((id, new_name, cnt + 2, remove_args id args prt_args) :: env)
           ~prt_args
           expr2
       in
       CLetIn (declared new_name args, decl_converted, expr2_converted))
  | Ast.Fun (args, body) ->
    let env_args = List.map (fun arg -> arg, arg, 0, []) args @ env in
    let body_converted =
      closure_conversion ~env:env_args ~prt_args:(args @ prt_args) body
    in
    CApp
      ( CFun (remove_args "" args prt_args @ args, body_converted)
      , List.map (fun arg -> CId arg) prt_args )
  | Ast.App (func, args) ->
    let func_converted = closure_conversion ~env ~prt_args func in
    let args_converted = List.map (closure_conversion ~env ~prt_args) args in
    CApp (func_converted, args_converted)
;;

let clos_conv ast =
  List.fold_left
    (fun cc_ast ast ->
      cc_ast
      >>= fun (cc_ast, funs) ->
      let c_ast = closure_conversion ~env:funs ast in
      let new_funs =
        match c_ast with
        | CLet (d, _) | CLetIn (d, _, _) ->
          (match d with
           | Ast.Decl (new_name, _) | Ast.DeclRec (new_name, _) ->
             (match ast with
              | Ast.Let (d, _) | Ast.LetIn (d, _, _) ->
                (match d with
                 | Ast.Decl (old_name, _) | Ast.DeclRec (old_name, _) ->
                   (old_name, new_name, 0, []) :: funs)
              | _ -> (new_name, new_name, 0, []) :: funs))
        | _ -> funs
      in
      Result (cc_ast @ [ c_ast ], new_funs))
    (Result ([], default_fun))
    ast
  >>= fun (ast, _) -> Result ast
;;
