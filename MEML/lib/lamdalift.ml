open Ast
open Llast
open Base

module COUNTERMONAD = struct
  let return value state = state, value

  let ( >>= ) m f state =
    let value, st = m state in
    f st value
  ;;

  let ( let* ) = ( >>= )
  let get state = state, state
  let put state _ = state, ()
  let run f = f
end

open COUNTERMONAD

type args = Args of pattern list * expression

let set_empty = Set.empty (module String)
let map_empty = Map.empty (module String)

let rec new_name env =
  let* num = get in
  let* () = put (num + 1) in
  let name = String.concat [ "lambada"; Int.to_string num ] in
  if Set.mem env name then new_name env else return name
;;

let rec arguments pl = function
  | EFun (p, e) -> arguments (p :: pl) e
  | e -> Args (List.rev pl, e)
;;

let rec init_env acc = function
  | [] -> acc
  | Let lets :: tl ->
    init_env
      (List.fold ~init:acc ~f:(fun acc (_, id_list, _) ->
         (List.fold_left ~init:acc ~f:(fun acc id -> Set.union acc (Set.add set_empty id)))
           id_list)lets)
      tl
  | Expression _ :: tl -> init_env acc tl
;;

let rec lift_expression ctx acc gctx state = function
  | EConst c ->
    (match c with
     | CNil -> LLConst CNil, acc, state
     | CBool b -> LLConst (CBool b), acc, state
     | CInt i -> LLConst (CInt i), acc, state)
  | EVar (n, _) ->
    (match Map.find ctx n with
     | Some found -> LLVar found, acc, state
     | None -> LLVar n, acc, state)
  | EIfElse (i, t, e) ->
    let li, acc, state = lift_expression ctx acc gctx state i in
    let lt, acc, state = lift_expression ctx acc gctx state t in
    let le, acc, state = lift_expression ctx acc gctx state e in
    LLIfElse (li, lt, le), acc, state
  | EApp (e1, e2, _) ->
    let le1, acc, state = lift_expression ctx acc gctx state e1 in
    let le2, acc, state = lift_expression ctx acc gctx state e2 in
    LLApp (le1, le2), acc, state
  | ETuple t ->
    let rec ltuple env acc gctx state = function
      | [] -> [], acc, state
      | e :: rest ->
        let l, acc, state = lift_expression env acc gctx state e in
        let ls, acc, state = ltuple env acc gctx state rest in
        l :: ls, acc, state
    in
    let lt, acc, state = ltuple ctx acc gctx state t in
    LLTuple lt, acc, state
  | ELetIn (r, n, e, ine) ->
    (match e with
     | EFun (_, _) ->
       let args, body =
         match arguments [] e with
         | Args (arg, expr) -> arg, expr
       in
       let state, name = run (new_name gctx) state in
       let update_ctx = Map.set ctx ~key:(String.concat n) ~data:name in
       let line, acc, state =
         match r with
         | Rec -> lift_expression update_ctx acc gctx state body
         | Notrec -> lift_expression ctx acc gctx state body
       in
       lift_expression update_ctx (LLLet [(r, [ name ], args, line)] :: acc) gctx state ine
     | _ ->
       let le, acc, state = lift_expression ctx acc gctx state e in
       let line, acc, state = lift_expression ctx acc gctx state ine in
       LLLetIn (r, n, le, line), acc, state)
  | EFun (p, e) ->
    let args, b =
      match arguments [] (EFun (p, e)) with
      | Args (arg, expr) -> arg, expr
    in
    let state, name = run (new_name gctx) state in
    let le, acc, state =
      let ctx = map_empty in
      lift_expression ctx acc gctx state b
    in
    LLVar name, LLLet [(Notrec, [ name ], args, le)] :: acc, state
  | EBinaryOp (op, e1, e2) ->
    let le1, acc, state = lift_expression ctx acc gctx state e1 in
    let le2, acc, state = lift_expression ctx acc gctx state e2 in
    LLEbinOp (op, le1, le2), acc, state
  | EMatch (m, p) ->
    let rec lift env acc gctx state = function
      | [] -> [], acc, state
      | (p, e) :: rest ->
        let lifted_expr, acc, state = lift_expression env acc gctx state e in
        let lifted_branches, acc, state = lift env acc gctx state rest in
        (p, lifted_expr) :: lifted_branches, acc, state
    in
    let lp, acc, state = lift ctx acc gctx state p in
    let lm, acc, state = lift_expression ctx acc gctx state m in
    LLMatch (lm, lp), acc, state
  | EList (e1, e2) ->
    let le1, acc, state = lift_expression ctx acc gctx state e1 in
    let le2, acc, state = lift_expression ctx acc gctx state e2 in
    LLList (le1, le2), acc, state
;;

let lift_bindings gctx state = function
    | Let bindings ->
    let transformed_bindings,acc, state =
      List.fold_left
        ~f:(fun (acc, _, state) (r, n, e) ->
          let args, expr =
            match arguments [] e with
            | Args (args, expr) -> args, expr
          in
          let lift, acc_inner, state =  
            let new_ctx = map_empty in
            lift_expression new_ctx [] gctx state expr
          in
          ((r, n, args, lift) :: acc, acc_inner, state))
        ~init:([],[], state)
        bindings
    in
    (LLLet transformed_bindings :: acc), state 
  | Expression e ->
    let lift, acc, state =
      let new_ctx = map_empty in
      lift_expression new_ctx [] gctx state e
    in
    LLExpression lift :: acc, state
;;

let lambada_lift prog =
  let lift_fold prog =
    List.fold_left prog ~init:([], 0) ~f:(fun (h, state) decl ->
      let new_acc, state = lift_bindings (init_env set_empty prog) state decl in
      h @ List.rev new_acc, state)
  in
  let lift, _ = lift_fold prog in
  lift
;;

let rec meven n = if n = 0 then 1 else modd (n - 1)
and modd n = if n = 0 then 1 else meven (n - 1)
