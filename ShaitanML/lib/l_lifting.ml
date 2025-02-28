open Ast
open Base
open L_lifting_ast

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

type bindings = Args_body of pattern list * expr

let get_new_num =
  let* i = get in
  let* () = put (i + 1) in
  return i
;;

let rec collect_bindings_from_pat = function
  | PAny -> (module String) |> Set.empty
  | PConst _ -> (module String) |> Set.empty
  | PVar id -> Set.add ((module String) |> Set.empty) id
  | PCons (left, right) ->
    let collected_in_left = collect_bindings_from_pat left in
    let collected_in_right = collect_bindings_from_pat right in
    Set.union collected_in_left collected_in_right
  | PConstraint (pat, _) -> collect_bindings_from_pat pat
  | PTuple pats ->
    List.fold_left
      pats
      ~init:((module String) |> Set.empty)
      ~f:(fun acc h -> Set.union acc (collect_bindings_from_pat h))
;;

let rec new_name env =
  let* new_num = get_new_num in
  let name_candidate = String.concat [ "ll_"; Int.to_string new_num ] in
  if Set.mem env name_candidate then new_name env else return name_candidate
;;

let rec collect_function_arguments collected = function
  | EFun (pat, next) -> collect_function_arguments (pat :: collected) next
  | expr -> Args_body (List.rev collected, expr)
;;

let rec init_env acc = function
  | [] -> acc
  | SValue (_, [ (pat, _) ]) :: tl ->
    init_env (Set.union acc (collect_bindings_from_pat pat)) tl
  | SValue (_, lst) :: tl ->
    let bindings_from_mutual =
      List.fold_left lst ~init:acc ~f:(fun acc (pat, _) ->
        Set.union acc (collect_bindings_from_pat pat))
    in
    init_env bindings_from_mutual tl
;;

let prog_lift prog =
  let rec lift_expr ctx acc global_ctx state = function
    | EConst const ->
      (match const with
       | CNil -> LLConst CNil, acc, state
       | CUnit -> LLConst CUnit, acc, state
       | CBool flag -> LLConst (CBool flag), acc, state
       | CInt integer -> LLConst (CInt integer), acc, state
       | CString s -> LLConst (CString s), acc, state)
    | EVar id ->
      (match Map.find ctx id with
       | Some found -> LLVar found, acc, state
       | None -> LLVar id, acc, state)
    | EIf (guard_expr, if_expr, else_expr) ->
      let lifted_guard, acc, state = lift_expr ctx acc global_ctx state guard_expr in
      let lifted_if, acc, state = lift_expr ctx acc global_ctx state if_expr in
      let lifted_elese, acc, state = lift_expr ctx acc global_ctx state else_expr in
      LLIf (lifted_guard, lifted_if, lifted_elese), acc, state
    | EApply (left_exp, right_exp) ->
      let lifted_left, acc, state = lift_expr ctx acc global_ctx state left_exp in
      let lifted_right, acc, state = lift_expr ctx acc global_ctx state right_exp in
      LLApplication (lifted_left, lifted_right), acc, state
    | EConstraint (expr, typ) ->
      let lifted, ll_list, state = lift_expr ctx acc global_ctx state expr in
      LLConstraint (lifted, typ), ll_list, state
    | ETuple exp_list ->
      let rec lift_exprs env acc global_ctx state = function
        | [] -> [], acc, state
        | e :: rest ->
          let l, acc, state = lift_expr env acc global_ctx state e in
          let ls, acc, state = lift_exprs env acc global_ctx state rest in
          l :: ls, acc, state
      in
      let lifted_exprs, acc, state = lift_exprs ctx acc global_ctx state exp_list in
      LLTuple lifted_exprs, acc, state
    | EMatch (exp, branches) ->
      let rec lift_branches env acc global_ctx state = function
        | [] -> [], acc, state
        | (p, e) :: rest ->
          let lifted_expr, acc, state = lift_expr env acc global_ctx state e in
          let lifted_branches, acc, state = lift_branches env acc global_ctx state rest in
          (p, lifted_expr) :: lifted_branches, acc, state
      in
      let lifted_branches, acc, state = lift_branches ctx acc global_ctx state branches in
      let lifted_exp, acc, state = lift_expr ctx acc global_ctx state exp in
      LLMatch (lifted_exp, lifted_branches), acc, state
    | ELet (rec_flag, (pat, outer), inner) ->
      (match pat, outer with
       | PVar id, EFun (_, _) ->
         let args, new_outer =
           match collect_function_arguments [] outer with
           | Args_body (arg, expr) -> arg, expr
         in
         let state, fresh_name = run (new_name global_ctx) state in
         let updated_ctx = Map.set ctx ~key:id ~data:fresh_name in
         let lifted_outer, acc, state =
           match rec_flag with
           | Rec -> lift_expr updated_ctx acc global_ctx state new_outer
           | Nonrec -> lift_expr ctx acc global_ctx state new_outer
         in
         lift_expr
           updated_ctx
           (LLSingleLet (rec_flag, LLLet (PVar fresh_name, args, lifted_outer)) :: acc)
           global_ctx
           state
           inner
       | _ ->
         let lifted_outer, acc, state = lift_expr ctx acc global_ctx state outer in
         let lifted_inner, acc, state = lift_expr ctx acc global_ctx state inner in
         LLLetIn (rec_flag, pat, lifted_outer, lifted_inner), acc, state)
    | EFun (pat, body) ->
      let arguments, new_body =
        match collect_function_arguments [] (EFun (pat, body)) with
        | Args_body (arg, expr) -> arg, expr
      in
      let state, fresh_name = run (new_name global_ctx) state in
      let lifted, acc, state =
        let new_ctx = (module String) |> Map.empty in
        lift_expr new_ctx acc global_ctx state new_body
      in
      ( LLVar fresh_name
      , LLSingleLet (Nonrec, LLLet (PVar fresh_name, arguments, lifted)) :: acc
      , state )
    | ECons (left, right) ->
      let lifted_left, acc, state = lift_expr ctx acc global_ctx state left in
      let lifted_right, acc, state = lift_expr ctx acc global_ctx state right in
      LLCons (lifted_left, lifted_right), acc, state
  in
  let lift_bindings global_ctx state = function
    | SValue (rec_flag, [ (pat, e) ]) ->
      let pats, expr =
        match collect_function_arguments [] e with
        | Args_body (args, expr) -> args, expr
      in
      let lifted, acc, state =
        let new_ctx = (module String) |> Map.empty in
        lift_expr new_ctx [] global_ctx state expr
      in
      LLSingleLet (rec_flag, LLLet (pat, pats, lifted)) :: acc, state
    | SValue (rec_flag, dlets) ->
      let rec dlets_helper lifted_acc llets_acc cur_state = function
        | (pat, e) :: tl ->
          let p, expr =
            match collect_function_arguments [] e with
            | Args_body (args, expr) -> args, expr
          in
          let lifted, acc, new_state =
            let new_ctx = (module String) |> Map.empty in
            lift_expr new_ctx [] global_ctx cur_state expr
          in
          dlets_helper
            (LLLet (pat, p, lifted) :: lifted_acc)
            (acc @ llets_acc)
            new_state
            tl
        | [] -> List.rev lifted_acc, llets_acc, cur_state
      in
      let lifted_acc, llets_acc, cur_state = dlets_helper [] [] 0 dlets in
      LLMutLetRec (rec_flag, lifted_acc) :: llets_acc, cur_state
  in
  let lift_fold prog =
    List.fold_left prog ~init:([], 0) ~f:(fun (h, state) decl ->
      let new_acc, state =
        lift_bindings (init_env ((module String) |> Set.empty) prog) state decl
      in
      h @ List.rev new_acc, state)
  in
  lift_fold prog
;;

let lift_ast prog =
  let lifted, _ = prog_lift prog in
  lifted
;;
