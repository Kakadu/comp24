open Ast
open Base
open L_lifting_ast

module COUNTERMONAD = struct
  let return value state = state, value
  let (>>=) m f state =
    let value, st = m state in
    f st value
  let (let*) = (>>=)
  let get state = state, state
  let put state _ = state, ()
  let run f = f
end

open COUNTERMONAD

type bindings = Args_body of pattern list * expr

(* LL-specific types *)
type ll_expr =
  | LLConstant of const
  | LLIdentifier of id
  | LLIfThenElse of ll_expr * ll_expr * ll_expr
  | LLApplication of ll_expr * ll_expr
  | LLConstraint of ll_expr * type_annot
  | LLTuple of ll_expr list
  | LLMatch of ll_expr * (pattern * ll_expr) list
  | LLLetIn of rec_flag * pattern * ll_expr * ll_expr

type ll_declaration =
  | LLDSingleLet of rec_flag * ll_let
  | LLDMutualRecDecl of rec_flag * ll_let list

and ll_let = LLLet of pattern * pattern list * ll_expr

let get_new_num =
  let* i = get in
  let* () = put (i + 1) in
  return i

let rec collect_bindings_from_pat = function
  | PAny -> Set.empty (module String)
  | PConst _ -> Set.empty (module String)
  | PVar id -> Set.singleton (module String) id
  | PCons (left, right) ->
      Set.union (collect_bindings_from_pat left) (collect_bindings_from_pat right)
  | PConstraint (pat, _) -> collect_bindings_from_pat pat
  | PTuple pats ->
      List.fold_left pats
        ~init:(Set.empty (module String))
        ~f:(fun acc p -> Set.union acc (collect_bindings_from_pat p))

let rec new_name env =
  let* new_num = get_new_num in
  let name_candidate = "ll_" ^ Int.to_string new_num in
  if Set.mem env name_candidate then new_name env else return name_candidate

let rec collect_function_arguments expr =
  match expr with
  | EFun (pat, body) -> 
      let Args_body (args, rest) = collect_function_arguments body in
      Args_body (pat :: args, rest)
  | e -> Args_body ([], e)

let rec init_env acc = function
  | [] -> acc
  | SEval _ :: tl -> init_env acc tl
  | SValue (_, bindings) :: tl ->
      let new_acc = List.fold_left bindings
        ~init:acc
        ~f:(fun acc (pat, _) -> Set.union acc (collect_bindings_from_pat pat))
      in
      init_env new_acc tl

let prog_lift prog =
  let rec lift_expr ctx acc global_ctx state = function
    | EConst const -> LLConstant const, acc, state
    | EVar id ->
        (match Map.find ctx id with
         | Some found -> LLIdentifier found, acc, state
         | None -> LLIdentifier id, acc, state)
    | EIf (guard, if_expr, else_expr) ->
        let lifted_guard, acc, state = lift_expr ctx acc global_ctx state guard in
        let lifted_if, acc, state = lift_expr ctx acc global_ctx state if_expr in
        let lifted_else, acc, state = lift_expr ctx acc global_ctx state else_expr in
        LLIfThenElse (lifted_guard, lifted_if, lifted_else), acc, state
    | EApply (left, right) ->
        let lifted_left, acc, state = lift_expr ctx acc global_ctx state left in
        let lifted_right, acc, state = lift_expr ctx acc global_ctx state right in
        LLApplication (lifted_left, lifted_right), acc, state
    | ETuple exprs ->
        let rec lift_list acc state = function
          | [] -> [], acc, state
          | e :: rest ->
              let lifted, acc, state = lift_expr ctx acc global_ctx state e in
              let lifted_rest, acc, state = lift_list acc state rest in
              lifted :: lifted_rest, acc, state
        in
        let lifted_exprs, acc, state = lift_list acc state exprs in
        LLTuple lifted_exprs, acc, state
    | EMatch (e, cases) ->
        let lifted_expr, acc, state = lift_expr ctx acc global_ctx state e in
        let rec lift_cases acc state = function
          | [] -> [], acc, state
          | (pat, e) :: rest ->
              let lifted_e, acc, state = lift_expr ctx acc global_ctx state e in
              let lifted_rest, acc, state = lift_cases acc state rest in
              (pat, lifted_e) :: lifted_rest, acc, state
        in
        let lifted_cases, acc, state = lift_cases acc state cases in
        LLMatch (lifted_expr, lifted_cases), acc, state
    | EFun (pat, body) ->
        let Args_body (args, final_body) = collect_function_arguments (EFun (pat, body)) in
        let state, fresh_name = run (new_name global_ctx) state in
        let lifted_body, acc, state = 
          let new_ctx = Map.empty (module String) in
          lift_expr new_ctx acc global_ctx state final_body
        in
        LLIdentifier fresh_name,
        LLDSingleLet (Nonrec, LLLet (PVar fresh_name, args, lifted_body)) :: acc,
        state
    | ELet (rec_flag, bindings, body) ->
        let lift_binding (pat, expr) =
          let Args_body (args, final_expr) = collect_function_arguments expr in
          let lifted_expr, acc, state = lift_expr ctx acc global_ctx state final_expr in
          LLLet (pat, args, lifted_expr), acc, state
        in
        let rec lift_bindings acc state = function
          | [] -> [], acc, state
          | b :: rest ->
              let lifted_b, acc, state = lift_binding b in
              let lifted_rest, acc, state = lift_bindings acc state rest in
              lifted_b :: lifted_rest, acc, state
        in
        let lifted_bindings, acc, state = lift_bindings acc state bindings in
        let lifted_body, acc, state = lift_expr ctx acc global_ctx state body in
        match rec_flag with
        | Rec -> 
            LLLetIn (rec_flag, PTuple (List.map bindings ~f:fst), 
                    LLTuple (List.map lifted_bindings ~f:(fun (LLLet (_, _, e)) -> e)),
                    lifted_body),
            acc, state
        | Nonrec ->
            List.fold_right lifted_bindings
              ~init:(lifted_body, acc, state)
              ~f:(fun (LLLet (p, args, e)) (body, acc, state) ->
                  LLLetIn (Nonrec, p, e, body), acc, state)
  in

  let lift_str_item global_ctx state = function
    | SEval expr ->
        let lifted, acc, state = lift_expr (Map.empty (module String)) [] global_ctx state expr in
        acc @ [LLDSingleLet (Nonrec, LLLet (PAny, [], lifted))], state
    | SValue (rec_flag, bindings) ->
        let process_binding (pat, expr) =
          let Args_body (args, body) = collect_function_arguments expr in
          let lifted, acc, state = 
            lift_expr (Map.empty (module String)) [] global_ctx state body in
          LLLet (pat, args, lifted), acc, state
        in
        let rec process_all acc state = function
          | [] -> [], acc, state
          | b :: rest ->
              let lifted_b, acc, state = process_binding b in
              let lifted_rest, acc, state = process_all acc state rest in
              lifted_b :: lifted_rest, acc, state
        in
        let lifted_bindings, acc, state = process_all [] state bindings in
        match rec_flag with
        | Rec -> [LLDMutualRecDecl (rec_flag, lifted_bindings)], state
        | Nonrec -> 
            List.map lifted_bindings 
              ~f:(fun decl -> LLDSingleLet (Nonrec, decl)),
            state
  in

  let rec process_items acc state = function
    | [] -> List.rev acc, state
    | item :: rest ->
        let global_env = init_env (Set.empty (module String)) prog in
        let new_decls, state = lift_str_item global_env state item in
        process_items (List.rev_append new_decls acc) state rest
  in
  process_items [] 0 prog

let lift_ast prog =
  let lifted, _ = prog_lift prog in
  lifted