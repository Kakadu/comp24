open Ast
open Base

open Base

(* Find unbound variables within an expression *)
let unbound_identifiers exp =
  let rec helper =
    (* Takes set of unbound identifiers and binds*)
    let rec bind_pattern pat base_set =
      let rec bind_pattern_list acc lis =
        match lis with
        | h :: tl ->
          (match h with
           | PAny -> bind_pattern_list acc tl
           | PCons (pat1, pat2) ->
             let new_acc = bind_pattern_list acc (pat1 :: [ pat2 ]) in
             bind_pattern_list new_acc tl
           | PVar id -> bind_pattern_list (Set.remove acc id) tl
           | PTuple patterns ->
             let new_acc = bind_pattern_list acc patterns in
             bind_pattern_list new_acc tl
           | PConst _ -> bind_pattern_list acc tl
           | PConstraint (pat, _) ->
             let new_acc = bind_pattern pat base_set in
             bind_pattern_list new_acc tl)
        | [] -> acc
      in
      match pat with
      | PAny | PConst _ -> base_set
      | PCons (pat1, pat2) -> bind_pattern_list base_set (pat1 :: [ pat2 ])
      | PVar x -> Set.remove base_set x
      | PTuple pats -> bind_pattern_list base_set pats
      | PConstraint (pat, _) -> bind_pattern_list base_set [ pat ]
    in
    function
    | EConst _ -> (module String) |> Set.empty
    | EVar id -> if id != "+" && id != "-" && id != "*" && id != "/" then Set.add ((module String) |> Set.empty) id else (module String) |> Set.empty
    | EFun (pat, exp) ->
      let unbound_in_fun = helper exp in
      bind_pattern pat unbound_in_fun
    | EApply (left_exp, right_exp) ->
      let unbound_in_left = helper left_exp in
      let unbound_in_right = helper right_exp in
      Set.union unbound_in_left unbound_in_right
    | EIf (guard_expr, if_expr, else_expr) ->
      let unbound_in_guard = helper guard_expr in
      let unbound_in_if = helper if_expr in
      let unbound_in_else = helper else_expr in
      Set.union (Set.union unbound_in_guard unbound_in_if) unbound_in_else
    | ELet (rec_flag, bindings, inner_exp) ->
      let rec set_of_pat = function
        | PAny | PConst _ -> (module String) |> Set.empty
        | PVar id -> Set.add ((module String) |> Set.empty) id
        | PCons (left_pat, right_pat) ->
          Set.union (set_of_pat left_pat) (set_of_pat right_pat)
        | PTuple pats ->
          let rec tuple_helper acc ls =
            match ls with
            | [] -> acc
            | h :: tl -> tuple_helper (Set.union acc (set_of_pat h)) tl
          in
          tuple_helper ((module String) |> Set.empty) pats
        | PConstraint (pat, _) -> set_of_pat pat
      in
      let rec collect_binds acc = function
        | EFun (pat, next) -> collect_binds (Set.union acc (set_of_pat pat)) next
        | _ -> acc
      in
      let unbound_in_bindings = 
        List.fold bindings ~init:((module String) |> Set.empty)
          ~f:(fun acc (pat, exp) -> 
              Set.union acc (Set.diff (helper exp) (set_of_pat pat)))
      in
      let unbound_in_inner = helper inner_exp in
      let binds = 
        List.fold bindings ~init:((module String) |> Set.empty)
          ~f:(fun acc (pat, exp) -> 
              Set.union acc (collect_binds ((module String) |> Set.empty) exp))
      in
      let bound_vars = 
        List.fold bindings ~init:((module String) |> Set.empty)
          ~f:(fun acc (pat, _) -> Set.union acc (set_of_pat pat))
      in
      let unbound_in_inner_final = 
        Set.diff (Set.diff unbound_in_inner bound_vars) binds 
      in
      Set.union unbound_in_bindings unbound_in_inner_final
    | ETuple exps ->
      List.fold
        exps
        ~init:((module String) |> Set.empty)
        ~f:(fun acc h -> Set.union acc (helper h))
    | EMatch (pat, branches) ->
      let unbound_in_braches =
        List.fold
          branches
          ~init:((module String) |> Set.empty)
          ~f:(fun acc (pat, exp) -> Set.union acc (bind_pattern pat (helper exp)))
      in
      Set.union (helper pat) unbound_in_braches
    | ECons (hd, tl) ->
      Set.union (helper hd) (helper tl)
  in
  helper exp

let rec close_function lts local_ctx global_ctx convert = function
  | EFun (pat, body) ->
    EFun (pat, close_function lts local_ctx global_ctx convert body)
  | expr -> convert lts local_ctx global_ctx expr

let rec get_global_names = function
  | PVar id -> Set.add ((module String) |> Set.empty) id
  | PAny | PConst _ -> (module String) |> Set.empty
  | PConstraint (pat, _) -> get_global_names pat
  | PCons (pat1, pat2) ->
    Set.union (get_global_names pat1) (get_global_names pat2)
  | PTuple pats ->
    List.fold pats
      ~init:((module String) |> Set.empty)
      ~f:(fun acc pat -> Set.union acc (get_global_names pat))

let convert_binding global_ctx declaration =
  let rec helper lts local_ctx global_ctx = function
    | EConst const -> EConst const
    | EVar id ->
      (match Map.find local_ctx id with
       | Some free ->
         let ids = List.map (Set.to_list free) ~f:(fun x -> EVar x) in
         List.fold_left ids ~init:(EVar id) ~f:(fun f arg -> EApply (f, arg))
       | None -> EVar id)
    | EFun (pat, body) ->
      let unbound_names = unbound_identifiers (EFun (pat, body)) in
      let unbound_names_without_global = Set.diff unbound_names global_ctx in
      let unbound_ids_patterns =
        List.map (Set.to_list unbound_names_without_global) ~f:(fun x -> PVar x)
      in
      let unbound_ids_exps =
        List.map (Set.to_list unbound_names_without_global) ~f:(fun x -> EVar x)
      in
      let closed_fun =
        close_function lts local_ctx global_ctx helper (EFun (pat, body))
      in
      let new_fun =
        List.fold_right
          unbound_ids_patterns
          ~init:closed_fun
          ~f:(fun pat exp -> EFun (pat, exp))
      in
      List.fold_left unbound_ids_exps ~init:new_fun
        ~f:(fun f arg -> EApply (f, arg))
    | EApply (left, right) ->
      EApply
        ( helper lts local_ctx (Set.diff global_ctx lts) left
        , helper lts local_ctx (Set.diff global_ctx lts) right )
    | EIf (guard, then_branch, else_branch) ->
      EIf
        ( helper lts local_ctx global_ctx guard
        , helper lts local_ctx global_ctx then_branch
        , helper lts local_ctx global_ctx else_branch )
    | ELet (rec_flag, bindings, inner) ->
      let handle_binding (pat, exp) =
        match pat with
        | PVar id when (match exp with EFun _ -> true | _ -> false) ->
          let updated_lts = Set.add lts id in
          let updated_global_env =
            match rec_flag with
            | Rec -> Set.add global_ctx id
            | Nonrec -> Set.add global_ctx id
          in
          let unbound_names = unbound_identifiers exp in
          let unbound_names_without_global = Set.diff unbound_names updated_global_env in
          let closed_fun = close_function lts local_ctx updated_global_env helper exp in
          let unbound_ids_without_global =
            List.map (Set.to_list unbound_names_without_global) ~f:(fun x -> PVar x)
          in
          let closed_exp =
            List.fold_right unbound_ids_without_global
              ~init:closed_fun
              ~f:(fun pat exp -> EFun (pat, exp))
          in
          let updated_local_env =
            Map.set local_ctx ~key:id ~data:unbound_names_without_global
          in
          (pat, closed_exp), updated_local_env, Set.add global_ctx id
        | _ -> 
          (pat, helper lts local_ctx global_ctx exp), local_ctx, global_ctx
      in
      let new_bindings, final_local_ctx, final_global_ctx =
        List.fold bindings
          ~init:([], local_ctx, global_ctx)
          ~f:(fun (acc_binds, acc_local, acc_global) binding ->
              let new_binding, new_local, new_global = handle_binding binding in
              new_binding :: acc_binds, new_local, new_global)
      in
      ELet (rec_flag, List.rev new_bindings,
            helper lts final_local_ctx final_global_ctx inner)
    | ETuple exps ->
      ETuple (List.map exps ~f:(helper lts local_ctx global_ctx))
    | EMatch (exp, branches) ->
      EMatch 
        ( helper lts local_ctx global_ctx exp
        , List.map branches ~f:(fun (pat, exp) -> 
            pat, helper lts local_ctx global_ctx exp))
    | ECons (hd, tl) ->
      ECons (helper lts local_ctx global_ctx hd,
             helper lts local_ctx global_ctx tl)
  in
  match declaration with
  | SEval expr -> 
    SEval (helper ((module String) |> Set.empty) 
             ((module String) |> Map.empty) global_ctx expr)
  | SValue (flag, bindings) ->
    let new_bindings = List.map bindings ~f:(fun (pat, exp) ->
        ( pat
        , helper ((module String) |> Set.empty)
            ((module String) |> Map.empty)
            global_ctx
            exp ))
    in
    SValue (flag, new_bindings)

let convert_ast ast =
  let close ast =
    List.fold ast
      ~init:([], (module String) |> Set.empty)
      ~f:(fun (acc, ctx) decl ->
          match decl with
          | SEval expr -> 
            convert_binding ctx (SEval expr) :: acc, ctx
          | SValue (flag, bindings) ->
            let new_ctx = 
              List.fold bindings ~init:ctx
                ~f:(fun acc (pat, _) -> Set.union acc (get_global_names pat))
            in
            convert_binding ctx (SValue (flag, bindings)) :: acc, new_ctx)
  in
  match ast |> close with
  | converted_ast, _ -> converted_ast |> List.rev


  let check_closure s = match Parser.test_parse s with | Ok ast ->
(* module ClosureConversion = struct
  open Ast

  (* Тип для представления замыканий *)
  type closure = {
    params: id list;
    free_vars: id list;
    body: expr;
  }

  (* Накапливаем свободные переменные *)
  let rec collect_free_vars bound_vars = function
    | EVar id when not (List.mem id bound_vars) -> [id]
    | EVar _ -> []
    | EConst _ -> []
    | ETuple es -> List.concat_map (collect_free_vars bound_vars) es
    | ECons (e1, e2) -> 
        collect_free_vars bound_vars e1 @ collect_free_vars bound_vars e2
    | EApply (e1, e2) ->
        collect_free_vars bound_vars e1 @ collect_free_vars bound_vars e2
    | EIf (e1, e2, e3) ->
        collect_free_vars bound_vars e1 @ 
        collect_free_vars bound_vars e2 @
        collect_free_vars bound_vars e3
    | EMatch (e, cases) ->
        let case_vars (pat, expr) =
          let bound = pattern_vars pat @ bound_vars in
          collect_free_vars bound expr
        in
        collect_free_vars bound_vars e @
        List.concat_map case_vars cases
    | ELet (rec_flag, bindings, e) ->
        let bound = match rec_flag with
          | Rec -> 
              bound_vars @ List.concat_map (fun (pat, _) -> pattern_vars pat) bindings
          | Nonrec -> bound_vars
        in
        let binding_vars =
          List.concat_map (fun (_, expr) -> collect_free_vars bound expr) bindings
        in
        binding_vars @ collect_free_vars bound e
    | EFun (pat, body) ->
        let bound = pattern_vars pat @ bound_vars in
        collect_free_vars bound body

  (* Получаем переменные из паттерна *)
  and pattern_vars = function
    | PVar id -> [id]
    | PTuple ps -> List.concat_map pattern_vars ps
    | PCons (p1, p2) -> pattern_vars p1 @ pattern_vars p2
    | PConstraint (p, _) -> pattern_vars p
    | PAny | PConst _ -> []

  (* Преобразование выражения *)
  let rec convert bound_vars expr =
    match expr with
    | EFun (pat, body) ->
        let params = pattern_vars pat in
        let free = List.filter 
          (fun v -> not (List.mem v (params @ bound_vars)))
          (collect_free_vars bound_vars body)
        in
        let closure = {
          params;
          free_vars = free;
          body = convert (params @ bound_vars) body
        } in
        (* Создаем замыкание с захваченными переменными *)
        make_closure closure
    | ELet (rf, bindings, e) ->
        let new_bound = match rf with
          | Rec -> 
              bound_vars @ List.concat_map (fun (pat, _) -> pattern_vars pat) bindings
          | Nonrec -> bound_vars
        in
        let new_bindings = 
          List.map (fun (pat, e) -> (pat, convert new_bound e)) bindings 
        in
        ELet (rf, new_bindings, convert new_bound e)
    | EApply (e1, e2) ->
        EApply (convert bound_vars e1, convert bound_vars e2)
    | EIf (e1, e2, e3) ->
        EIf (convert bound_vars e1, 
             convert bound_vars e2,
             convert bound_vars e3)
    | EMatch (e, cases) ->
        let convert_case (pat, expr) =
          (pat, convert (pattern_vars pat @ bound_vars) expr)
        in
        EMatch (convert bound_vars e, List.map convert_case cases)
    | ETuple es ->
        ETuple (List.map (convert bound_vars) es)
    | ECons (e1, e2) ->
        ECons (convert bound_vars e1, convert bound_vars e2)
    | e -> e

  (* Создание замыкания *)
  and make_closure closure =
    (* Здесь можно реализовать конкретное представление замыкания,
       например, как кортеж с функцией и списком захваченных переменных *)
    ETuple [
      EFun (PTuple (List.map (fun id -> PVar id) closure.params),
            closure.body);
      ETuple (List.map (fun id -> EVar id) closure.free_vars)
    ]

  (* Преобразование всей программы *)
  let convert_program = function
    | SEval e -> SEval (convert [] e)
    | SValue (rf, bindings) ->
        let new_bindings =
          List.map (fun (pat, e) -> (pat, convert [] e)) bindings
        in
        SValue (rf, new_bindings)
end *)

(* module ClosureConversion = struct
  open Ast
  module StringSet = Set.Make(String)
  module StringMap = Map.Make(String)

  (* Собираем свободные переменные *)
  let rec pattern_vars = function
    | PVar id -> [id]
    | PTuple ps -> List.concat_map pattern_vars ps
    | PCons (p1, p2) -> pattern_vars p1 @ pattern_vars p2
    | PConstraint (p, _) -> pattern_vars p
    | PAny | PConst _ -> []
  let free_vars expr =
    let rec efun_helper set = function
      | EFun (PVar x, e) -> 
          efun_helper (StringSet.add x set) e
      | EFun (pat, e) -> 
          let bound = StringSet.of_list (pattern_vars pat) in
          efun_helper (StringSet.union set bound) e
      | _ -> set
    in

    let rec helper = function
      | EConst _ -> StringSet.empty
      | EVar x -> StringSet.singleton x
      | ETuple es -> 
          List.fold_left StringSet.union StringSet.empty 
            (List.map helper es)
      | ECons (e1, e2) ->
          StringSet.union (helper e1) (helper e2)
      | EApply (e1, e2) ->
          StringSet.union (helper e1) (helper e2)
      | EIf (e1, e2, e3) ->
          StringSet.union (helper e1)
            (StringSet.union (helper e2) (helper e3))
      | EMatch (e, cases) ->
          let case_vars (pat, expr) =
            let bound = StringSet.of_list (pattern_vars pat) in
            StringSet.diff (helper expr) bound
          in
          List.fold_left StringSet.union (helper e)
            (List.map case_vars cases)
      | EFun (pat, e) ->
          let bound = StringSet.of_list (pattern_vars pat) in
          StringSet.diff (helper e) bound
      | ELet (rec_flag, bindings, e) ->
          let binding_vars (pat, expr) =
            let bound = StringSet.of_list (pattern_vars pat) in
            let free_in_expr = helper expr in
            match rec_flag with
            | Rec -> StringSet.diff free_in_expr bound
            | Nonrec -> free_in_expr
          in
          let free_in_bindings =
            List.fold_left StringSet.union StringSet.empty
              (List.map binding_vars bindings)
          in
          let bound_vars =
            List.fold_left StringSet.union StringSet.empty
              (List.map (fun (pat, _) -> 
                StringSet.of_list (pattern_vars pat)) bindings)
          in
          StringSet.union free_in_bindings
            (StringSet.diff (helper e) bound_vars)
    in
    helper expr

  (* Преобразование выражения *)
  let closure_conversion global_env =
    let rec expr_closure local_env global_env = function
      | EConst c -> EConst c
      | EVar x as orig ->
          (match StringMap.find_opt x local_env with
           | Some free ->
               let free_vars = StringSet.elements free in
               List.fold_left
                 (fun acc var -> EApply(acc, EVar var))
                 orig
                 free_vars
           | None -> orig)
      | ETuple es ->
          ETuple (List.map (expr_closure local_env global_env) es)
      | ECons (e1, e2) ->
          ECons (expr_closure local_env global_env e1,
                expr_closure local_env global_env e2)
      | EApply (e1, e2) ->
          EApply (expr_closure local_env global_env e1,
                 expr_closure local_env global_env e2)
      | EIf (e1, e2, e3) ->
          EIf (expr_closure local_env global_env e1,
               expr_closure local_env global_env e2,
               expr_closure local_env global_env e3)
      | EMatch (e, cases) ->
          let convert_case (pat, expr) =
            (pat, expr_closure local_env global_env expr)
          in
          EMatch (expr_closure local_env global_env e,
                 List.map convert_case cases)
      | EFun (pat, _) as orig ->
          let free = free_vars orig in
          let free' = StringSet.diff free global_env in
          let e' = efun_helper local_env global_env orig in
          let free_list = StringSet.elements free' in
          let closure =
            EFun (PTuple (List.map (fun x -> PVar x) free_list), e')
          in
          List.fold_left
            (fun acc var -> EApply(acc, EVar var))
            closure
            free_list
      | ELet (rec_flag, bindings, e2) as orig ->
          let free = free_vars orig in
          let free' = StringSet.diff free global_env in
          let convert_binding local_env (pat, e1) =
            let e1' = efun_helper local_env global_env e1 in
            (pat, e1')
          in
          let new_bindings = 
            List.map (convert_binding local_env) bindings
          in
          let bound_vars = 
            List.fold_left StringSet.union StringSet.empty
              (List.map (fun (pat, _) -> 
                StringSet.of_list (pattern_vars pat)) bindings)
          in
          let local_env' =
            StringMap.add_seq
              (List.to_seq 
                (List.map (fun x -> (x, free')) 
                  (StringSet.elements bound_vars)))
              local_env
          in
          let global_env' = 
            StringSet.union global_env bound_vars
          in
          let e2' = expr_closure local_env' global_env' e2 in
          ELet (rec_flag, new_bindings, e2')

    and efun_helper local_env global_env = function
      | EFun (pat, e) ->
          let bound = StringSet.of_list (pattern_vars pat) in
          let e' = expr_closure local_env 
            (StringSet.union global_env bound) e
          in
          EFun (pat, e')
      | expr -> expr_closure local_env global_env expr
    in
    expr_closure StringMap.empty global_env

  (* Преобразование всей программы *)
  let prog_conversion program =
    let process_str_item (acc_items, global_env) = function
      | SEval expr ->
          let expr' = closure_conversion global_env expr in
          (SEval expr' :: acc_items, global_env)
      | SValue (rec_flag, bindings) as item ->
          let bound_vars =
            List.fold_left StringSet.union StringSet.empty
              (List.map (fun (pat, _) -> 
                StringSet.of_list (pattern_vars pat)) bindings)
          in
          let global_env' = StringSet.union global_env bound_vars in
          let convert_binding (pat, expr) =
            (pat, closure_conversion global_env' expr)
          in
          let new_bindings = List.map convert_binding bindings in
          (SValue (rec_flag, new_bindings) :: acc_items, global_env')
    in
    let (converted, _) =
      List.fold_left process_str_item ([], StringSet.empty) program
    in
    List.rev converted
end *)

(* open Ast
open Base *)

(* module EnvSet = Set.Make(String);; *)
(* module ENV_SET = struct
  type t = string *)

  (* let compare (n1 : t) (n2 : t) = Base.Poly.compare n1 n2
end *)

(* module Va = Base.Map.Poly *)

(* module FreeVars = Stdlib.Set.Make (ENV_SET)
let rec collect_free_vars expr set =
  match expr with
    | EVar x -> FreeVars.singleton (x)
    | EFun (pat, expr) -> 
      let rec bypassEFun env = function
      | PVar x -> FreeVars.remove x env
      |  *)

(* let find_free_vars expr =
  let rec fun_helper set = function
    | EFun (PVar x, e) -> fun_helper (Set.add set x) e
    | EFun (_, e) -> fun_helper set e
    | _ -> set
  in
  let rec helper = function
    | EConst _ -> Set.empty (module String)
    | EVar x -> Set.singleton (module String) x
    (* | EBinOp (_, e1, e2) -> Set.union (helper e1) (helper e2) *)
    | EIf (e1, e2, e3) -> Set.union (Set.union (helper e1) (helper e2)) (helper e3)
    | EApply (e1, e2) -> Set.union (helper e1) (helper e2)
    | EFun (name, e) ->
      (match name with
       | PVar name -> Set.remove (helper e) name
       | _ -> helper e)
    | ELet (rec_flag, [ (PVar pattern_, e1) ], e2) ->
      let free1 = helper e1 in
      let free1' =
        match rec_flag with
        | Rec -> Set.remove free1 pattern_
        | Nonrec -> free1
      in
      let e1_pat = fun_helper (Set.empty (module String)) e1 in
      let free2 = Set.diff (helper e2) e1_pat in
      let free2' = Set.remove free2 pattern_ in
      Set.union free1' free2'
  in
  helper expr
;;

let closure_conv g_env decl =
  let rec expr_closure loc_env g_env = function
    | EConst x -> EConst x
    | EVar x as orig ->
      (match Map.find loc_env x with
       | Some free ->
         let ids = List.map (Set.to_list free) ~f:(fun x -> EVar x) in
         constr_apply orig ids
       | None -> orig)
    | EIf (e1, e2, e3) ->
      let e1' = expr_closure loc_env g_env e1 in
      let e2' = expr_closure loc_env g_env e2 in
      let e3' = expr_closure loc_env g_env e3 in
      EIf (e1', e2', e3')
    | EApply (e1, e2) ->
      let e1' = expr_closure loc_env g_env e1 in
      let e2' = expr_closure loc_env g_env e2 in
      EApply (e1', e2')
    | EFun (x, _) as orig ->
      let s = find_free_vars orig in
      let s' = Set.diff s g_env in
      let e' = fun_helper loc_env g_env orig in
      (match x with
       | PVar _ ->
         let fun_fold = constr_fun (List.map (Set.to_list s') ~f:(fun x -> PVar x)) e' in
         constr_apply fun_fold (List.map (Set.to_list s') ~f:(fun x -> EVar x))
       | _ -> e')
    | ELet (rec_flag, [ (PVar x, (EFun (_, _) as e1)) ], e2) as orig ->
      let free = find_free_vars orig in
      let free' = Set.diff free g_env in
      let e1' = fun_helper loc_env g_env e1 in
      let e1_closure =
        constr_fun (List.map (Set.to_list free') ~f:(fun x -> PVar x)) e1'
      in
      let local_env' = Map.set loc_env ~key:x ~data:free' in
      let e2' = expr_closure local_env' (Set.add g_env x) e2 in
      ELet (rec_flag, [ PVar x, e1_closure ], e2')
  and fun_helper local_env global_env = function
    | EFun ((PVar _ as orig), e) ->
      let e' = fun_helper local_env global_env e in
      constr_fun [ orig ] e'
    | EFun (p, e) ->
      let e' = fun_helper local_env global_env e in
      constr_fun [ p ] e'
    | expr -> expr_closure local_env global_env expr
  in
  let decl_closure global_env = function
    | ELet (rec_flag, [ (PVar id, e1) ], _) ->
      let global_env' =
        match rec_flag with
        | Rec -> Set.add g_env id
        | Nonrec -> g_env
      in
      let e' = fun_helper (Map.empty (module String)) global_env' e1 in
      ELet (rec_flag, [ PVar id, e1 ], e')
  in
  decl_closure g_env decl
;;

let convert_program = function
  | SEval e -> SEval (closure_conv (Set.empty (module String)) e)
  | SValue (rf, bindings) ->
    let new_bindings =
      List.map bindings (fun (pat, e) -> pat, closure_conv (Set.empty (module String)) e)
    in
    SValue (rf, new_bindings)
;; *)

(* if is_rec then Set.add global_env id else global_env in *)

(* let find_free_vars expr = 
  let fun_helper set = function
  | EFun (PVar x, e)
  | *)

(* let see_binding_list list_ = List.map *)
(* | Rec -> let pattern = Set.remove sub_env pat *)
(* | Nonrec -> sub_env) *)
(* fun list_ -> List.map list_ helper in  *)

(* | ELet (rec_flag, blist, expr) -> let blist_process blist =  *)

(* let free1 =  *)
(* match rec_flag with *)
(* | Rec -> Set.remove(helper blist) *)
(* | Nonrec -> *)
(* let free1' = if b then Set.remove free1 x else free1 in *)
(* let e1_pat = efun_helper (Set.empty (module String)) e1 in *)
(* let free2 = Set.diff (helper e2) e1_pat in *)
(* let free2' = Set.remove free2 x in *)
(* Set.union free1' free2' *)
(* in
  helper expr *)
(* and  *)
(* numbers = [1; 2; 3; 4; 5];; *)
(* let doubled_numbers = List.map (fun x -> x * 2) numbers;; *)
(* blist_process rec_flag blist expr helper fun_helper = List.map blist 
  (
    let free1 pat blist_expr = (helper blist_expr, blist_expr, pat) 
      in let free1' = match rec_flag with
        | Rec -> Set.remove
        | Nonrec -> 
  in free_) *)
(* (match rec_flag with
      | Rec -> Set.remove (
          match free_ with
          | (free1_res, blist_expr, pat) -> free1_res pat
          )
    )) *)
(* | Nonrec -> free1), blist_expr *)
(* in let e1_pat = fun_helper (Set.empty (module String)) blist_expr in
      let free2 = Set.diff (helper expr) e1_pat in
      let free2' = Set.remove free2 x in
      Set.union free1' free2'  *)
