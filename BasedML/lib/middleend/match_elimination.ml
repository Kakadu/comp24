(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Common.StateMonad
open Llast
open Ast

type bound_expr = BoundExpression of pattern * llexpr

let rec map1 f = function
  | [] -> return []
  | h :: tl -> f h >>= fun c -> map1 f tl >>= fun lst -> return (c :: lst)
;;

type context = { reserved_names : (string, Base.String.comparator_witness) Base.Set.t }

let empty_context = { reserved_names = (module Base.String) |> Base.Set.empty }

let rec collect_bindings_from_pat = function
  | PWildCard -> (module Base.String) |> Base.Set.empty
  | PConstant _ -> (module Base.String) |> Base.Set.empty
  | PIdentifier id -> Base.Set.add ((module Base.String) |> Base.Set.empty) id
  | PCons (left, right) ->
    let collected_in_left = collect_bindings_from_pat left in
    let collected_in_right = collect_bindings_from_pat right in
    Base.Set.union collected_in_left collected_in_right
  | PConstraint (pat, _) -> collect_bindings_from_pat pat
  | PTuple pats ->
    Base.List.fold_left
      pats
      ~init:((module Base.String) |> Base.Set.empty)
      ~f:(fun acc h -> Base.Set.union acc (collect_bindings_from_pat h))
;;

let show_idname old_name counter =
  if counter >= 0 then Printf.sprintf "%s_%d" old_name counter else old_name
;;

let rec get_all_id acc = function
  | PWildCard | PConstant _ -> acc
  | PConstraint (pat, _tp) -> get_all_id acc pat
  | PCons (pat1, pat2) -> get_all_id (get_all_id acc pat2) pat1
  | PTuple plst -> List.fold_left get_all_id acc plst
  | PIdentifier x -> x :: acc
;;

let rec generate_bindings =
  let optimized_conj operand1 operand2 =
    match operand1, operand2 with
    | _, LLConstant (CBool true) | LLConstant (CBool false), _ -> operand1
    | LLConstant (CBool true), _ | _, LLConstant (CBool false) -> operand2
    | _ -> LLApplication (LLApplication (LLIdentifier "( && )", operand1), operand2)
  in
  function
  | BoundExpression (PConstant constant, expression) ->
    ( LLApplication (LLApplication (LLIdentifier "( = )", expression), LLConstant constant)
    , [] )
    |> return
  | BoundExpression (PTuple pattern_list, expression) ->
    let tuple_helper pattern_list expression_list =
      let rec step
        collected_conditions
        collected_bindings
        new_pattern_list
        new_expression_list
        =
        match new_pattern_list, new_expression_list with
        | [], [] -> return (collected_conditions, collected_bindings)
        | pattern :: rest_patterns, expression :: rest_expressions ->
          let bound_expr = BoundExpression (pattern, expression) in
          let* head_condition, bindings = generate_bindings bound_expr in
          step
            (optimized_conj collected_conditions head_condition)
            (List.append collected_bindings bindings)
            rest_patterns
            rest_expressions
        | _ -> fail "Length mismatch"
      in
      step (LLConstant (CBool true)) [] pattern_list expression_list
    in
    (match expression with
     | LLTuple elements_list -> tuple_helper pattern_list elements_list
     | other ->
       let accessed_fields =
         List.init (List.length pattern_list) (fun num ->
           LLApplication
             (LLApplication (LLIdentifier "get_field", other), LLConstant (CInt num)))
       in
       tuple_helper pattern_list accessed_fields)
  | BoundExpression (PWildCard, _) -> (LLConstant (CBool true), []) |> return
  | BoundExpression (PCons (left, right), expression) ->
    (match expression with
     | LLApplication (LLApplication (LLIdentifier "( :: )", head), tail) ->
       let bound_left = BoundExpression (left, head) in
       let bound_right = BoundExpression (right, tail) in
       let* head_condition, head_bindings = generate_bindings bound_left in
       let* tail_condition, tail_bindings = generate_bindings bound_right in
       ( optimized_conj head_condition tail_condition
       , List.append head_bindings tail_bindings )
       |> return
     | other ->
       let tag_checking =
         LLApplication
           (LLApplication (LLIdentifier "check_tag", other), LLConstant (CInt 0))
       in
       let* head_condition, head_bindings =
         let bound_left =
           BoundExpression
             ( left
             , LLApplication
                 (LLApplication (LLIdentifier "get_field", other), LLConstant (CInt 0)) )
         in
         generate_bindings bound_left
       in
       let* tail_condition, tail_bindings =
         let bound_right =
           BoundExpression
             ( right
             , LLApplication
                 (LLApplication (LLIdentifier "get_field", other), LLConstant (CInt 1)) )
         in
         generate_bindings bound_right
       in
       ( optimized_conj tag_checking (optimized_conj head_condition tail_condition)
       , List.append head_bindings tail_bindings )
       |> return)
  | BoundExpression (PConstraint (pat, _), llexpr) ->
    let bound_expr = BoundExpression (pat, llexpr) in
    generate_bindings bound_expr
  | BoundExpression (PIdentifier name, llexpr) ->
    (LLConstant (CBool true), [ name, llexpr ]) |> return
;;

let rec generate_unique_name old_name counter =
  let* ctx = read in
  let new_name = old_name, counter in
  if Base.Set.mem
       (Base.Set.union
          ctx.reserved_names
          (Base.Set.singleton (module Base.String) old_name))
       (show_idname old_name counter)
  then generate_unique_name old_name (counter + 1)
  else
    let* _ =
      write
        { reserved_names =
            Base.Set.add ctx.reserved_names (Printf.sprintf "%s_%d" old_name counter)
        }
    in
    new_name |> return
;;

let rec match_of_pat_let = function
  | LLConstant c -> LLConstant c |> return
  | LLIdentifier id ->
    let* old_ctx = read in
    let* _ = write { reserved_names = Base.Set.add old_ctx.reserved_names id } in
    LLIdentifier id |> return
  | LLIfThenElse (guard, then_branch, else_branch) ->
    let* turned_guard = match_of_pat_let guard in
    let* turned_then_branch = match_of_pat_let then_branch in
    let* turned_else_branch = match_of_pat_let else_branch in
    LLIfThenElse (turned_guard, turned_then_branch, turned_else_branch) |> return
  | LLConstraint (llex, typ) ->
    let* turned_llexp = match_of_pat_let llex in
    LLConstraint (turned_llexp, typ) |> return
  | LLApplication (left, right) ->
    let* turned_left = match_of_pat_let left in
    let* turned_right = match_of_pat_let right in
    LLApplication (turned_left, turned_right) |> return
  | LLTuple elems ->
    let* new_elems = map1 match_of_pat_let elems in
    LLTuple new_elems |> return
  | LLLetIn (_, pat, outer, inner) ->
    let* old_ctx = read in
    let* _ =
      write
        { reserved_names =
            Base.Set.union old_ctx.reserved_names (collect_bindings_from_pat pat)
        }
    in
    let* converted_outer = match_of_pat_let outer in
    let* converted_inner = match_of_pat_let inner in
    LLMatch (converted_outer, [ pat, converted_inner ]) |> return
  | LLMatch (main_exp, cases) ->
    let* new_main_exp = match_of_pat_let main_exp in
    let rec case_helper acc = function
      | [] -> List.rev acc |> return
      | (pat, expr) :: tl ->
        let* old_ctx = read in
        let* _ =
          write
            { reserved_names =
                Base.Set.union old_ctx.reserved_names (collect_bindings_from_pat pat)
            }
        in
        let* new_expr = match_of_pat_let expr in
        case_helper ((pat, new_expr) :: acc) tl
    in
    let* new_cases = case_helper [] cases in
    LLMatch (new_main_exp, new_cases) |> return
;;

let rec eliminate_lexpr =
  let optimized_if_then_else guard then_branch else_branch =
    match guard with
    | LLConstant (CBool true) -> then_branch
    | LLConstant (CBool false) -> else_branch
    | _ -> LLIfThenElse (guard, then_branch, else_branch)
  in
  function
  | LLTuple elements ->
    let* new_elements = map1 eliminate_lexpr elements in
    LLTuple new_elements |> return
  | LLIfThenElse (guard_branch, then_branch, else_branch) ->
    let* new_guard_branch = eliminate_lexpr guard_branch in
    let* new_then_branch = eliminate_lexpr then_branch in
    let* new_else_branch = eliminate_lexpr else_branch in
    LLIfThenElse (new_guard_branch, new_then_branch, new_else_branch) |> return
  | LLApplication (left, right) ->
    let* new_left = eliminate_lexpr left in
    let* new_right = eliminate_lexpr right in
    LLApplication (new_left, new_right) |> return
  | LLConstraint (llexpr, typ) ->
    let* new_llexpr = eliminate_lexpr llexpr in
    LLConstraint (new_llexpr, typ) |> return
  | LLMatch (main_expression, match_cases) ->
    let* match_free_main_expression = eliminate_lexpr main_expression in
    let* conditions =
      map1
        (fun (pat, _) ->
          let bound_expr = BoundExpression (pat, match_free_main_expression) in
          generate_bindings bound_expr)
        match_cases
    in
    List.fold_right2
      (fun (head_condition, binds) (_, expr) acc ->
        let* match_free_lexpr = eliminate_lexpr expr in
        let* acc' = acc in
        return
          (optimized_if_then_else
             head_condition
             (List.fold_right
                (fun (name, llexpr) acc ->
                  LLLetIn (NotRec, PIdentifier name, llexpr, acc))
                binds
                match_free_lexpr)
             acc'))
      conditions
      match_cases
      (LLApplication (LLIdentifier "match_error", LLConstant CUnit) |> return)
  | LLConstant constant -> LLConstant constant |> return
  | LLIdentifier name -> LLIdentifier name |> return
  | _ -> fail "Error: Unexpected expression"
;;

let rec rename_pat = function
  | PWildCard -> PWildCard |> return
  | PConstant c -> PConstant c |> return
  | PConstraint (pat, typ) ->
    let* new_pat = rename_pat pat in
    PConstraint (new_pat, typ) |> return
  | PCons (left, right) ->
    let* new_left = rename_pat left in
    let* new_right = rename_pat right in
    PCons (new_left, new_right) |> return
  | PTuple pats ->
    let* new_pats = map1 rename_pat pats in
    PTuple new_pats |> return
  | PIdentifier id ->
    let* old_name, counter = generate_unique_name id 0 in
    PIdentifier (Printf.sprintf "%s_%d" old_name counter) |> return
;;

let process_simple_let = function
  | LLLet (PIdentifier id, args, llexpr) ->
    let rec get_new_args acc = function
      | [] -> List.rev acc |> return
      | _ :: tl ->
        let* name, counter = generate_unique_name "arg" 0 in
        let new_name = Printf.sprintf "%s_%d" name counter in
        get_new_args (new_name :: acc) tl
    in
    let rec bind_names_to_patterns base_exp names patterns =
      match names, patterns with
      | [], [] -> return base_exp
      | name :: names_tail, pattern :: patterns_tail ->
        let* ctx = read in
        let* _ = write { reserved_names = Base.Set.add ctx.reserved_names name } in
        let* inner_exp = bind_names_to_patterns base_exp names_tail patterns_tail in
        return (LLLetIn (NotRec, pattern, LLIdentifier name, inner_exp))
      | _ -> fail "Error: Mismatch in the lengths of names and patterns lists"
    in
    let* ctx = read in
    let* _ = write { reserved_names = Base.Set.add ctx.reserved_names id } in
    let* new_names = get_new_args [] args in
    let* new_llexpr = bind_names_to_patterns llexpr new_names args in
    let* llexpr_with_no_lets = match_of_pat_let new_llexpr in
    let* llexpr_with_no_matches = eliminate_lexpr llexpr_with_no_lets in
    LLLet
      (PIdentifier id, List.map (fun x -> PIdentifier x) new_names, llexpr_with_no_matches)
    |> return
  | _ -> fail "Error: unexpected lllet"
;;

let rec eliminate_match_in_declarations acc = function
  | LLDSingleLet (flag, LLLet (PIdentifier id, args, llexpr)) :: tl ->
    let* new_let = process_simple_let (LLLet (PIdentifier id, args, llexpr)) in
    eliminate_match_in_declarations (LLDSingleLet (flag, new_let) :: acc) tl
  | LLDSingleLet (flag, LLLet (arbitrary_pattern, [], llexpr)) :: tl ->
    let rec after_let_helper acc counter = function
      | [] -> List.rev acc
      | h :: tl ->
        let new_elem =
          LLDSingleLet
            ( NotRec
            , LLLet
                ( PIdentifier h
                , []
                , LLApplication
                    ( LLApplication (LLIdentifier "get_field", LLIdentifier "res")
                    , LLConstant (CInt counter) ) ) )
        in
        after_let_helper (new_elem :: acc) (counter + 1) tl
    in
    let old_names = get_all_id [] arbitrary_pattern in
    let* pat_with_new_names = rename_pat arbitrary_pattern in
    let new_names = get_all_id [] pat_with_new_names in
    let additional_lets = List.rev (after_let_helper [] 0 (List.rev old_names)) in
    let* eliminated_llexpr = eliminate_lexpr llexpr in
    let* new_body =
      eliminate_lexpr
        (LLMatch
           ( eliminated_llexpr
           , [ ( pat_with_new_names
               , LLTuple (List.map (fun x -> LLIdentifier x) (List.rev new_names)) )
             ] ))
    in
    eliminate_match_in_declarations
      (List.append
         (List.append
            additional_lets
            [ LLDSingleLet (flag, LLLet (PIdentifier "res", [], new_body)) ])
         acc)
      tl
  | LLDMutualRecDecl (Rec, lets) :: tl ->
    let* new_lets = map1 process_simple_let lets in
    let new_mut_let = LLDMutualRecDecl (Rec, new_lets) in
    eliminate_match_in_declarations (new_mut_let :: acc) tl
  | [] -> List.rev acc |> return
  | _ -> fail "Error: unexpected declaration"
;;
