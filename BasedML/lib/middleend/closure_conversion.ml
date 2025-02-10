(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Base

(* Takes set of unbound identifiers and binds some of them according to pattern *)
let rec bind_pattern pat base_set =
  let rec bind_pattern_list acc = function
    | h :: tl ->
      (match h with
       | PWildCard -> bind_pattern_list acc tl
       | PCons (pat1, pat2) ->
         let new_acc = pat1 :: [ pat2 ] |> bind_pattern_list acc in
         bind_pattern_list new_acc tl
       | PIdentifier id -> bind_pattern_list (Set.remove acc id) tl
       | PTuple patterns ->
         let new_acc = bind_pattern_list acc patterns in
         bind_pattern_list new_acc tl
       | PConstant _ -> bind_pattern_list acc tl
       | PConstraint (pat, _) ->
         let new_acc = bind_pattern pat base_set in
         bind_pattern_list new_acc tl)
    | [] -> acc
  in
  match pat with
  | PWildCard | PConstant _ -> base_set
  | PCons (pat1, pat2) -> pat1 :: [ pat2 ] |> bind_pattern_list base_set
  | PIdentifier x -> Set.remove base_set x
  | PTuple pats -> bind_pattern_list base_set pats
  | PConstraint (pat, _) -> bind_pattern_list base_set [ pat ]
;;

(* Find unbound variables within an expression *)
let rec unbound_identifiers binder = function
  | EConstant _ -> (module String) |> Set.empty
  | EIdentifier id -> Set.add ((module String) |> Set.empty) id
  | EFunction (pat, exp) ->
    let unbound_in_fun = unbound_identifiers binder exp in
    binder pat unbound_in_fun
  | EApplication (left_exp, right_exp) ->
    let unbound_in_left = unbound_identifiers binder left_exp in
    let unbound_in_right = unbound_identifiers binder right_exp in
    Set.union unbound_in_left unbound_in_right
  | EIfThenElse (guard_expr, if_expr, else_expr) ->
    let unbound_in_guard = unbound_identifiers binder guard_expr in
    let unbound_in_if = unbound_identifiers binder if_expr in
    let unbound_in_else = unbound_identifiers binder else_expr in
    Set.union (Set.union unbound_in_guard unbound_in_if) unbound_in_else
  | ELetIn (_, pat, outer_exp, inner_exp) ->
    let rec set_of_pat = function
      | PWildCard | PConstant _ -> (module String) |> Set.empty
      | PIdentifier id -> Set.add ((module String) |> Set.empty) id
      | PCons (left_pat, right_pat) ->
        Set.union (set_of_pat left_pat) (set_of_pat right_pat)
      | PTuple pats ->
        let initial_set = Set.empty (module String) in
        List.fold_left
          ~init:initial_set
          ~f:(fun acc h -> Set.union acc (set_of_pat h))
          pats
      | PConstraint (pat, _) -> set_of_pat pat
    in
    let rec collect_binds acc = function
      | EFunction (pat, next) -> collect_binds (Set.union acc (set_of_pat pat)) next
      | _ -> acc
    in
    let unbound_in_outer = unbound_identifiers binder outer_exp in
    let unbound_in_inner = unbound_identifiers binder inner_exp in
    let binds = collect_binds ((module String) |> Set.empty) outer_exp in
    let unbound_in_outer_without_pat = bind_pattern pat unbound_in_outer in
    let unbound_in_inner_without_pat = bind_pattern pat unbound_in_inner in
    let unbound_in_inner_final = Set.diff unbound_in_inner_without_pat binds in
    let unbound_in_outer_final = Set.diff unbound_in_outer_without_pat binds in
    Set.union unbound_in_outer_final unbound_in_inner_final
  | ETuple exps ->
    List.fold
      exps
      ~init:((module String) |> Set.empty)
      ~f:(fun acc h -> Set.union acc (unbound_identifiers binder h))
  | EMatch (pat, branches) ->
    let unbound_in_braches =
      List.fold
        branches
        ~init:((module String) |> Set.empty)
        ~f:(fun acc (pat, exp) ->
          Set.union acc (bind_pattern pat (unbound_identifiers binder exp)))
    in
    bind_pattern pat unbound_in_braches
  | EConstraint (expr, _) -> unbound_identifiers binder expr
;;

type context =
  { lts : (string, Base.String.comparator_witness) Base.Set.t
  ; local_ctx :
      ( string
        , (string, Base.String.comparator_witness) Base.Set.t
        , Base.String.comparator_witness )
        Base.Map.t
  ; global_ctx : (string, Base.String.comparator_witness) Base.Set.t
  ; closures :
      ( string
        , (string, string, Base.String.comparator_witness) Base.Map.t
        , Base.String.comparator_witness )
        Base.Map.t
  ; reserved_names : (string, Base.String.comparator_witness) Base.Set.t
  }

let rec sep_app exp cont =
  match exp with
  | EApplication (left, right) ->
    sep_app left (fun rest_llexp -> cont (right :: rest_llexp))
  | llexp -> cont [], llexp
;;

let rec build_app_l acc = function
  | [] -> acc
  | [ h ] -> EApplication (acc, h)
  | h1 :: h2 :: tl -> build_app_l (EApplication (acc, h1)) (h2 :: tl)
;;

let rec close_function context convert = function
  | EFunction (pat, body) -> EFunction (pat, close_function context convert body)
  | expr -> convert context expr
;;

let rec get_global_names = function
  | PIdentifier id -> Set.add ((module String) |> Set.empty) id
  | PWildCard | PConstant _ -> (module String) |> Set.empty
  | PConstraint (pat, _) -> get_global_names pat
  | PCons (pat1, pat2) ->
    List.fold_left
      [ pat1; pat2 ]
      ~f:(fun acc h -> Set.union acc (get_global_names h))
      ~init:((module String) |> Set.empty)
  | PTuple pats ->
    List.fold_left
      pats
      ~f:(fun acc h -> Set.union acc (get_global_names h))
      ~init:((module String) |> Set.empty)
;;

let stdlib_set =
  Set.of_list
    (module String)
    [ "( + )"; "( :: )"; "( * )"; "( - )"; "( == )"; "( = )"; "( / )"; "print_int" ]
;;

let convert global_context declaration =
  let rec helper context =
    let rename_app app =
      let names, main = sep_app app (fun x -> x) in
      match main with
      | EIdentifier id ->
        (match Map.find context.closures id with
         | None -> app
         | Some name_map ->
           let rec substitute_name = function
             | EIdentifier id ->
               (match Map.find name_map id with
                | None -> EIdentifier id
                | Some new_id -> EIdentifier new_id)
             | EConstant c -> EConstant c
             | EConstraint (exp, typ) -> EConstraint (substitute_name exp, typ)
             | EApplication (left, right) ->
               EApplication (substitute_name left, substitute_name right)
             | ETuple exps -> ETuple (List.map exps ~f:(fun a -> substitute_name a))
             | EIfThenElse (guard, then_br, else_br) ->
               EIfThenElse
                 (substitute_name guard, substitute_name then_br, substitute_name else_br)
             | EFunction (pat, body) -> EFunction (pat, substitute_name body)
             | EMatch (pat, cases) ->
               EMatch (pat, List.map cases ~f:(fun (p, exp) -> p, substitute_name exp))
             | ELetIn (flag, pat, exp1, exp2) -> ELetIn (flag, pat, exp1, exp2)
           in
           let new_names = List.map (List.rev names) ~f:substitute_name in
           build_app_l main new_names)
      | _ -> app
    in
    function
    | EConstant const -> EConstant const
    | EIdentifier id ->
      (match Map.find context.local_ctx id with
       | Some free ->
         let ids = List.map (Set.to_list free) ~f:(fun x -> EIdentifier x) in
         let before_renaming =
           List.fold_left
             ids
             ~f:(fun f arg -> EApplication (f, arg))
             ~init:(EIdentifier id)
         in
         rename_app before_renaming
       | None -> EIdentifier id)
    | EFunction (pat, body) ->
      let unbound_names = unbound_identifiers bind_pattern (EFunction (pat, body)) in
      let unbound_names_without_global =
        Set.diff (Set.diff unbound_names context.global_ctx) stdlib_set
      in
      let unbound_ids_patterns =
        List.map (Set.to_list unbound_names_without_global) ~f:(fun x -> PIdentifier x)
      in
      let unbound_ids_exps =
        List.map (Set.to_list unbound_names_without_global) ~f:(fun x -> EIdentifier x)
      in
      let closed_fun =
        close_function
          { context with
            global_ctx = Set.diff context.global_ctx stdlib_set
          ; reserved_names =
              Set.union
                (Set.union context.reserved_names (get_global_names pat))
                (unbound_identifiers (fun _ t -> t) body)
          }
          helper
          (EFunction (pat, body))
      in
      let new_fun =
        List.fold_right
          ~f:(fun pat exp -> EFunction (pat, exp))
          unbound_ids_patterns
          ~init:closed_fun
      in
      let before_renaming =
        List.fold_left
          unbound_ids_exps
          ~f:(fun f arg -> EApplication (f, arg))
          ~init:new_fun
      in
      rename_app before_renaming
    | EApplication (left, right) ->
      let before_renaming =
        EApplication
          ( helper
              { context with
                global_ctx = Set.diff (Set.diff context.global_ctx context.lts) stdlib_set
              ; reserved_names =
                  Set.union
                    context.reserved_names
                    (unbound_identifiers (fun _ t -> t) left)
              }
              left
          , helper
              { context with
                global_ctx = Set.diff (Set.diff context.global_ctx context.lts) stdlib_set
              ; reserved_names =
                  Set.union
                    context.reserved_names
                    (unbound_identifiers (fun _ t -> t) right)
              }
              right )
      in
      rename_app before_renaming
    | EIfThenElse (guard, then_branch, else_branch) ->
      let global_ctx = Set.diff context.global_ctx stdlib_set in
      EIfThenElse
        ( helper
            { context with
              global_ctx = Set.diff global_ctx stdlib_set
            ; reserved_names =
                Set.union
                  context.reserved_names
                  (unbound_identifiers (fun _ t -> t) guard)
            }
            guard
        , helper
            { context with
              global_ctx = Set.diff global_ctx stdlib_set
            ; reserved_names =
                Set.union
                  context.reserved_names
                  (unbound_identifiers (fun _ t -> t) then_branch)
            }
            then_branch
        , helper
            { context with
              global_ctx = Set.diff global_ctx stdlib_set
            ; reserved_names =
                Set.union
                  context.reserved_names
                  (unbound_identifiers (fun _ t -> t) else_branch)
            }
            else_branch )
    | ELetIn (rec_flag, pat, outer, inner) ->
      let updated_reserved =
        Set.union
          (get_global_names pat)
          (Set.union
             (unbound_identifiers (fun _ t -> t) outer)
             (Set.union (unbound_identifiers (fun _ t -> t) inner) context.reserved_names))
      in
      (match pat, outer with
       (* Inner fun *)
       | PIdentifier id, EFunction (_, _) ->
         let create_name_map unbound_names_lst =
           let rec generate_unique_name counter =
             let new_name = Printf.sprintf "%s_arg_%d" id counter in
             if Set.mem
                  (Set.union context.reserved_names (Set.singleton (module String) id))
                  new_name
             then generate_unique_name (counter + 1)
             else new_name
           in
           let rec aux counter acc = function
             | [] -> acc
             | old_name :: rest when Set.mem stdlib_set old_name -> aux counter acc rest
             | old_name :: rest ->
               let new_name = generate_unique_name counter in
               aux
                 (counter + 1)
                 (Map.update acc old_name ~f:(fun existing_value ->
                    match existing_value with
                    | None | Some _ -> new_name))
                 rest
           in
           aux 1 (Map.empty (module String)) unbound_names_lst
         in
         let updated_lts = Set.add context.lts id in
         let updated_global_ctx =
           match rec_flag with
           | Rec -> Set.add (Set.diff context.global_ctx stdlib_set) id
           | NotRec -> Set.add (Set.diff context.global_ctx stdlib_set) id
         in
         let unbound_names =
           unbound_identifiers bind_pattern (ELetIn (rec_flag, pat, outer, inner))
         in
         let unbound_names_lst = Set.elements unbound_names in
         let new_closures =
           Map.update context.closures id ~f:(fun existing_value ->
             match existing_value with
             | None | Some _ -> create_name_map unbound_names_lst)
         in
         let unbound_names_without_global =
           Set.diff (Set.diff unbound_names updated_global_ctx) stdlib_set
         in
         let closed_fun =
           close_function
             { context with
               global_ctx = updated_global_ctx
             ; reserved_names = updated_reserved
             }
             helper
             outer
         in
         let unbound_ids_without_global =
           List.map (Set.to_list unbound_names_without_global) ~f:(fun x -> PIdentifier x)
         in
         let closed_outer =
           List.fold_right
             unbound_ids_without_global
             ~f:(fun pat exp -> EFunction (pat, exp))
             ~init:closed_fun
         in
         let updated_local_ctx =
           Map.set context.local_ctx ~key:id ~data:unbound_names_without_global
         in
         let closed_inner =
           helper
             { lts = updated_lts
             ; local_ctx = updated_local_ctx
             ; global_ctx = Set.diff (Set.add context.global_ctx id) stdlib_set
             ; closures = new_closures
             ; reserved_names = updated_reserved
             }
             inner
         in
         let updated_outer =
           helper
             { lts = updated_lts
             ; local_ctx = updated_local_ctx
             ; global_ctx = Set.diff (Set.add context.global_ctx id) stdlib_set
             ; closures = new_closures
             ; reserved_names = updated_reserved
             }
             closed_outer
         in
         (match Map.find new_closures id with
          | Some names_map ->
            Map.fold
              names_map
              ~init:(ELetIn (rec_flag, PIdentifier id, updated_outer, closed_inner))
              ~f:(fun ~key ~data ac ->
                ELetIn (NotRec, PIdentifier data, EIdentifier key, ac))
          | None -> ELetIn (rec_flag, PIdentifier id, updated_outer, closed_inner))
       | _ ->
         ELetIn
           ( rec_flag
           , pat
           , helper
               { context with
                 global_ctx = Set.diff context.global_ctx stdlib_set
               ; reserved_names = updated_reserved
               }
               outer
           , helper
               { context with
                 global_ctx = Set.diff context.global_ctx stdlib_set
               ; reserved_names = updated_reserved
               }
               inner ))
    | ETuple exps ->
      let new_exps =
        let updated_reserved =
          List.fold exps ~init:context.reserved_names ~f:(fun acc h ->
            Set.union acc (unbound_identifiers (fun _ t -> t) h))
        in
        List.map
          exps
          ~f:
            (helper
               { context with
                 global_ctx = Set.diff context.global_ctx stdlib_set
               ; reserved_names = updated_reserved
               })
      in
      ETuple new_exps
    | EMatch (pat, branches) ->
      let new_branches =
        let updated_reserved =
          Set.union
            (get_global_names pat)
            (List.fold
               branches
               ~init:context.reserved_names
               ~f:(fun acc (br_pat, br_exp) ->
                 Set.union
                   acc
                   (Set.union
                      (get_global_names br_pat)
                      (unbound_identifiers (fun _ t -> t) br_exp))))
        in
        List.map branches ~f:(fun (pat, exp) ->
          ( pat
          , helper
              { context with
                global_ctx = Set.diff context.global_ctx stdlib_set
              ; reserved_names = updated_reserved
              }
              exp ))
      in
      EMatch (pat, new_branches)
    | EConstraint (exp, type_name) ->
      EConstraint
        ( helper
            { context with
              global_ctx = Set.diff context.global_ctx stdlib_set
            ; reserved_names =
                Set.union context.reserved_names (unbound_identifiers (fun _ t -> t) exp)
            }
            exp
        , type_name )
  in
  let close_declaration global_ctx_close_decl = function
    | DSingleLet (flag, DLet (pat, exp)) ->
      DSingleLet
        ( flag
        , DLet
            ( pat
            , close_function
                { lts = (module String) |> Set.empty
                ; local_ctx = (module String) |> Map.empty
                ; global_ctx = Set.diff global_ctx_close_decl stdlib_set
                ; closures = (module String) |> Map.empty
                ; reserved_names = get_global_names pat
                }
                helper
                exp ) )
    | DMutualRecDecl (flag, decls) ->
      let rec handle_mutual_rec global_ctx_mut_let = function
        | [] -> [], Set.diff global_ctx_mut_let stdlib_set
        | DLet (pat, exp) :: tl ->
          let closed_exp =
            close_function
              { lts = (module String) |> Set.empty
              ; local_ctx = (module String) |> Map.empty
              ; global_ctx = Set.diff global_ctx_mut_let stdlib_set
              ; closures = (module String) |> Map.empty
              ; reserved_names = get_global_names pat
              }
              helper
              exp
          in
          let new_decl, new_ctx = handle_mutual_rec global_ctx_mut_let tl in
          DLet (pat, closed_exp) :: new_decl, new_ctx
      in
      let new_decls, _ = handle_mutual_rec global_ctx_close_decl decls in
      DMutualRecDecl (flag, new_decls)
  in
  close_declaration global_context declaration
;;

let convert_ast ast =
  let close ast =
    List.fold_left
      ast
      ~init:([], (module String) |> Set.empty)
      ~f:(fun (acc, ctx) ->
        function
        | DSingleLet (flag, DLet (pat, body)) ->
          ( convert ctx (DSingleLet (flag, DLet (pat, body))) :: acc
          , Set.union ctx (get_global_names pat) )
        | DMutualRecDecl (flag, decls) ->
          convert ctx (DMutualRecDecl (flag, decls)) :: acc, ctx)
  in
  let converted_ast, _ = ast |> close in
  converted_ast |> List.rev
;;

let test_closure_convert ast =
  let converted = convert_ast ast in
  Stdlib.Format.printf "%s" (Ast.show_declarations converted)
;;
