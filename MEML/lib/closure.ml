open Ast
open Base

let set_empty = Set.empty (module String)
let map_empty = Map.empty (module String)

let unrelated e =
  let rec helper =
    let bind_name name set = Set.remove set name in
    let bind_pattern p set =
      let rec bind_pattern_list acc = function
        | hd :: tl ->
          (match hd with
           | PConst _ | PWild -> bind_pattern_list acc tl
           | PCon (p1, p2) ->
             let new_acc = bind_pattern_list acc (p1 :: [ p2 ]) in
             bind_pattern_list new_acc tl
           | PVar (n, _) -> bind_pattern_list (Set.remove acc n) tl
           | PTuple t ->
             let new_acc = bind_pattern_list acc t in
             bind_pattern_list new_acc tl)
        | [] -> acc
      in
      match p with
      | PWild | PConst _ -> set
      | PCon (p1, p2) -> bind_pattern_list set (p1 :: [ p2 ])
      | PVar (n, _) -> Set.remove set n
      | PTuple t -> bind_pattern_list set t
    in
    function
    | EConst _ -> set_empty
    | EVar (n, _) -> Set.add set_empty n
    | EFun (p, e) -> bind_pattern p @@ helper e
    | EApp (e1, e2) | EBinaryOp (_, e1, e2) | EList (e1, e2) ->
      Set.union (helper e1) (helper e2)
    | EIfElse (i, t, e) -> Set.union (Set.union (helper i) (helper t)) (helper e)
    | ELetIn (_, n, e, ine) ->
      let rec pat_set = function
        | PWild | PConst _ -> set_empty
        | PVar (n, _) -> Set.add set_empty n
        | PCon (left_pat, right_pat) -> Set.union (pat_set left_pat) (pat_set right_pat)
        | PTuple pats ->
          let initial_set = set_empty in
          List.fold_left
            ~init:initial_set
            ~f:(fun acc h -> Set.union acc (pat_set h))
            pats
      in
      let rec binds acc = function
        | EFun (p, next) -> binds (Set.union acc (pat_set p)) next
        | _ -> acc
      in
      let inner = bind_name n @@ helper ine in
      let outer = bind_name n @@ helper e in
      let inner = Set.diff inner @@ binds set_empty e in
      let outer = Set.diff outer @@ binds set_empty e in
      Set.union outer inner
    | ETuple exps ->
      List.fold exps ~init:set_empty ~f:(fun acc h -> Set.union acc (helper h))
    | _ -> failwith "pspspsp"
  in
  helper e
;;

let closure_expr gctx bindings =
  let rec closure_function lts lctx gctx closure_expr = function
    | EFun (p, e) -> EFun (p, closure_function lts lctx gctx closure_expr e)
    | e -> closure_expr lts lctx gctx e
  in
  let rec helper lts lctx gctx = function
    | EConst const -> EConst const
    | EVar (n, _) ->
      (match Map.find lctx n with
       | Some free ->
         let ids = List.map (Set.to_list free) ~f:(fun x -> EVar (x, TUnknown)) in
         List.fold_left ids ~f:(fun f arg -> EApp (f, arg)) ~init:(EVar (n, TUnknown))
       | None -> EVar (n, TUnknown))
    | EFun (p, e) ->
      let unrelated = unrelated (EFun (p, e)) in
      let unrelated = Set.diff unrelated gctx in
      let unrelated_patterns =
        List.map (Set.to_list unrelated) ~f:(fun x -> PVar (x, TUnknown))
      in
      let unrelated_exps =
        List.map (Set.to_list unrelated) ~f:(fun x -> EVar (x, TUnknown))
      in
      let closured_fun = closure_function lts lctx gctx helper (EFun (p, e)) in
      let new_fun =
        List.fold_right ~f:(fun p e -> EFun (p, e)) unrelated_patterns ~init:closured_fun
      in
      List.fold_left unrelated_exps ~f:(fun f arg -> EApp (f, arg)) ~init:new_fun
    | EBinaryOp (op, e1, e2) ->
      EBinaryOp
        ( op
        , helper lts lctx (Set.diff gctx lts) e1
        , helper lts lctx (Set.diff gctx lts) e2 )
    | EApp (e1, e2) ->
      EApp (helper lts lctx (Set.diff gctx lts) e1, helper lts lctx (Set.diff gctx lts) e2)
    | EIfElse (i, t, e) ->
      EIfElse (helper lts lctx gctx i, helper lts lctx gctx t, helper lts lctx gctx e)
    | ELetIn (r, n, e, ein) ->
      (match e with
       | EFun (_, _) ->
         let lts = Set.add lts n in
         let genv =
           match r with
           | Rec -> Set.add gctx n
           | Notrec -> Set.add gctx n
         in
         let unrelated = unrelated (ELetIn (r, n, e, ein)) in
         let unrelated = Set.diff unrelated genv in
         let e = closure_function lts lctx genv helper e in
         let unrelated_global =
           List.map (Set.to_list unrelated) ~f:(fun x -> PVar (x, TUnknown))
         in
         let e = List.fold_right unrelated_global ~f:(fun p e -> EFun (p, e)) ~init:e in
         let local_env = Map.set lctx ~key:n ~data:unrelated in
         let ein = helper lts local_env (Set.add gctx n) ein in
         let e = helper lts local_env (Set.add gctx n) e in
         ELetIn (r, n, e, ein)
       | _ -> ELetIn (r, n, helper lts lctx gctx e, helper lts lctx gctx ein))
    | ETuple exps ->
      let new_exps = List.map exps ~f:(helper lts lctx gctx) in
      ETuple new_exps
    | EList (hd, tl) ->
      let hd = helper lts lctx gctx hd in
      let tl = helper lts lctx gctx tl in
      EList (hd, tl)
    | _ -> failwith ""
  in
  let closure_bindings gctx = function
    | Let (flag, p, e) -> Let (flag, p, closure_function set_empty map_empty gctx helper e)
    | Expression e -> Expression (closure_function set_empty map_empty gctx helper e)
  in
  closure_bindings gctx bindings
;;

let closure bindings =
  let helper acc ctx e = closure_expr ctx e :: acc, ctx in
  let closure_bindings, _ =
    List.fold bindings ~init:([], set_empty) ~f:(fun (acc, ctx) -> helper acc ctx)
  in
  List.rev closure_bindings
;;
