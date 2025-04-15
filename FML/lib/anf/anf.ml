open Anf_ast
open Me_ast
open Common
open StateMonad


let const_to_immexp = function
  | Me_Cint i -> ImmInt i
  | Me_CBool b -> ImmBool b
;;

let rec collect_apps expr =
  match expr with
  | Me_EApp (f, arg) ->
      let f', args = collect_apps f in
      (f', args @ [arg])
  | _ -> (expr, [])

let rec to_cexp: me_expr -> ((string * cexpr) list * cexpr) t = function
  | Me_ETuple elems ->
    let* binds, imm_elems =
      RList.fold_left elems ~init:(return ([], [])) ~f:(fun (acc_binds, acc_elems) el ->
        let* binds_el, imm_el = check_hard_expr el in
        return (acc_binds @ binds_el, acc_elems @ [imm_el]))
    in
    return (binds, CImmExpr (ImmTuple imm_elems))

  | Me_ELet (NoRec, name, e1, e2) ->
    let* binds1, ce1 = to_cexp e1 in
    let* binds2, ce2 = to_cexp e2 in
    return (binds1 @ [ name, ce1 ] @ binds2, ce2)

  | Me_ECons (e1, e2) ->
    let* binds1, a1 = check_hard_expr e1 in
    let* binds2, a2 = check_hard_expr e2 in
    return (binds1 @ binds2, CECons (a1, a2))

  | Me_EIf (e1, e2, e3) ->
    let* binds1, e1 = check_hard_expr e1 in
    let handle_for_let (e: me_expr) =
    let* binds, expr = to_cexp e in
      RList.fold_right
      binds
      ~init:(return @@ ACExpr(expr))
      ~f:(fun (name, cexp) acc -> return @@ ALetIn(name, cexp, acc)) in

    let* ce2 = handle_for_let e2 in
    let* ce3 = handle_for_let e3 in
    
    return (binds1, CEIf(e1, ce2, ce3))

  | Me_EApp (e1, e2) ->
    let f_expr, args = collect_apps (Me_EApp (e1, e2)) in
    let* binds_f, fun_imm = check_hard_expr f_expr in
    let* binds_args, imm_args =
      RList.fold_left args ~init:(return ([], [])) ~f:(fun (acc_binds, acc_args) arg ->
        let* binds_arg, imm_arg = check_hard_expr arg in
        return (acc_binds @ binds_arg, acc_args @ [imm_arg]))
    in
    (match fun_imm with
    | ImmIdentifier f_name ->
        return (binds_f @ binds_args, CEApply (f_name, imm_args))
    | _ -> failwith "Expected function to be an identifier after all")


  | Me_EConst c -> return ([], CImmExpr (const_to_immexp c))
  | Me_EIdentifier v -> return ([], CImmExpr (ImmIdentifier v))
  | Me_EUnit -> return ([], CImmExpr ImmUnit)
  | Me_ENill -> return ([], CImmExpr ImmNill)
  | _ -> failwith "See you later space cowboy"

(* для обработки сложных выражений в условиях *)
and check_hard_expr  e = match e with
  | Me_EIdentifier v -> return ([], ImmIdentifier v)
  | Me_EConst c -> return ([], const_to_immexp c)
  | Me_EUnit -> return ([], ImmUnit)
  | Me_ENill -> return ([], ImmNill)
    | e -> 
      let* id = fresh in
      let name = "anf" ^ Int.to_string id in
      let* binds1, ce = to_cexp e in
      return (binds1 @ [ name, ce ], (ImmIdentifier name))
;;

let anf_decl =
  let handle_for_let (e: me_expr) =
    let* binds, expr = to_cexp e in
    RList.fold_right
      binds
      ~init:(return @@ ACExpr expr)
      ~f:(fun (name, cexp) acc -> return @@ ALetIn(name, cexp, acc))
  in
  let is_based_value = function
  | Me_EConst _ | Me_EIdentifier _ | Me_EUnit | Me_ENill -> true
  | _ -> false in
  function
  | Me_Nonrec decls ->
    RList.fold_left decls ~init:(return []) ~f:(fun acc (name, e) ->
      match e with
      | Me_EFun (args, body) ->
        let* body' = handle_for_let body in
        return (acc @ [ADNoRec [ALet (name, args, body')]])

      | _ when is_based_value e ->
        let* body' = handle_for_let e in
        return (acc @ [Based_value (name, body')])

      | _ ->
        let* expr' = handle_for_let e in
        return (acc @ [ADNoRec [ALet (name, [], expr')]])
    )

  | Me_Rec decls ->
    let* bindings =
      RList.fold_left decls ~init:(return []) ~f:(fun acc (name, e) ->
        let* new_e =
          match e with
          | Me_EFun (args, body) ->
            let* body' = handle_for_let body in
            return (ALet (name, args, body'))
          | _ ->
            let* expr' = handle_for_let e in
            return (ALet (name, [], expr'))
        in
        return (acc @ [new_e])
      )
    in
    return [ADREC bindings]
  ;;

let anf prog =
  StateMonad.run
    (RList.fold_left prog ~init:(return []) ~f:(fun acc decl ->
       let* d = anf_decl decl in
       return (acc @ d)))
;;
