open Types
open Monads
open Helpers
open Parser.Ast

let unnamed_expr = ""
let start_type_var = TVarId.create 0
let fresh_var = fresh >>| fun n -> TVar n
let init_env = TypeEnv.empty

let instantiate (scheme : Scheme.t) : type_val t =
  let vars, ty = scheme in
  VarSet.fold
    (fun var ty ->
      let* ty = ty in
      let* fv = fresh_var in
      let+ sub = Subst.singleton var fv in
      Subst.apply sub ty)
    vars
    (return ty)
;;

let lookup_env env var =
  match TypeEnv.find var env with
  | None -> fail (No_variable var)
  | Some scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans)
;;

let generalize env ty =
  let free = VarSet.diff (free_vars ty) (TypeEnv.free_vars env) in
  Scheme.create free ty
;;

let rec infer_base_type c =
  let const_to_type = function
    | Const_int _ -> TInt
    | Const_char _ -> TChar
    | Const_string _ -> TString
    | Const_bool _ -> TBool
  in
  return (Subst.empty, TBase (const_to_type c))

and infer_if env cond bthen belse =
  let bool_type = TBase TBool in
  let* s1, t1 = infer_expression env cond in
  let* s2, t2 = infer_expression env bthen in
  let* s3 = Subst.unify t1 bool_type in
  let* else_branch_subs, res_type =
    match belse with
    | Some exp ->
      let* s4, t3 = infer_expression env exp in
      let+ s5 = Subst.unify t2 t3 in
      [ s4; s5 ], Subst.apply s5 t2
    | None -> return ([], t2)
  in
  let+ united_sub = Subst.compose_all @@ else_branch_subs @ [ s3; s2; s1 ] in
  united_sub, res_type

and infer_fun env vars exp =
  let rec helper env = function
    | var :: tl ->
      let* fv = fresh_var in
      let env =
        match var with
        | Pat_var name -> TypeEnv.extend env name (Scheme.create VarSet.empty fv)
        | _ -> env
      in
      let+ env, vars = helper env tl in
      env, fv :: vars
    | _ -> return (env, [])
  in
  let* env, fvs = helper env vars in
  let+ sub, ty = infer_expression env exp in
  sub, List.fold_right (fun fv ty -> TArrow (Subst.apply sub fv, ty)) fvs ty

and infer_let env rec_flag bindings expression =
  match rec_flag with
  | Nonrecursive ->
    let* env, sub1 =
      fold_left
        (fun (env, sub) bind ->
          match bind with
          | Val_binding (var, args, exp) ->
            let* s1, t1 = infer_fun env args exp in
            let env = TypeEnv.apply s1 env in
            let sheme = generalize env t1 in
            let env = TypeEnv.extend env var sheme in
            let+ sub = Subst.compose sub s1 in
            env, sub
          | _ -> raise @@ Unimplemented "infer_let")
        (return (env, Subst.empty))
        bindings
    in
    let* sub2, ty = infer_expression env expression in
    let+ sub = Subst.compose sub1 sub2 in
    sub, ty
  | Recursive ->
    let* bind_data, env =
      fold_left
        (fun (data, env) bind ->
          match bind with
          | Val_binding (var, args, exp) ->
            let+ fv = fresh_var in
            let env = TypeEnv.extend env var (Scheme.create VarSet.empty fv) in
            (fv, var, args, exp) :: data, env
          | _ -> fail Invalid_let)
        (return ([], env))
        bindings
    in
    let* env, sub1 =
      fold_left
        (fun (env, sub) (fv, var, args, exp) ->
          let* s1, t1 = infer_fun env args exp in
          let* s2 = Subst.unify (Subst.apply s1 fv) t1 in
          let* s = Subst.compose s2 s1 in
          let env = TypeEnv.apply s env in
          let sheme = generalize env (Subst.apply s t1) in
          let env = TypeEnv.extend env var sheme in
          let+ sub = Subst.compose sub s1 in
          env, sub)
        (return (env, Subst.empty))
        bind_data
    in
    let* sub2, ty = infer_expression env expression in
    let+ sub = Subst.compose sub1 sub2 in
    sub, ty

and infer_apply env left right =
  let* s1, t1 = infer_expression env left in
  let* s2, t2 = infer_expression (TypeEnv.apply s1 env) right in
  let* fv = fresh_var in
  let* s3 = Subst.unify (Subst.apply s2 t1) (TArrow (t2, fv)) in
  let ty = Subst.apply s3 fv in
  let+ sub = Subst.compose_all [ s3; s2; s1 ] in
  sub, ty

and infer_tuple env l =
  let+ sub, tys =
    fold_left
      (fun (sub, tys) exp ->
        let* sub1, ty1 = infer_expression env exp in
        let+ sub = Subst.compose sub1 sub in
        sub, ty1 :: tys)
      (return (Subst.empty, []))
      l
  in
  sub, TTuple (List.rev tys)

and infer_expression env expr =
  let rec helper env = function
    | Exp_constant c -> infer_base_type c
    | Exp_if (cond, bthen, belse) -> infer_if env cond bthen belse
    | Exp_ident var -> lookup_env env var
    | Exp_fun (vars, exp) -> infer_fun env vars exp
    | Exp_let (rec_flag, bindings, expression) ->
      infer_let env rec_flag bindings expression
    | Exp_apply (l, r) -> infer_apply env l r
    | Exp_tuple l -> infer_tuple env l
    | _ as t -> raise (Unimplemented (show_expression t ^ "infer_expr"))
  in
  helper env expr
;;

let infer_structure_item prog =
  let rec helper env prog types =
    match prog with
    | [] -> return types
    | h :: tl ->
      (match h with
       | Str_eval e ->
         let* _, t = infer_expression env e in
         let types = types @ [ unnamed_expr, t ] in
         helper env tl types
       | Str_value _ -> raise (Unimplemented "str_value infer"))
  in
  helper init_env prog []
;;

let error_to_string = function
  | Occurs_check -> "occurs check"
  | Unification_failed (val1, val2) ->
    "failed unification of types " ^ show_type_val val1 ^ " and " ^ show_type_val val2
  | No_variable v -> "variable " ^ v ^ " is not found"
  | Invalid_let -> "Only variables are allowed as left-hand side of `let rec'"
;;

let infer_program prog =
  match run (infer_structure_item prog) start_type_var with
  | Ok _ as x -> x
  | Error e -> Error ("Type infering error: " ^ error_to_string e)
;;
