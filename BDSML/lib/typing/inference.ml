(** Copyright 2024-2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Types
open Monads
open Parser.Ast

let unnamed_expr = ""
let start_type_var = TVarId.create 0
let fresh_var = fresh >>| fun n -> TVar n

let poli_constructors_names =
  [ "::", "list"; "[]", "list"; "Some", "optional"; "None", "optional" ]
;;

let find_constructor name =
  match List.find_opt (fun (s, _) -> name = s) poli_constructors_names with
  | Some res -> return @@ snd res
  | None -> fail @@ No_variable name
;;

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

module TypeVars = Map.Make (String)

let typexpr_to_type typexpr =
  let base_type_mapper = function
    | "int" -> Some TInt
    | "char" -> Some TChar
    | "string" -> Some TString
    | "bool" -> Some TBool
    | _ -> None
  in
  let rec helper (type_vars : type_val TypeVars.t) = function
    | Type_constructor_param (tye, name) ->
      let+ tye, type_vars = helper type_vars tye in
      TConstructor (Some tye, name), type_vars
    | Type_tuple m ->
      let+ tys, type_vars =
        fold_left
          (fun (acc, type_vars) ty ->
            let+ ty, type_vars = helper type_vars ty in
            ty :: acc, type_vars)
          (return ([], type_vars))
          m
      in
      TTuple (List.rev tys), type_vars
    | Type_single name ->
      (match base_type_mapper name with
       | Some t -> return (TBase t, type_vars)
       | None ->
         (match String.get name 0 with
          | '\'' ->
            (match TypeVars.find_opt name type_vars with
             | None ->
               let+ fv = fresh_var in
               fv, TypeVars.add name fv type_vars
             | Some v -> return (v, type_vars))
          | _ -> return (TConstructor (None, name), type_vars)))
    | Type_fun l ->
      let rec helper2 type_vars = function
        | h :: [] -> (helper type_vars) h
        | h :: tl ->
          let* h, type_vars = helper type_vars h in
          let+ tl, type_vars = helper2 type_vars tl in
          TArrow (h, tl), type_vars
        | _ -> fail (Invalid_ast "fun type without any type")
      in
      helper2 type_vars l
  in
  let+ ty, _ = helper TypeVars.empty typexpr in
  ty
;;

let infer_base_type c =
  let const_to_type = function
    | Const_int _ -> TInt
    | Const_char _ -> TChar
    | Const_string _ -> TString
    | Const_bool _ -> TBool
    | Const_unit -> TUnit
  in
  return (Subst.empty, TBase (const_to_type c))
;;

let rec infer_pattern env = function
  | Pat_any ->
    let+ fv = fresh_var in
    env, Subst.empty, fv
  | Pat_var name ->
    let+ fv = fresh_var in
    let env = TypeEnv.extend env name (Scheme.create VarSet.empty fv) in
    env, Subst.empty, fv
  | Pat_type (pat, tye) ->
    let* env, sub1, ty = infer_pattern env pat in
    let* ty2 = typexpr_to_type tye in
    let* sub2 = Subst.unify ty ty2 in
    let+ sub = Subst.compose sub1 sub2 in
    env, sub, ty
  | Pat_constant base ->
    let+ sub, base_type = infer_base_type base in
    env, sub, base_type
  | Pat_tuple l ->
    let+ env, sub, tys =
      fold_left
        (fun (env, sub, l) el ->
          let* env, sub2, ty = infer_pattern env el in
          let+ sub = Subst.compose sub sub2 in
          env, sub, ty :: l)
        (return (env, Subst.empty, []))
        l
    in
    env, sub, TTuple (List.rev tys)
  | Pat_or (l, r) ->
    let* env, sub1, ty1 = infer_pattern env l in
    let* env, sub2, ty2 = infer_pattern env r in
    let* sub3 = Subst.unify ty1 ty2 in
    let+ sub = Subst.compose_all [ sub1; sub2; sub3 ] in
    env, sub, ty1
  | Pat_construct (name, pat) ->
    (* only constructors with one type var is supported *)
    (match pat with
     | Some x ->
       let* env, s1, ty = infer_pattern env x in
       let* constr_name = find_constructor name in
       if name = "::"
       then (
         match ty with
         | TTuple [ l; r ] ->
           let* sub = Subst.unify (TConstructor (Some l, "list")) r in
           let+ sub = Subst.compose s1 sub in
           env, sub, Subst.apply sub r
         | _ -> fail Invalid_list_constructor_argument)
       else return (env, s1, TConstructor (Some ty, constr_name))
     | None ->
       let+ fv = fresh_var
       and+ constr_name = find_constructor name in
       env, Subst.empty, TConstructor (Some fv, constr_name))
;;

let rec infer_if env cond bthen belse =
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

and infer_fun env args exp =
  let* env, sub1, args =
    fold_left
      (fun (env, sub, targs) arg ->
        let* env, sub2, ty = infer_pattern env arg in
        let+ sub = Subst.compose sub sub2 in
        env, sub, ty :: targs)
      (return (env, Subst.empty, []))
      args
  in
  let* sub2, ty = infer_expression env exp in
  let+ sub = Subst.compose sub1 sub2 in
  ( sub
  , List.fold_right
      (fun fv ty -> TArrow (Subst.apply sub fv, Subst.apply sub ty))
      (List.rev args)
      ty )

and infer_let env rec_flag bindings =
  match rec_flag with
  | Nonrecursive ->
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
        | Pat_binding (pat, exp) ->
          let* env, sub2, ty = infer_pattern env pat in
          let* sub3, ty2 = infer_expression env exp in
          let* sub4 = Subst.unify ty ty2 in
          let+ sub = Subst.compose_all [ sub; sub2; sub3; sub4 ] in
          TypeEnv.apply sub env, sub)
      (return (env, Subst.empty))
      bindings
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

and infer_typexpr env exp typexpr =
  let* s1, t1 = infer_expression env exp in
  let* t2 = typexpr_to_type typexpr in
  let* s2 = Subst.unify t1 t2 in
  let+ sub = Subst.compose s1 s2 in
  sub, t2

and infer_construct env name = function
  | Some e ->
    let* s1, ty = infer_expression env e in
    let* constr_name = find_constructor name in
    if name = "::"
    then (
      match ty with
      | TTuple [ l; r ] ->
        let* sub = Subst.unify (TConstructor (Some l, "list")) r in
        let+ sub = Subst.compose s1 sub in
        sub, Subst.apply sub r
      | _ -> fail Invalid_list_constructor_argument)
    else return (s1, TConstructor (Some ty, constr_name))
  | None ->
    let+ fv = fresh_var
    and+ constr_name = find_constructor name in
    Subst.empty, TConstructor (Some fv, constr_name)

and infer_cases env cases =
  let* caseslist =
    map
      (fun case ->
        let* env, sub1, ty1 = infer_pattern env case.left in
        let* sub2, ty2 = infer_expression env case.right in
        let+ sub = Subst.compose sub1 sub2 in
        sub, ty1, ty2)
      cases
  in
  let+ sub, ty1, ty2 =
    fold_left
      (fun (sub1, ty11, ty21) (sub2, ty12, ty22) ->
        let* sub3 = Subst.unify ty11 ty12 in
        let* sub4 = Subst.unify ty21 ty22 in
        let+ sub = Subst.compose_all [ sub1; sub2; sub3; sub4 ] in
        sub, ty11, ty21)
      (return (List.hd caseslist))
      caseslist
  in
  sub, Subst.apply sub ty1, Subst.apply sub ty2

and infer_match env exp cases =
  let* sub, ty = infer_expression env exp in
  let* sub2, ty1, ty2 = infer_cases env cases in
  let* sub3 = Subst.unify ty ty1 in
  let+ sub = Subst.compose_all [ sub; sub2; sub3 ] in
  sub, ty2

and infer_function env cases =
  let* fv = fresh_var in
  let* sub, ty1, ty2 = infer_cases env cases in
  let* sub2 = Subst.unify fv ty1 in
  let+ sub = Subst.compose sub sub2 in
  sub, TArrow (Subst.apply sub fv, ty2)

and infer_expression (env : TypeEnv.t) : expression -> (Subst.t * type_val) t = function
  | Exp_constant c -> infer_base_type c
  | Exp_if (cond, bthen, belse) -> infer_if env cond bthen belse
  | Exp_ident var -> lookup_env env var
  | Exp_fun (vars, exp) -> infer_fun env vars exp
  | Exp_function cases -> infer_function env cases
  | Exp_let (rec_flag, bindings, expression) ->
    let* env, sub = infer_let env rec_flag bindings in
    let* sub2, ty = infer_expression env expression in
    let+ sub = Subst.compose sub sub2 in
    sub, ty
  | Exp_apply (l, r) -> infer_apply env l r
  | Exp_match (exp, cases) -> infer_match env exp cases
  | Exp_tuple l -> infer_tuple env l
  | Exp_construct (name, exp) -> infer_construct env name exp
  | Exp_type (exp, typexp) -> infer_typexpr env exp typexp
;;

let predefine_operators =
  [ "( + )", "int -> int -> int"
  ; "( - )", "int -> int -> int"
  ; "( * )", "int -> int -> int"
  ; "( / )", "int -> int -> int"
  ; "( ~- )", "int -> int"
  ; "( ~+ )", "int -> int"
  ; "not", "bool -> bool"
  ; "( > )", "'a -> 'a -> bool"
  ; "( >= )", "'a -> 'a -> bool"
  ; "( < )", "'a -> 'a -> bool"
  ; "( <= )", "'a -> 'a -> bool"
  ; "( = )", "'a -> 'a -> bool"
  ; "( <> )", "'a -> 'a -> bool"
  ; "( || )", "bool -> bool -> bool"
  ]
;;

let init_env =
  let+ add_predefines =
    let empty_env = TypeEnv.empty in
    map
      (fun (name, ty) ->
        match Parser.Typexpr_parser.parse_typexpr_str ty with
        | Result.Ok ty ->
          let+ ty = typexpr_to_type ty in
          let scheme = generalize empty_env ty in
          name, scheme
        | Result.Error s -> fail (Invalid_predefined_operators s))
      predefine_operators
  in
  TypeEnv.init add_predefines
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
       | Str_value (rec_flag, bindings) ->
         let* new_env, _ = infer_let env rec_flag bindings in
         let vars_types =
           List.map (fun (var, sch) -> var, Scheme.get_type sch)
           @@ TypeEnv.to_list
           @@ TypeEnv.diff new_env env
         in
         let types = types @ vars_types in
         helper new_env tl types)
  in
  let* env = init_env in
  helper env prog []
;;

let error_to_string = function
  | Occurs_check -> "occurs check"
  | Unification_failed (val1, val2) ->
    "failed unification of types " ^ show_type_val val1 ^ " and " ^ show_type_val val2
  | No_variable v -> "variable " ^ v ^ " is not found"
  | Invalid_let -> "only variables are allowed as left-hand side of `let rec'"
  | Invalid_list_constructor_argument -> "invalid list constructor argument"
  | Invalid_ast s -> "invalid ast part: " ^ s
  | Invalid_predefined_operators s -> "invalid predefined operators: " ^ s
;;

let infer_program prog =
  match run (infer_structure_item prog) start_type_var with
  | Ok _ as x -> x
  | Error e -> Error ("Type infering error: " ^ error_to_string e)
;;
