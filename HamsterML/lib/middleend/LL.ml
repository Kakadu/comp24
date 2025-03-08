open Ast
open Utils.R
open Base
module Format = Stdlib.Format

type ll_expr =
  | LLConst of value
  | LLVar of id
  | LLOperation of op
  | LLTuple of ll_expr * ll_expr * ll_expr list
  | LLList of ll_expr list
  | LLListConcat of ll_expr * ll_expr
  | LLConstraint of ll_expr * dataType
  | LLApplication of ll_expr * ll_expr
  | LLLet of funType * ll_bind list * ll_expr option
  | LLIf of ll_expr * ll_expr * ll_expr option
  | LLMatch of ll_expr * ll_case list
[@@deriving show]

and ll_bind = pattern * args * ll_expr
and ll_case = pattern * ll_expr
and ll_prog = ll_expr list

module NameSet = struct
  include Utils.NameSet

  (* (a, b, c) => Var a, Var b, Var c *)
  let to_args (set : t) : args =
    List.rev @@ Set.fold set ~init:[] ~f:(fun acc id -> Var id :: acc)
  ;;

  let rec generate_name (set : t) =
    let* fresh_num = fresh in
    let varname = "ll_arg_" ^ Int.to_string fresh_num in
    match find varname set with
    | None -> return (extend varname set, varname)
    | Some _ -> generate_name set
  ;;
end

module NameEnv = struct
  include Utils.NameEnv

  let rec generate_name (env : t) (name : id) =
    match find name env with
    | None ->
      let* fresh_num = fresh in
      let new_name = "ll_var_" ^ Int.to_string fresh_num in
      (match find new_name env with
       | None -> return (extend (name, new_name) env, new_name)
       | Some _ -> generate_name env name)
    | Some new_name -> return (env, new_name)
  ;;
end

let generate_lambda_name =
  let* fresh_num = fresh in
  return @@ "ll_lam_" ^ Int.to_string fresh_num
;;

(* Simplify weird patterns in function arguments *)
let simplify_arguments p_args expr : (expr * NameSet.t) t =
  let rec helper (acc : expr) (names : NameSet.t) = function
    | [] -> return (acc, names)
    | Var id :: tl -> helper acc (NameSet.extend id names) tl
    | head_p :: tl ->
      let* names, new_name = NameSet.generate_name names in
      (* match <new_name> with head_p -> acc *)
      helper (Match (EVar new_name, [ head_p, acc ])) names tl
  in
  helper expr NameSet.empty p_args
;;

(* 'Let' constructions that should be lifted *)
type lifted_lets = ll_expr list [@@deriving show]

(* add bind names to environment with new names *)
let collect_mutual_names (env : NameEnv.t) (binds : bind list) : NameEnv.t t =
  fold_list binds ~init:env ~f:(fun env bnd ->
    match bnd with
    | Var name, _, _ ->
      let* env, _ = NameEnv.generate_name env name in
      return env
    | _ -> return env)
;;

(* Check if a let binding defines a function or just a value *)
let is_fun_binding (args : args) =
  match args with
  | [] -> false
  | _ -> true
;;

let _if cond a b = if cond then a else b

(* perform lambda lifting for every bind *)
let rec ll_bind
  (lifted : lifted_lets)
  (env : NameEnv.t)
  ((name, args, body) : pattern * args * expr)
  =
  (* main = let () = print_int 10 in 0 *)
  match name with
  | Var name ->
    let* env, new_name = NameEnv.generate_name env name in
    (* ll_var_0 = let () = print_int 10 in 0 *)
    let* body, nameset = simplify_arguments args body in
    let* llbody, lifted = ll_expr lifted env body in
    let (new_bind : ll_bind) = Var new_name, NameSet.to_args nameset, llbody in
    return (env, new_bind, lifted)
  | pattern ->
    let* llbody, lifted = ll_expr lifted env body in
    let (new_bind : ll_bind) = pattern, args, llbody in
    return (env, new_bind, lifted)

(* perform lambda lifting for every expr *)
and ll_expr (lifted : lifted_lets) (env : NameEnv.t) (expr : expr)
  : (ll_expr * lifted_lets) t
  =
  match expr with
  | Fun (args, body) ->
    let* name = generate_lambda_name in
    let* body, nameset = simplify_arguments args body in
    let* llexpr, lifted = ll_expr lifted env body in
    let (lam_bind : ll_bind) = Var name, NameSet.to_args nameset, llexpr in
    let lam_let = LLLet (Nonrecursive, [ lam_bind ], None) in
    return (LLVar name, lam_let :: lifted)
  | Let (rec_flag, binds, Some scope) ->
    (* Work with rec flag *)
    let* env =
      match rec_flag with
      | Recursive -> collect_mutual_names env binds
      | Nonrecursive -> return env
    in
    (* Work with binds (bind1 and bind2 and ...) *)
    let* env, new_binds, lifted, is_fun =
      (* () = print_int 10 *)
      fold_list
        binds
        ~init:(env, [], lifted, true)
        ~f:(fun (env, binds, lifted, is_fun) (name, args, expr) ->
          let* env, new_bind, lifted = ll_bind lifted env (name, args, expr) in
          return (env, new_bind :: binds, lifted, is_fun && is_fun_binding args))
    in
    let* ll_scope, lifted = ll_expr lifted env scope in
    let ll_new_let =
      LLLet (rec_flag, List.rev new_binds, _if is_fun None (Some ll_scope))
    in
    return @@ _if is_fun (ll_scope, ll_new_let :: lifted) (ll_new_let, lifted)
  | EConst v -> return (LLConst v, lifted)
  | EOperation op -> return (LLOperation op, lifted)
  | EVar id ->
    (match NameEnv.find id env with
     | None -> return (LLVar id, lifted)
     | Some new_id -> return (LLVar new_id, lifted))
  | EList exprs ->
    let* ll_exprs, lifted =
      fold_list exprs ~init:([], lifted) ~f:(fun (exprs, lifted) cur_expr ->
        let* ll_cur_expr, lifted = ll_expr lifted env cur_expr in
        return (ll_cur_expr :: exprs, lifted))
    in
    return (LLList ll_exprs, lifted)
  | ETuple (e1, e2, tl) ->
    let* ll_e1, lifted = ll_expr lifted env e1 in
    let* ll_e2, lifted = ll_expr lifted env e2 in
    let* ll_tl, lifted =
      fold_list tl ~init:([], lifted) ~f:(fun (exprs, lifted) cur_expr ->
        let* ll_cur_expr, lifted = ll_expr lifted env cur_expr in
        return (ll_cur_expr :: exprs, lifted))
    in
    return (LLTuple (ll_e1, ll_e2, ll_tl), lifted)
  | EListConcat (lexpr, rexpr) ->
    let* ll_lexpr, lifted = ll_expr lifted env lexpr in
    let* ll_rexpr, lifted = ll_expr lifted env rexpr in
    return (LLListConcat (ll_lexpr, ll_rexpr), lifted)
  | EConstraint (expr, dt) ->
    let* ll_expr, lifted = ll_expr lifted env expr in
    return (LLConstraint (ll_expr, dt), lifted)
  | If (cond, th, Some el) ->
    let* ll_cond, lifted = ll_expr lifted env cond in
    let* ll_th, lifted = ll_expr lifted env th in
    let* ll_else, lifted = ll_expr lifted env el in
    return (LLIf (ll_cond, ll_th, Some ll_else), lifted)
  | If (cond, th, None) ->
    let* ll_cond, lifted = ll_expr lifted env cond in
    let* ll_th, lifted = ll_expr lifted env th in
    return (LLIf (ll_cond, ll_th, None), lifted)
  | Match (e, cases) ->
    let* ll_e, lifted = ll_expr lifted env e in
    let* ll_cases, lifted =
      fold_list cases ~init:([], lifted) ~f:(fun (cases, lifted) case ->
        let pat, expr = case in
        let* ll_expr, lifted = ll_expr lifted env expr in
        return (((pat, ll_expr) : ll_case) :: cases, lifted))
    in
    return (LLMatch (ll_e, ll_cases), lifted)
  | Application (lexpr, rexpr) ->
    let* ll_lexpr, lifted = ll_expr lifted env lexpr in
    let* ll_rexpr, lifted = ll_expr lifted env rexpr in
    return (LLApplication (ll_lexpr, ll_rexpr), lifted)
  | _ -> failwith "Incorrect expression was encountered during LL"
;;

let ll_prog (prog : prog) : ll_prog t =
  let rec helper lifted env = function
    | [] -> return []
    | first_let :: tl_lets ->
      let* ll_first_let, env =
        match first_let with
        | Let (rec_flag, binds, in_scope) ->
          (* Update environment according to rec_flag *)
          let* env =
            match rec_flag with
            | Recursive -> collect_mutual_names env binds
            | Nonrecursive -> return env
          in
          (* Convert all binds *)
          let* env, new_binds, lifted =
            fold_list
              binds
              ~init:(env, [], lifted)
              ~f:(fun (env, binds, lifted) (name, args, expr) ->
                let* env, new_bind, lifted = ll_bind lifted env (name, args, expr) in
                return (env, new_bind :: binds, lifted))
          in
          (* Work with 'in' scope *)
          let new_binds = List.rev new_binds in
          (match in_scope with
           | Some scope ->
             let* ll_scope, lifted = ll_expr lifted env scope in
             return (lifted @ [ LLLet (rec_flag, new_binds, Some ll_scope) ], env)
           | None -> return (lifted @ [ LLLet (rec_flag, new_binds, None) ], env))
        | _ -> failwith "Incorrect starting point was encountered during LL"
      in
      let* ll_tl_lets = helper lifted env tl_lets in
      return (ll_first_let @ ll_tl_lets)
  in
  helper [] NameEnv.empty prog
;;
