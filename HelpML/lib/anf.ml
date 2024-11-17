open RestrictedAst
open Llast
open Ast
(* open AnfPrinter *)
open Counter
open StdlibFunc
open IState
open IState.Syntax

let conv_const = function
  | CBool b -> ImmBool b
  | CInt i -> ImmNum i
  | CUnit -> ImmUnit
;;

let conv_pattern = function
  | PWild -> PImmWild
  | PVar id -> PImmExpr (ImmId id)
  | PConst c -> PImmExpr (conv_const c)
;;

let rec gen_var base global =
  let* fresh_id = fresh_name_int in
  let new_id = base ^ Int.to_string fresh_id in
  let is_in = Base.Set.mem global new_id in
  if is_in then gen_var base global else return new_id
;;

let rec anf_expr
  (env : (string, 'a) Base.Set.t)
  (e : llexpr)
  (expr_with_imm_hole : immexpr -> aexpr t)
  : aexpr t
  =
  match e with
  | LLConst c -> expr_with_imm_hole @@ conv_const c
  | LLVar id -> expr_with_imm_hole @@ ImmId id
  | LLBinOp (op, left, right) ->
    anf_expr env left (fun limm ->
      anf_expr env right (fun rimm ->
        let* var = gen_var "b_op_" env in
        expr_with_imm_hole @@ ImmId var
        >>= fun ae -> return (ALetIn (var, CBinOp (op, limm, rimm), ae))))
  | LLIf (cond, t, e) ->
    anf_expr env cond (fun cimm ->
      let* taexpr = anf_expr env t (fun timm -> return @@ ACExpr (CImmExpr timm)) in
      let* eaexpr = anf_expr env e (fun eimm -> return @@ ACExpr (CImmExpr eimm)) in
      let* var = gen_var "if_" env in
      expr_with_imm_hole @@ ImmId var
      >>= fun ae -> return (ALetIn (var, CIf (cimm, taexpr, eaexpr), ae)))
  | LLApp (f, arg) ->
    anf_expr env f (fun fimm ->
      anf_expr env arg (fun argimm ->
        let* var = gen_var "app_" env in
        expr_with_imm_hole @@ ImmId var
        >>= fun ae -> return (ALetIn (var, CApp (fimm, argimm), ae))))
  | LLLetIn (varname, e1, e2) ->
    let new_env = Base.Set.add env varname in
    anf_expr new_env e1 (fun immval ->
      anf_expr new_env e2 expr_with_imm_hole
      >>= fun body -> return (ALetIn (varname, CImmExpr immval, body)))
;;

let anf_binding env = function
  | LLLet (r, varname, args, e) ->
    let rec get_initial_env args acc =
      match args with
      | [] -> acc
      | PVar id :: tl -> Base.Set.add acc id |> get_initial_env tl
      | _ :: tl -> get_initial_env tl acc
    in
    anf_expr (get_initial_env args env) e (fun ie -> return (ACExpr (CImmExpr ie)))
    >>= fun anf_e -> return (ALet (r, varname, List.map conv_pattern args, anf_e))
;;

let anf_program (binds : llbinding list) =
  let rec get_initial_env binds acc =
    match binds with
    | [] -> acc
    | LLLet (_, varname, _, _) :: tl -> Base.Set.add acc varname |> get_initial_env tl
  in
  let env = get_initial_env binds (Base.Set.empty (module Base.String)) in
  let env = List.fold_left (fun acc (id, _) -> Base.Set.add acc id) env stdlib in
  List.map (fun bind -> snd @@ IState.runState ~init:0 (anf_binding env bind)) binds
;;

(* let print_anf_prog llbinds =
  let res = anf_program llbinds in
  Format.printf "%a" pp_prexpr res
;;

let print_anf_expr llexpr =
  let res =
    snd
    @@ IState.runState
         ~init:0
         (anf_expr
            (Base.Set.empty (module Base.String))
            llexpr
            (fun ie -> return (ACExpr (CImmExpr ie))))
  in
  Format.printf "%a" pp_aexpr res
;; *)