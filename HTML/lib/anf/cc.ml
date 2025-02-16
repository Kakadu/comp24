open AstLib.Ast

module StringSet = struct
  type t = (string, Base.String.comparator_witness) Base.Set.t

  let empty = Base.Set.empty (module Base.String)
  let add = Base.Set.add
  let remove = Base.Set.remove
  let diff = Base.Set.diff
  let singleton = Base.Set.add empty
  let contains set key = Base.Set.mem set key
  let union = Base.Set.union
  let union_all = Base.Set.union_list (module Base.String)
  let elements = Base.Set.to_list
  let from_list = Base.Set.of_list (module Base.String)
  let is_empty = Base.Set.is_empty
end

(* todo WriterT? *)
module CounterWriterMonad = struct
  type 'a cc = int -> 'a * int * decl list

  let return (x : 'a) : 'a cc = fun s -> x, s, []

  let bind (m : 'a cc) (f : 'a -> 'b cc) : 'b cc =
    fun s ->
    let x, s', d1 = m s in
    let y, s'', d2 = f x s' in
    y, s'', d1 @ d2
  ;;

  let ( >>= ) = bind
  let ( let* ) = bind

  let fresh_name (prefix : string) : string cc =
    fun s -> prefix ^ string_of_int s, s + 1, []
  ;;

  let tell (d : decl) : unit cc = fun s -> (), s, [ d ]
end

open CounterWriterMonad

let ident_to_string (id : ident) : string =
  match id with
  | IdentOfDefinable (IdentLetters s) -> s
  | IdentOfDefinable (IdentOp s) -> s
  | IdentOfBaseOp _ -> failwith "base operator is not a variable"
;;

let rec bound_vars_pattern ((pat, _) : pattern typed) : string list =
  match pat with
  | PId s -> [ s ]
  | PTuple (p1, p2, ps) ->
    bound_vars_pattern p1
    @ bound_vars_pattern p2
    @ List.concat (List.map bound_vars_pattern ps)
  | PList (p1, p2) -> bound_vars_pattern p1 @ bound_vars_pattern p2
  | PConst _ -> []
;;

let pattern_to_string ((pat, ty) : pattern_or_op typed) : string list =
  match pat with
  | POpPat p -> bound_vars_pattern (p, ty)
  | POpOp s -> [ s ]
;;

let rec free_vars_expr (global_env : StringSet.t) ((e, _) : expr typed) : StringSet.t =
  match e with
  | EConst _ -> StringSet.empty
  | EId id ->
    (match id with
     | (IdentOfDefinable (IdentLetters s) | IdentOfDefinable (IdentOp s))
       when (not @@ String.starts_with ~prefix:"fun-" s)
            && (not @@ StringSet.contains global_env s) ->
       StringSet.singleton (ident_to_string id)
     | _ -> StringSet.empty)
  | EFun (pat, body) ->
    let bound =
      List.fold_left (fun s x -> StringSet.add s x) global_env (bound_vars_pattern pat)
    in
    free_vars_expr bound body
  | EApp (e1, e2) ->
    StringSet.union (free_vars_expr global_env e1) (free_vars_expr global_env e2)
  | EIf (e1, e2, e3) ->
    let fv_e1 = free_vars_expr global_env e1 in
    let fv_e2 = free_vars_expr global_env e2 in
    let fv_e3 = free_vars_expr global_env e3 in
    StringSet.union_all [ fv_e1; fv_e2; fv_e3 ]
  | EList (e1, e2) ->
    StringSet.union (free_vars_expr global_env e1) (free_vars_expr global_env e2)
  | ETuple (e1, e2, es) ->
    List.fold_left
      (fun acc et -> StringSet.union acc (free_vars_expr global_env et))
      (StringSet.union (free_vars_expr global_env e1) (free_vars_expr global_env e2))
      es
  | EClsr (decl, e) ->
    let bound_in_decl, fv_decl = free_vars_decl global_env decl in
    let fv_e = free_vars_expr global_env e in
    StringSet.union fv_decl (StringSet.diff fv_e bound_in_decl)
  | EMatch (e, br, brs) ->
    let brs = br :: brs in
    let fv_e = free_vars_expr global_env e in
    let free_vars_branch ((pat, expr) : branch) : StringSet.t =
      let bound =
        List.fold_left (fun s x -> StringSet.add s x) global_env (bound_vars_pattern pat)
      in
      StringSet.diff (free_vars_expr global_env expr) bound
    in
    let fv_brs =
      List.fold_left
        (fun acc b -> StringSet.union acc (free_vars_branch b))
        StringSet.empty
        brs
    in
    StringSet.union fv_e fv_brs

and free_vars_decl env (d : decl) : StringSet.t * StringSet.t =
  match d with
  | DLet (_, (pat_or_op, expr)) ->
    let bound =
      match pat_or_op with
      | POpPat (PId s), _ -> StringSet.singleton s
      | _ -> StringSet.empty
    in
    bound, free_vars_expr (StringSet.union bound env) expr
  (* todo no test for this, praying it works 🙏🙏🙏*)
  | DLetMut (_, lb, lb2, lbs) ->
    let lbs = lb :: lb2 :: lbs in
    let all_pats =
      List.filter_map
        (fun ((pat_or_op, _), _) ->
          match pat_or_op with
          | POpPat (PId s) -> Some s
          | _ -> None)
        lbs
    in
    let bound = List.fold_left StringSet.add StringSet.empty all_pats in
    let free_in_lb (_, e) = free_vars_expr env e in
    let free_all =
      List.fold_left (fun s lb -> StringSet.union s (free_in_lb lb)) StringSet.empty lbs
    in
    bound, free_all
;;

let pattern_of_free_vars (fv : string list) : pattern typed =
  match fv with
  | [] -> PConst CUnit, None
  | [ x ] -> PId x, None
  | x :: y :: rest ->
    let p1 = PId x, None in
    let p2 = PId y, None in
    let rest_pats = List.map (fun v -> PId v, None) rest in
    PTuple (p1, p2, rest_pats), None
;;

let rec get_efun_args_body local_env = function
  | EFun (pat, e), _ -> get_efun_args_body (bound_vars_pattern pat @ local_env) e
  | expr -> local_env, expr
;;

let rec replace_fun_body body = function
  | EFun (pat, e), ty -> EFun (pat, replace_fun_body body e), ty
  | _ -> body
;;

let rec subst_eid ((e, t) : expr typed) (subst : (string * expr typed) list) : expr typed =
  match e with
  | EConst _ -> e, t
  | EId id ->
    (match id with
     | IdentOfDefinable _ ->
       let name = ident_to_string id in
       (try List.assoc name subst with
        | Not_found -> e, t)
     | IdentOfBaseOp _ -> e, t)
  | EFun (pat, body) ->
    let bound = bound_vars_pattern pat in
    let subst' = List.filter (fun (x, _) -> not (List.mem x bound)) subst in
    EFun (pat, subst_eid body subst'), t
  | EApp (e1, e2) -> EApp (subst_eid e1 subst, subst_eid e2 subst), t
  | EIf (e1, e2, e3) ->
    EIf (subst_eid e1 subst, subst_eid e2 subst, subst_eid e3 subst), t
  | EList (e1, e2) -> EList (subst_eid e1 subst, subst_eid e2 subst), t
  | ETuple (e1, e2, es) ->
    ( ETuple
        (subst_eid e1 subst, subst_eid e2 subst, List.map (fun e -> subst_eid e subst) es)
    , t )
  | EClsr (decl, e) -> EClsr (substitute_decl decl subst, subst_eid e subst), t
  | EMatch (e, br, brs) ->
    let substitute_branch ((pat, expr) : branch) (subst : (string * expr typed) list)
      : branch
      =
      let bound = bound_vars_pattern pat in
      let subst' = List.filter (fun (x, _) -> not (List.mem x bound)) subst in
      pat, subst_eid expr subst'
    in
    ( EMatch
        ( subst_eid e subst
        , substitute_branch br subst
        , List.map (fun b -> substitute_branch b subst) brs )
    , t )

and substitute_decl (d : decl) (subst : (string * expr typed) list) : decl =
  match d with
  | DLet (rf, (pat_or_op, expr)) ->
    let bound =
      match pat_or_op with
      | POpPat (PId s), _ -> [ s ]
      | _ -> []
    in
    let subst' = List.filter (fun (x, _) -> not (List.mem x bound)) subst in
    DLet (rf, (pat_or_op, subst_eid expr subst'))
  | _ -> failwith "todo pohui"
;;

let rec closure_convert_expr
  (global_env : StringSet.t)
  ((e, t) : expr typed)
  (rec_name : string option)
  : expr typed cc
  =
  match e with
  | EConst _ | EId _ -> return (e, t)
  | EFun (_, _) as efun ->
    let local_env, body = get_efun_args_body [] (efun, t) in
    let new_env = StringSet.union global_env (StringSet.from_list local_env) in
    let* body' = closure_convert_expr global_env body None in
    let fv = free_vars_expr new_env body' in
    let fv = StringSet.elements fv in
    let* f_name = fresh_name "fun-" in
    let body'' =
      match rec_name with
      | Some rec_name ->
        subst_eid body [ rec_name, (EId (IdentOfDefinable (IdentLetters f_name)), None) ]
      | None -> body'
    in
    let new_fun =
      List.fold_left (fun acc x -> EFun ((PId x, None), acc), None) body'' (local_env @ fv)
    in
    let rf = if rec_name = None then Not_recursive else Recursive in
    let decl = DLet (rf, ((POpPat (PId f_name), None), new_fun)) in
    let* () = tell decl in
    let new_fun_id = EId (IdentOfDefinable (IdentLetters f_name)), None in
    let applied_fun =
      List.fold_right
        (fun x acc -> EApp (acc, (EId (IdentOfDefinable (IdentLetters x)), None)), None)
        fv
        new_fun_id
    in
    return @@ applied_fun
  | EApp (e1, e2) ->
    let* ce1 = closure_convert_expr global_env e1 rec_name in
    let* ce2 = closure_convert_expr global_env e2 rec_name in
    return (EApp (ce1, ce2), t)
  | EIf (e1, e2, e3) ->
    let* ce1 = closure_convert_expr global_env e1 rec_name in
    let* ce2 = closure_convert_expr global_env e2 rec_name in
    let* ce3 = closure_convert_expr global_env e3 rec_name in
    return (EIf (ce1, ce2, ce3), t)
  | EList (e1, e2) ->
    let* ce1 = closure_convert_expr global_env e1 rec_name in
    let* ce2 = closure_convert_expr global_env e2 rec_name in
    return (EList (ce1, ce2), t)
  | ETuple (e1, e2, es) ->
    let* ce1 = closure_convert_expr global_env e1 rec_name in
    let* ce2 = closure_convert_expr global_env e2 rec_name in
    let rec convert_list = function
      | [] -> return []
      | x :: xs ->
        let* cx = closure_convert_expr global_env x rec_name in
        let* cxs = convert_list xs in
        return (cx :: cxs)
    in
    let* ces = convert_list es in
    return (ETuple (ce1, ce2, ces), t)
  | EClsr (decl, e) ->
    let* cdecl = closure_convert_let_in global_env decl in
    let* ce = closure_convert_expr global_env e rec_name in
    return (EClsr (cdecl, ce), t)
  | EMatch (e, br, brs) ->
    (* todo proper env *)
    let closure_convert_branch env (br : branch) : branch cc =
      let pat, expr = br in
      let* cexpr = closure_convert_expr env expr rec_name in
      return (pat, cexpr)
    in
    let* ce = closure_convert_expr global_env e rec_name in
    let* cbr = closure_convert_branch global_env br in
    let rec conv_branches env = function
      | [] -> return []
      | x :: xs ->
        let* cx = closure_convert_branch env x in
        let* cxs = conv_branches env xs in
        return (cx :: cxs)
    in
    let* cbrs = conv_branches global_env brs in
    return (EMatch (ce, cbr, cbrs), t)

(* ugly edge case *)
(* THINK ABOUT OTHER RECURSIVE CASES TODO *)
and closure_convert_let_in global_env (d : decl) : decl cc =
  match d with
  | DLet (rf, (pat_or_op, expr)) ->
    let global_env =
      StringSet.union global_env
      @@ StringSet.from_list
      @@ if rf = Recursive then pattern_to_string pat_or_op else []
    in
    let* cexpr =
      closure_convert_expr
        global_env
        expr
        (if rf = Recursive then Some (List.hd (pattern_to_string pat_or_op)) else None)
    in
    return (DLet (Not_recursive, (pat_or_op, cexpr)))
  | _ -> failwith "todo pohui"

and common_convert_decl (global_env : StringSet.t) (rf, (pat_or_op, expr))
  : (StringSet.t * let_body) cc
  =
  let _, body = get_efun_args_body (StringSet.elements global_env) expr in
  (* horrible todo*)
  let global_env =
    StringSet.union global_env
    @@ StringSet.from_list
    @@ if rf = Recursive then pattern_to_string pat_or_op else []
  in
  let* body' = closure_convert_expr global_env body None in
  let global_env =
    StringSet.union global_env (StringSet.from_list (pattern_to_string pat_or_op))
  in
  return (global_env, (pat_or_op, replace_fun_body body' expr))

and closure_convert_decl (global_env : StringSet.t) (d : decl) : (StringSet.t * decl) cc =
  match d with
  | DLet (rf, (pat_or_op, expr)) ->
    let* global_env, lb = common_convert_decl global_env (rf, (pat_or_op, expr)) in
    return (global_env, DLet (rf, lb))
  | DLetMut (rf, lb, lb2, lbs) ->
    let* global_env, clb = common_convert_decl global_env (Recursive, lb) in
    let* global_env, clb2 = common_convert_decl global_env (Recursive, lb2) in
    let rec conv lbs global_env =
      match lbs with
      | [] -> return []
      | lb :: xs ->
        let* global_env, clb = common_convert_decl global_env (Recursive, lb) in
        let* clbs = conv xs global_env in
        return (clb :: clbs)
    in
    let* clbs = conv lbs global_env in
    return (global_env, DLetMut (rf, clb, clb2, clbs))
;;

let closure_convert_decl_list (global_env : StringSet.t) (decls : decl list)
  : decl list cc
  =
  let rec helper global_env decls =
    match decls with
    | [] -> return []
    | d :: ds ->
      let* global_env, cd = closure_convert_decl global_env d in
      let* cds = helper global_env ds in
      return (cd :: cds)
  in
  helper global_env decls
;;

let closure_convert (prog : decl list) : decl list =
  let global_env = StringSet.from_list Common.Stdlib.stdlib in
  let decls, _, extra = closure_convert_decl_list global_env prog 0 in
  extra @ decls
;;
