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

let rec bound_vars_pattern ((pat, _) : pattern_typed) : string list =
  match pat with
  | PId s -> [ s ]
  | PTuple (p1, p2, ps) ->
    bound_vars_pattern p1
    @ bound_vars_pattern p2
    @ List.concat (List.map bound_vars_pattern ps)
  | PList (p1, p2) -> bound_vars_pattern p1 @ bound_vars_pattern p2
  | PConst _ -> []
;;

let pattern_to_string ((pat, ty) : pattern_or_op_typed) : string list =
  match pat with
  | POpPat p -> bound_vars_pattern (p, ty)
  | POpOp s -> [ s ]
;;

let rec free_vars_expr (global_env : StringSet.t) ((e, _) : expr_typed) : StringSet.t =
  match e with
  | EConst _ -> StringSet.empty
  | EId id ->
    (* todo global env *)
    (match id with
     | (IdentOfDefinable (IdentLetters s) | IdentOfDefinable (IdentOp s))
       when 
           (not @@ String.starts_with ~prefix:"fun-" s)
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
  | DLet (_rec_flag, (pat_or_op, expr)) ->
    let bound =
      match pat_or_op with
      (* todo type lost? *)
      | POpPat (PId s), _ -> StringSet.singleton s
      | _ -> StringSet.empty
    in
    bound, free_vars_expr (StringSet.union bound env) expr
  | _ -> failwith "hui"
;;

(* | DLetMut (_rec_flag, lb, lb2, lbs) ->
   let all_pats = List.map (fun (pat_or_op, _) ->
   match pat_or_op with
   | POpPat (PId s) -> s
   | _ -> ""
   ) (lb :: lb2 :: lbs) in
   let bound =
   List.fold_left (fun s x -> if x = "" then s else StringSet.add x s)
   StringSet.empty all_pats
   in
   let free_in_lb (_, e) = free_vars_expr e in
   let free_all = List.fold_left (fun s lb -> StringSet.union s (free_in_lb lb))
   StringSet.empty (lb :: lb2 :: lbs)
   in
   (bound, free_all) *)

(* -------------------------------------------------------------------------- *)

let pattern_of_free_vars (fv : string list) : pattern_typed =
  match fv with
  | [] -> PConst CUnit, None
  | [ x ] -> PId x, None
  | x :: y :: rest ->
    let p1 = PId x, None in
    let p2 = PId y, None in
    let rest_pats = List.map (fun v -> PId v, None) rest in
    PTuple (p1, p2, rest_pats), None
;;

let rec efun_conversion local_env = function
  | EFun (pat, e), _ ->
    efun_conversion (List.fold_left StringSet.add local_env (bound_vars_pattern pat)) e
  | expr -> local_env, expr
;;

let rec change_body body = function
  | EFun (pat, e), ty -> EFun (pat, change_body body e), ty
  | _ -> body
;;

let rec closure_convert_expr (global_env : StringSet.t) ((e, t) : expr_typed)
  : expr_typed cc
  =
  match e with
  | EConst _ | EId _ -> return (e, t)
  | EFun (pat, _) as efun ->
    let local_env, body = efun_conversion StringSet.empty (efun, t) in
    let new_env = StringSet.union global_env local_env in
    let* body' = closure_convert_expr global_env body in
    let fv = free_vars_expr new_env body' in
    let fv = StringSet.elements fv in
    let* f_name = fresh_name "fun-" in
    let main_efun = EFun (pat, body'), t in
    let new_fun =
      List.fold_left (fun acc x -> EFun ((PId x, None), acc), None) main_efun fv
    in
    let decl = DLet (Not_recursive, ((POpPat (PId f_name), None), new_fun)) in
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
    let* ce1 = closure_convert_expr global_env e1 in
    let* ce2 = closure_convert_expr global_env e2 in
    return (EApp (ce1, ce2), t)
  | EIf (e1, e2, e3) ->
    let* ce1 = closure_convert_expr global_env e1 in
    let* ce2 = closure_convert_expr global_env e2 in
    let* ce3 = closure_convert_expr global_env e3 in
    return (EIf (ce1, ce2, ce3), t)
  | EList (e1, e2) ->
    let* ce1 = closure_convert_expr global_env e1 in
    let* ce2 = closure_convert_expr global_env e2 in
    return (EList (ce1, ce2), t)
  | ETuple (e1, e2, es) ->
    let* ce1 = closure_convert_expr global_env e1 in
    let* ce2 = closure_convert_expr global_env e2 in
    let rec convert_list = function
      | [] -> return []
      | x :: xs ->
        let* cx = closure_convert_expr global_env x in
        let* cxs = convert_list xs in
        return (cx :: cxs)
    in
    let* ces = convert_list es in
    return (ETuple (ce1, ce2, ces), t)
  | EClsr (decl, e) ->
    let* cdecl = closure_convert_let_in global_env decl in
    let* ce = closure_convert_expr global_env e in
    return (EClsr (cdecl, ce), t)
  | EMatch (e, br, brs) ->
    (* todo proper env *)
    let* ce = closure_convert_expr global_env e in
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
and closure_convert_let_in  global_env (d : decl) : decl cc =
  match d with
  | DLet (rf, (pat_or_op, expr)) ->
    let global_env =
      StringSet.union global_env
      @@ StringSet.from_list
      @@ if rf = Recursive then pattern_to_string pat_or_op else []
    in
    let* cexpr = closure_convert_expr global_env expr in
    return (DLet (rf, (pat_or_op, cexpr)))
  | _ -> failwith "todo"

and closure_convert_decl (global_env : StringSet.t) (d : decl) : (StringSet.t * decl) cc =
  match d with
  | DLet (rf, (pat_or_op, expr)) ->
    (* todo global env *)
    let _, body = efun_conversion global_env expr in
    (* horrible todo*)
    let global_env =
      StringSet.union global_env
      @@ StringSet.from_list
      @@ if rf = Recursive then pattern_to_string pat_or_op else []
    in
    let* body' = closure_convert_expr global_env body in
    return (global_env, DLet (rf, (pat_or_op, change_body body' expr)))
  | _ -> failwith "todo"
(* | DLetMut (rf, lb, lb2, lbs) ->
   let* clb = closure_convert_expr (snd lb) in
   let* clb2 = closure_convert_expr (snd lb2) in
   let rec conv lbs =
   match lbs with
   | [] -> return []
   | (pat_or_op, expr) :: xs ->
   let* cexpr = closure_convert_expr expr in
   let* cexprs = conv xs in
   return ((pat_or_op, cexpr) :: cexprs)
   in
   let* cexprs = conv lbs in
   return (DLetMut (rf, (fst lb, clb), (fst lb2, clb2), cexprs)) *)

and closure_convert_branch env (br : branch) : branch cc =
  let pat, expr = br in
  let* cexpr = closure_convert_expr env expr in
  return (pat, cexpr)
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
  List.rev extra @ decls
;;
