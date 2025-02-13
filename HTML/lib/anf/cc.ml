open AstLib.Ast

module StringSet = struct
  type t = (string, Base.String.comparator_witness) Base.Set.t

  let empty = Base.Set.empty (module Base.String)
  let add = Base.Set.add
  let remove = Base.Set.remove
  let diff = Base.Set.diff
  let singleton = Base.Set.add empty
  let contains set key = Base.Set.find set ~f:(( = ) key)
  let union = Base.Set.union
  let union_all = Base.Set.union_list (module Base.String)
  let elements = Base.Set.to_list
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

let rec free_vars_expr ((e, _) : expr_typed) : StringSet.t =
  match e with
  | EConst _ -> StringSet.empty
  | EId id ->
    (* todo global env *)
    (match id with
     | (IdentOfDefinable (IdentLetters s) | IdentOfDefinable (IdentOp s))
       when (not @@ List.mem s Common.Ops.base_binops)
            && (not @@ String.starts_with ~prefix:"fun-" s) ->
       StringSet.singleton (ident_to_string id)
     | _ -> StringSet.empty)
  | EFun (pat, body) ->
    let body_fv = free_vars_expr body in
    let bound =
      List.fold_left
        (fun s x -> StringSet.add s x)
        StringSet.empty
        (bound_vars_pattern pat)
    in
    StringSet.diff body_fv bound
  | EApp (e1, e2) -> StringSet.union (free_vars_expr e1) (free_vars_expr e2)
  | EIf (e1, e2, e3) ->
    let fv_e1 = free_vars_expr e1 in
    let fv_e2 = free_vars_expr e2 in
    let fv_e3 = free_vars_expr e3 in
    StringSet.union_all [ fv_e1; fv_e2; fv_e3 ]
  | EList (e1, e2) -> StringSet.union (free_vars_expr e1) (free_vars_expr e2)
  | ETuple (e1, e2, es) ->
    List.fold_left
      (fun acc et -> StringSet.union acc (free_vars_expr et))
      (StringSet.union (free_vars_expr e1) (free_vars_expr e2))
      es
  | EClsr (decl, e) ->
    let bound_in_decl, fv_decl = free_vars_decl decl in
    let fv_e = free_vars_expr e in
    StringSet.union fv_decl (StringSet.diff fv_e bound_in_decl)
  | EMatch (e, br, brs) ->
    let brs = br :: brs in
    let fv_e = free_vars_expr e in
    let free_vars_branch ((pat, expr) : branch) : StringSet.t =
      let bound =
        List.fold_left
          (fun s x -> StringSet.add s x)
          StringSet.empty
          (bound_vars_pattern pat)
      in
      StringSet.diff (free_vars_expr expr) bound
    in
    let fv_brs =
      List.fold_left
        (fun acc b -> StringSet.union acc (free_vars_branch b))
        StringSet.empty
        brs
    in
    StringSet.union fv_e fv_brs

and free_vars_decl (d : decl) : StringSet.t * StringSet.t =
  match d with
  | DLet (_rec_flag, (pat_or_op, expr)) ->
    let bound =
      match pat_or_op with
      (* todo type lost? *)
      | POpPat (PId s), _ -> StringSet.singleton s
      | _ -> StringSet.empty
    in
    bound, free_vars_expr expr
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

let rec substitute_expr ((e, t) : expr_typed) (subst : (string * expr_typed) list)
  : expr_typed
  =
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
    EFun (pat, substitute_expr body subst'), t
  | EApp (e1, e2) -> EApp (substitute_expr e1 subst, substitute_expr e2 subst), t
  | EIf (e1, e2, e3) ->
    EIf (substitute_expr e1 subst, substitute_expr e2 subst, substitute_expr e3 subst), t
  | EList (e1, e2) -> EList (substitute_expr e1 subst, substitute_expr e2 subst), t
  | ETuple (e1, e2, es) ->
    ( ETuple
        ( substitute_expr e1 subst
        , substitute_expr e2 subst
        , List.map (fun e -> substitute_expr e subst) es )
    , t )
  | EClsr (decl, e) -> EClsr (substitute_decl decl subst, substitute_expr e subst), t
  | EMatch (e, br, brs) ->
    let substitute_branch ((pat, expr) : branch) (subst : (string * expr_typed) list)
      : branch
      =
      let bound = bound_vars_pattern pat in
      let subst' = List.filter (fun (x, _) -> not (List.mem x bound)) subst in
      pat, substitute_expr expr subst'
    in
    ( EMatch
        ( substitute_expr e subst
        , substitute_branch br subst
        , List.map (fun b -> substitute_branch b subst) brs )
    , t )

and substitute_decl (d : decl) (subst : (string * expr_typed) list) : decl =
  match d with
  | DLet (rf, (pat_or_op, expr)) ->
    let bound =
      (* todo type lost *)
      match pat_or_op with
      | POpPat (PId s), _ -> [ s ]
      | _ -> []
    in
    let subst' = List.filter (fun (x, _) -> not (List.mem x bound)) subst in
    DLet (rf, (pat_or_op, substitute_expr expr subst'))
  | _ -> failwith "hui"
;;

(* | DLetMut (rf, lb, lb2, lbs) ->
   let all_bindings = lb :: lb2 :: lbs in
   let bound =
   List.fold_left (fun acc (pat_or_op, _) ->
   match pat_or_op with
   | POpPat (PId s) -> s :: acc
   | _ -> acc
   ) [] all_bindings
   in
   let subst' = List.filter (fun (x, _) -> not (List.mem x bound)) subst in
   let subst_lb (pat_or_op, expr) =
   (pat_or_op, substitute_expr expr subst')
   in
   DLetMut (rf, subst_lb lb, subst_lb lb2, List.map subst_lb lbs) *)

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

let rec closure_convert_expr ((e, t) : expr_typed) : expr_typed cc =
  match e with
  | EConst _ | EId _ -> return (e, t)
  (* | EFun (pat, (EFun (_, _) as efun, ty)) ->
     let* efun = closure_convert_expr (efun, ty) in
     return (EFun(pat, efun), t) *)
  | EFun (pat, body) ->
    let* body' = closure_convert_expr body in
    let fv_set = free_vars_expr body' in
    let bound =
      List.fold_left
        (fun s x -> StringSet.add s x)
        StringSet.empty
        (bound_vars_pattern pat)
    in
    let fv_set = StringSet.diff fv_set bound in
    let fv = StringSet.elements fv_set in
    if fv = []
    then
      return (EFun (pat, body'), t)
    else (
      (* let* env_name = fresh_name "env-" in *)
      (* let proj i =
         let proj_ident = IdentOfDefinable (IdentLetters ("proj" ^ string_of_int i)) in
         EApp ((EId proj_ident, None), (EId (IdentOfDefinable (IdentLetters env_name)), None))
         in
         let subst = List.mapi (fun i x -> x, (proj (i + 1), None)) fv in
         let body'' = substitute_expr body' subst in *)
      let env_pattern = pattern_of_free_vars fv in
      let new_fun = EFun (env_pattern, (EFun (pat, body'), t)) in
      let* f_name = fresh_name "fun-" in
      let _decl = DLet (Not_recursive, ((POpPat (PId f_name), None), (new_fun, None))) in
      let* () = tell _decl in
      let env_val =
        match fv with
        | [] -> EConst CUnit, None
        | [ x ] -> EId (IdentOfDefinable (IdentLetters x)), None
        | x :: xs ->
          let first = EId (IdentOfDefinable (IdentLetters x)), None in
          let rest =
            List.map (fun x -> EId (IdentOfDefinable (IdentLetters x)), None) xs
          in
          ETuple (first, List.hd rest, List.tl rest), None
      in
      return (EApp ((EId (IdentOfDefinable (IdentLetters f_name)), None), env_val), t))
  | EApp (e1, e2) ->
    let* ce1 = closure_convert_expr e1 in
    let* ce2 = closure_convert_expr e2 in
    return (EApp (ce1, ce2), t)
  | EIf (e1, e2, e3) ->
    let* ce1 = closure_convert_expr e1 in
    let* ce2 = closure_convert_expr e2 in
    let* ce3 = closure_convert_expr e3 in
    return (EIf (ce1, ce2, ce3), t)
  | EList (e1, e2) ->
    let* ce1 = closure_convert_expr e1 in
    let* ce2 = closure_convert_expr e2 in
    return (EList (ce1, ce2), t)
  | ETuple (e1, e2, es) ->
    let* ce1 = closure_convert_expr e1 in
    let* ce2 = closure_convert_expr e2 in
    let rec convert_list = function
      | [] -> return []
      | x :: xs ->
        let* cx = closure_convert_expr x in
        let* cxs = convert_list xs in
        return (cx :: cxs)
    in
    let* ces = convert_list es in
    return (ETuple (ce1, ce2, ces), t)
  | EClsr (decl, e) ->
    let* cdecl = closure_convert_decl decl in
    let* ce = closure_convert_expr e in
    return (EClsr (cdecl, ce), t)
    (* (match decl with
       | DLet (_, (_, (EApp (_, _) as eapp, ty))) -> return (eapp, ty)

       | _ -> return (EClsr (cdecl, ce), t)) *)

    (* let* cdecl = closure_convert_decl decl in
       (match cdecl with
       | DLet (rf, ((s, _), (EApp(_, _), ty) as eapp)) ->
       let* ce = closure_convert_expr e in
       let subst = substitute_expr e [(ident_to_string s, eapp)] in
       return (EClsr (cdecl, ce), t)
       | _ -> let* ce = closure_convert_expr e in
       return (EClsr (cdecl, ce), t)) *)
  | EMatch (e, br, brs) ->
    let* ce = closure_convert_expr e in
    let* cbr = closure_convert_branch br in
    let rec conv_branches = function
      | [] -> return []
      | x :: xs ->
        let* cx = closure_convert_branch x in
        let* cxs = conv_branches xs in
        return (cx :: cxs)
    in
    let* cbrs = conv_branches brs in
    return (EMatch (ce, cbr, cbrs), t)

and closure_convert_decl (d : decl) : decl cc =
  match d with
  | DLet (rf, (pat_or_op, expr)) ->
    let* cexpr = closure_convert_expr expr in
    return (DLet (rf, (pat_or_op, cexpr)))
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

and closure_convert_branch (br : branch) : branch cc =
  let pat, expr = br in
  let* cexpr = closure_convert_expr expr in
  return (pat, cexpr)
;;

let rec closure_convert_decl_list (decls : decl list) : decl list cc =
  match decls with
  | [] -> return []
  | d :: ds ->
    let* cd = closure_convert_decl d in
    let* cds = closure_convert_decl_list ds in
    return (cd :: cds)
;;

let closure_convert (prog : decl list) : decl list =
  let decls, _, extra = closure_convert_decl_list prog 0 in
  List.rev extra @ decls
;;
