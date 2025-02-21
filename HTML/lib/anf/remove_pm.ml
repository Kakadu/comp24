open AstLib.Ast

type bin_op =
  | Add (** + *)
  | Sub (** - *)
  | Mul (** * *)
  | Div (** / *)
  | Leq (** <= *)
  | Less (** < *)
  | Geq (** >= *)
  | Gre (** > *)
  | Eq (** == *)
  | Neq (** != *)
  | And (** && *)
  | Or (** || *)
[@@deriving eq, show { with_path = false }]

type expr_no_pm =
  | NoPMEConst of const (** Const. Examples: 100; true *)
  | NoPMEId of ident (** Identifier. Examples: a, b, c *)
  | NoPMEFun of pattern * expr_no_pm (** Function. Examples: fun x -> x + 1 *)
  | NoPMEApp of expr_no_pm * expr_no_pm (** Application. Examples: f (x - 1) *)
  | NoPMEIf of expr_no_pm * expr_no_pm * expr_no_pm
  (** If-then-else. Examples: if x >= y then x - y else y - x *)
  | NoPMEList of expr_no_pm * expr_no_pm (** Lists. Examples: [1; 2; 3] *)
  | NoPMETuple of expr_no_pm * expr_no_pm * expr_no_pm list
  (** Tuple. Examples: (1, 2, 3) *)
  | NoPMEClsr of decl_no_pm * expr_no_pm
  (** Closure. Examples: let inc x = x + 1 in inc 5*)
  | NoPMEBinOp of bin_op * expr_no_pm * expr_no_pm
  (** Since operators can be redefined, these binary operations have standard semantics. *)
  | NoPMEConstraint of expr_no_pm typed
[@@deriving eq, show { with_path = false }]

and let_body_no_pm = pattern_or_op * expr_no_pm
[@@deriving eq, show { with_path = false }]

and decl_no_pm =
  | NoPMDLet of rec_flag * let_body_no_pm (** Let declaration *)
  | NoPMDLetMut of rec_flag * let_body_no_pm * let_body_no_pm * let_body_no_pm list
  (** Mutual let declaration *)
[@@deriving eq, show { with_path = false }]

let econst c = NoPMEConst c
let eid i = NoPMEId i
let efun pat e = NoPMEFun (pat, e)
let eapp f args = NoPMEApp (f, args)
let eif e1 e2 e3 = NoPMEIf (e1, e2, e3)
let elist hd tl = NoPMEList (hd, tl)
let etuple e1 e2 l = NoPMETuple (e1, e2, l)
let eclsr d e = NoPMEClsr (d, e)
let ebin_op bop e1 e2 = NoPMEBinOp (bop, e1, e2)

let e_typed ?(typ = None) e : expr_no_pm =
  match typ with
  | Some typ -> NoPMEConstraint (e, typ)
  | None -> e
;;

let dlet rf let_body_no_pm = NoPMDLet (rf, let_body_no_pm)
let dletmut rec_flag fst snd tl = NoPMDLetMut (rec_flag, fst, snd, tl)

module R = struct
  open Base.Result

  type 'a t = int -> int * ('a, string) Result.t

  let fail error state = state, fail error
  let return value last = last, return value

  let ( >>= ) monad f state =
    let last, result = monad state in
    match result with
    | Error e -> last, Error e
    | Ok value -> f value last
  ;;

  let ( let* ) = ( >>= )
  let ( >>| ) m f = m >>= fun x -> return @@ f x
  let fresh last = last + 1, Ok last
  let run m = snd (m 0)

  let map f xs =
    let* res =
      List.fold_left
        (fun acc x ->
          let* acc = acc in
          let* res = f x in
          return (res :: acc))
        (return [])
        xs
    in
    return (List.rev res)
  ;;
end

open R

module RuntimeEnv = struct
  type runtime_member =
    { name : string
    ; typ : typ
    }

  let apply f x =
    return @@ e_typed (eapp (e_typed (eid (ident_of_definable (ident_letters f.name)))) x)
  ;;

  (** add it to infer *)
  let generic = tvar "a"

  let get_head = { name = "GET_HEAD"; typ = tarrow (tlist generic) generic }
  let get_tl = { name = "GET_TALE"; typ = tarrow (tlist generic) (tlist generic) }
  let get_nth = { name = "GET_NTH"; typ = tarrow (ttuple tint generic []) (tvar "b") }

  let not_exhaustive_pm =
    { name = "RTE_ERROR_NOT_EXHAUSTIVE_PATTERN_MATCHING"; typ = tarrow tunit generic }
  ;;

  let init_env = [ get_head; get_tl; get_nth; not_exhaustive_pm ]
end

module Env = struct
  let extend env id e = Base.Map.update env id ~f:(fun _ -> e)
  let empty = Base.Map.empty (module Base.String)
  let lookup_env id map = Base.Map.find map id

  let merge m1 m2 =
    Base.Map.fold_right m2 ~init:(return m1) ~f:(fun ~key ~data acc ->
      let* acc = acc in
      match lookup_env key acc with
      | Some _ -> fail "Key intersection"
      | None -> return @@ extend acc key data)
  ;;
end

open Env

let get_typ_of_pat = function
  | PConstraint (_, typ) -> Some typ
  | _ -> None
;;

let rec is_pattern_suitable = function
  | PId _, _ -> return @@ econst (CBool true)
  | PConst x, e -> return @@ ebin_op Eq (econst x) e
  | PList (p1, p2), e ->
    let is_not_empty = ebin_op Neq (econst CNil) e in
    let* head = RuntimeEnv.apply RuntimeEnv.get_head e in
    let* tl = RuntimeEnv.apply RuntimeEnv.get_tl e in
    let* head_res = is_pattern_suitable (p1, head) in
    let* tl_res = is_pattern_suitable (p2, tl) in
    return @@ ebin_op And (ebin_op And is_not_empty head_res) tl_res
  | PTuple (p1, p2, ps), e ->
    let all_pats = p1 :: p2 :: ps in
    let check_nth n pat_n =
      let* nth_el =
        RuntimeEnv.apply
          RuntimeEnv.get_nth
          (e_typed ~typ:(get_typ_of_pat pat_n) (etuple (econst (CInt n)) e []))
      in
      is_pattern_suitable (pat_n, nth_el)
    in
    let* res, _ =
      List.fold_left
        (fun acc pat ->
          let* res, i = acc in
          let* i_check = check_nth i pat in
          let res = ebin_op And res i_check in
          return (res, i + 1))
        (return @@ (econst (CBool true), 0))
        all_pats
    in
    return res
  | PConstraint (pat, typ), e -> is_pattern_suitable (pat, e_typed ~typ:(Some typ) e)
;;

let rec get_pattern_vars = function
  | PConst _, _ | PId "_", _ -> return empty
  | PId id, e -> return @@ extend empty id e
  | PList (p1, p2), e ->
    let* head = RuntimeEnv.apply RuntimeEnv.get_head e in
    let* tl = RuntimeEnv.apply RuntimeEnv.get_tl e in
    let* p1_map = get_pattern_vars (p1, head) in
    let* p2_map = get_pattern_vars (p2, tl) in
    Env.merge p1_map p2_map
  | PTuple (p1, p2, ps), e ->
    let all_pats = p1 :: p2 :: ps in
    let* res, _ =
      List.fold_left
        (fun acc x ->
          let* acc, i = acc in
          let* nth_el =
            RuntimeEnv.apply RuntimeEnv.get_nth (etuple (econst (CInt i)) e [])
          in
          let* pattern_vars = get_pattern_vars (x, nth_el) in
          let* acc' = merge acc pattern_vars in
          return (acc', i + 1))
        (return (empty, 0))
        all_pats
    in
    return res
  | PConstraint (pat, typ), e -> get_pattern_vars (pat, e_typed ~typ:(Some typ) e)
;;

let remove_pm =
  let rec helper = function
    | (pat, e_res) :: tl, e ->
      let* cond = is_pattern_suitable (pat, e) in
      let* pattern_vars = get_pattern_vars (pat, e) in
      let* res_expr =
        Base.Map.fold_right pattern_vars ~init:(return e_res) ~f:(fun ~key ~data acc ->
          let* acc = acc in
          let pat = pid key in
          let pat =
            match data with
            | NoPMEConstraint (_, typ) -> p_typed ~typ:(Some typ) pat
            | _ -> pat
          in
          let decl = dlet Not_recursive (pop_pat pat, data) in
          return @@ eclsr decl acc)
      in
      let* res_tl = helper (tl, e) in
      (match cond with
       | NoPMEConst (CBool true) -> return @@ res_expr
       | _ -> return @@ eif cond res_expr res_tl)
    | [], _ -> RuntimeEnv.apply RuntimeEnv.not_exhaustive_pm (econst CUnit)
  in
  helper
;;

let rec remove_expr_pm expr =
  let rec helper = function
    | EConst x -> return @@ econst x
    | EId x -> return @@ eid x
    | EIf (i, t, e) ->
      let* i_res = helper i in
      let* t_res = helper t in
      let* e_res = helper e in
      return @@ eif i_res t_res e_res
    | EApp (e1, e2) ->
      let* e1_res = helper e1 in
      let* e2_res = helper e2 in
      return @@ eapp e1_res e2_res
    | EFun (pat, e) ->
      let* e_res = helper e in
      return @@ efun pat e_res
    | EClsr (decl, expr) ->
      let* no_pm_decl = remove_decl_pm decl in
      let* expr_res = helper expr in
      return @@ eclsr no_pm_decl expr_res
    | EList (e1, e2) ->
      let* e1_res = helper e1 in
      let* e2_res = helper e2 in
      return @@ elist e1_res e2_res
    | ETuple (e1, e2, els) ->
      let* e1_res = helper e1 in
      let* e2_res = helper e2 in
      let* no_pm_els = map remove_expr_pm els in
      return @@ etuple e1_res e2_res no_pm_els
    | EConstraint (e, typ) ->
      let* e_res = helper e in
      return @@ e_typed ~typ:(Some typ) e_res
    | EMatch (e, br, brs) ->
      let* e_res = helper e in
      let* fresh = fresh in
      let evaluated_name = "EVALUATED_" ^ string_of_int fresh in
      let evaluated_pat = pop_pat (pid evaluated_name) in
      let evaluated_e = eid (ident_of_definable (ident_letters evaluated_name)) in
      let* no_pm_els =
        map
          (fun (pat, e) ->
            let* no_pm_el = remove_expr_pm e in
            return (pat, no_pm_el))
          (br :: brs)
      in
      let* res = remove_pm (no_pm_els, evaluated_e) in
      return @@ eclsr (dlet Not_recursive (evaluated_pat, e_res)) res
  in
  helper expr

and remove_decl_pm decl =
  let helper decl =
    match decl with
    | DLet (rec_flag, (pat, expr)) ->
      let* no_pm_expr = remove_expr_pm expr in
      return @@ NoPMDLet (rec_flag, (pat, no_pm_expr))
    | DLetMut (rec_flag, (pat, expr), (pat1, expr1), tl) ->
      let applied = tl in
      let* res =
        map
          (fun (pat, e) ->
            let* e = remove_expr_pm e in
            return (pat, e))
          applied
      in
      let* expr = remove_expr_pm expr in
      let* expr1 = remove_expr_pm expr1 in
      return @@ NoPMDLetMut (rec_flag, (pat, expr), (pat1, expr1), res)
  in
  helper decl
;;

let bin_op_to_str bo =
  let map = function
    | Add -> "+"
    | Sub -> "_"
    | Mul -> "*"
    | Div -> "/"
    | Leq -> "<="
    | Less -> "<"
    | Geq -> ">="
    | Gre -> ">"
    | Eq -> "="
    | Neq -> "!="
    | And -> "&&"
    | Or -> "||"
  in
  "base_bop_" ^ map bo
;;

let rec transform_back = function
  | NoPMEConst c -> EConst c
  | NoPMEId id -> EId id
  | NoPMEBinOp (bin_op, e1, e2) ->
    EApp
      ( EApp (EId (IdentOfDefinable (IdentOp (bin_op_to_str bin_op))), transform_back e1)
      , transform_back e2 )
  | NoPMEApp (e1, e2) -> EApp (transform_back e1, transform_back e2)
  | NoPMEFun (pat, expr) -> EFun (pat, transform_back expr)
  | NoPMEIf (i, t, e) -> EIf (transform_back i, transform_back t, transform_back e)
  | NoPMEClsr (decl, expr2) -> EClsr (transform_back_decl decl, transform_back expr2)
  | NoPMEConstraint (e, typ) -> EConstraint (transform_back e, typ)
  | NoPMEList (e1, e2) -> EList (transform_back e1, transform_back e2)
  | NoPMETuple (e1, e2, els) ->
    ETuple (transform_back e1, transform_back e2, List.map transform_back els)

and transform_back_decl = function
  | NoPMDLet (rec_flag, (decl, expr)) ->
    let expr = transform_back expr in
    DLet (rec_flag, (decl, expr))
  | NoPMDLetMut (rec_flag, (pat, expr), (pat1, expr1), tl) ->
    let applied = tl in
    let res = List.map (fun (pat, e) -> pat, transform_back e) applied in
    DLetMut (rec_flag, (pat, transform_back expr), (pat1, transform_back expr1), res)
;;

let transform_expr_in_decl f =
  let helper = function
    | NoPMDLet (rec_flag, (pat, expr)) -> NoPMDLet (rec_flag, (pat, f expr))
    | NoPMDLetMut (rec_flag, (pat, expr), (pat1, expr1), tl) ->
      let res = List.map (fun (pat, e) -> pat, f e) tl in
      NoPMDLetMut (rec_flag, (pat, f expr), (pat1, f expr1), res)
  in
  helper
;;

let rec optimize e =
  let rec helper = function
    | NoPMEBinOp (And, NoPMEConst (CBool true), e)
    | NoPMEBinOp (And, e, NoPMEConst (CBool true))
    | NoPMEIf (NoPMEConst (CBool true), e, _) -> helper e
    | NoPMEApp (e1, e2) -> eapp (helper e1) (helper e2)
    | NoPMEClsr (decl, expr) -> eclsr (transform_expr_in_decl helper decl) (helper expr)
    | (NoPMEConst _ | NoPMEId _) as e -> e
    | NoPMEConstraint (e, t) -> e_typed ~typ:(Some t) (helper e)
    | NoPMEFun (pat, e) -> efun pat (helper e)
    | NoPMEIf (i, t, e) -> eif (helper i) (helper t) (helper e)
    | NoPMEList (e1, e2) -> elist (helper e1) (helper e2)
    | NoPMETuple (e1, e2, es) -> etuple (helper e1) (helper e2) (List.map helper es)
    | NoPMEBinOp (bop, e1, e2) -> ebin_op bop (helper e1) (helper e2)
  in
  let optimized = helper e in
  if optimized = e then e else optimize optimized
;;

let remove_decls_pm decls =
  run
    (map
       (fun x ->
         remove_decl_pm x >>| transform_expr_in_decl optimize >>| transform_back_decl)
       decls)
;;
