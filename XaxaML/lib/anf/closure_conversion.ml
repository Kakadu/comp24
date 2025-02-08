open Ast
open Base

module StrSet : sig
  type t

  val empty : t
  val singleton : string -> t
  val union : t -> t -> t
  val union_list : t list -> t
  val add : t -> string -> t
  val find : t -> string -> bool
end = struct
  type t = (string, String.comparator_witness) Set.t

  let empty = Set.empty (module String)
  let singleton str = Set.singleton (module String) str
  let union = Set.union
  let union_list lst = Set.union_list (module String) lst

  let find s str =
    match Set.binary_search s ~compare:String.compare `First_equal_to str with
    | Some _ -> true
    | None -> false
  ;;

  let add = Set.add
end

let rec get_idens = function
  | P_typed (pat, _) -> get_idens pat
  | P_any | P_const _ -> StrSet.empty
  | P_val ident -> StrSet.singleton ident
  | P_cons_list (p1, p2) -> StrSet.union (get_idens p1) (get_idens p2)
  | P_tuple (hd, tl) -> StrSet.union (get_idens hd) (get_idents_pat_list tl)

and get_idents_pat_list pat_list =
  List.fold pat_list ~init:StrSet.empty ~f:(fun acc p -> StrSet.union acc (get_idens p))
;;

let rec get_free binded = function
  | E_typed (e, _) -> get_free binded e
  | E_const _ -> StrSet.empty
  | E_ident ident ->
    if StrSet.find binded ident then StrSet.empty else StrSet.singleton ident
  | E_ite (e1, e2, e3) ->
    StrSet.union_list [ get_free binded e1; get_free binded e2; get_free binded e3 ]
  | E_fun (first, other, e) ->
    let idents = get_idents_pat_list (first :: other) in
    let binded = StrSet.union binded idents in
    get_free binded e
  | E_app (e1, e2) -> StrSet.union (get_free binded e1) (get_free binded e2)
  | E_let (Non_rec (pat, _, e1), e2) ->
    let idents = get_idens pat in
    StrSet.union (get_free binded e1) (get_free (StrSet.union idents binded) e2)
  | E_let (Rec decls, e2) ->
    let idents, expr_list =
      List.fold_right decls ~init:(StrSet.empty, []) ~f:(fun (pat, _, e) (ids, exprs) ->
        StrSet.union ids (get_idens pat), e :: exprs)
    in
    let binded = StrSet.union binded idents in
    List.fold expr_list ~init:(get_free binded e2) ~f:(fun acc e ->
      StrSet.union acc (get_free binded e))
  | E_match (e1, case_list) ->
    let e1_free = get_free binded e1 in
    List.fold case_list ~init:e1_free ~f:(fun acc (pat, e) ->
      let idents = get_idens pat in
      let cur_bind = StrSet.union binded idents in
      let free = get_free cur_bind e in
      StrSet.union acc free)
  | E_cons_list (e1, e2) -> StrSet.union (get_free binded e1) (get_free binded e2)
  | E_tuple (e1, e_list) ->
    let e1_free = get_free binded e1 in
    List.fold e_list ~init:e1_free ~f:(fun acc e -> StrSet.union acc (get_free binded e))
;;

let rec expr global_env bindings = function
  | E_typed (e, _) -> expr global_env bindings e
  | E_const _ as orig -> StrSet.empty, orig
  | E_ident ident as orig ->
    let visited_ident = StrSet.singleton ident in
    if StrSet.find global_env ident
    then visited_ident, orig
    else visited_ident, Map.find_exn bindings ident
  | E_ite (if1, then1, else1) ->
    let visited1, if1 = expr global_env bindings if1 in
    let visited2, then1 = expr global_env bindings then1 in
    let visited3, else1 = expr global_env bindings else1 in
    StrSet.union_list [ visited1; visited2; visited3 ], E_ite (if1, then1, else1)
  | E_fun (first, other, e) as orig ->
    let visited, e = expr global_env bindings e in
    StrSet.empty, E_const C_unit
  | E_app (e1, e2) ->
    let visited1, e1 = expr global_env bindings e1 in
    let visited2, e2 = expr global_env bindings e2 in
    StrSet.union visited1 visited2, E_app (e1, e2)
  | _ -> StrSet.empty, E_const C_unit
;;

let decl =
  let helper = function
    | P_val ident, expr -> 0 (* TODO *)
    | P_tuple (fst, other), expr -> 0 (* TODO *)
    | P_cons_list (hd, tl), expr -> 0 (* TODO *)
    | _ -> -1
  in
  0
;;

let toplevel _env_names = function
  | Expr _ | Let_decl _ -> StrSet.empty, Expr (Ast.E_const (C_bool true))
;;

let std_names = List.fold Std_names.std_names ~init:StrSet.empty ~f:StrSet.add

let run_closure_conversion_program program =
  let rec helper last_env = function
    | [] -> []
    | hd :: tl ->
      let cur_env, cur_ast = toplevel last_env hd in
      cur_ast :: helper cur_env tl
  in
  helper std_names program
;;
