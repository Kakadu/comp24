(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Base 
open Pe_ast

module StateMonad : sig
  include Base.Monad.Infix

  val return : 'a -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold_left
      :  ('a, 'b, 'c) Base.Map.t
      -> init:'d t
      -> f:('a -> 'b -> 'd -> 'd t)
      -> 'd t
  end

  val fresh : int t
  val run : 'a t -> 'a
end = struct
  type 'a t = int -> int * 'a (* State and Result monad composition *)

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f s ->
    let s', v' = m s in
    f v' s'
  ;;

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun m f s ->
    let s', x = m s in
    s', f x
  ;;

  let return v last = last, v
  let bind x ~f = x >>= f
  let fresh last = last + 1, last (* Get new state *)
  let ( let* ) x f = bind x ~f (* Syntax sugar for bind *)

  module RMap = struct
    (* Classic map folding. *)
    let fold_left mp ~init ~f =
      Base.Map.fold mp ~init ~f:(fun ~key ~data acc ->
        let* acc = acc in
        f key data acc)
    ;;
  end

  module RList = struct
    (* Classic list folding. *)
    let fold_left lt ~init ~f =
      Base.List.fold_left lt ~init ~f:(fun acc item ->
        let* acc = acc in
        f acc item)
    ;;
  end

  (* Run and get the internal value. *)
  let run m = snd (m 0)
end


let get_new_id n name = String.concat [ name; "_me"; Int.to_string n ]

open StateMonad

let const_to_pe_const = function
  | CInt a -> Pe_Cint a
  | CBool a -> Pe_CBool a

let rec pattern_remove pat =
  match pat with
  | PUnit -> ["()"]
  | PConst _ -> []
  | PIdentifier id -> [id]
  | PNill -> ["[]"]
  | PTuple pats -> List.concat_map ~f:pattern_remove pats
  | PCons (hd, tl) -> pattern_remove hd @ pattern_remove tl
  | PConstraint (p, _) -> pattern_remove p
  | _ -> failwith "bye-bye, bro. im sleeping"

let rec_flags : Ast.rec_flag -> Pe_ast.rec_flag = function
  | Rec -> Rec
  | NoRec -> NoRec

let rec expr_to_mexpr expr =
  match expr with
  | EUnit -> return Pe_EUnit
  | ENill -> return Pe_ENill
  | EConstraint (e, _) -> expr_to_mexpr e
  | EConst c -> return @@ Pe_EConst (const_to_pe_const c)
  | EIdentifier id -> return @@ Pe_EIdentifier id
  | EApplication (f, arg) ->
    let* f' = expr_to_mexpr f in
    let* arg' = expr_to_mexpr arg in
    return @@ Pe_EApp (f', arg')
  | EFun (pat, body) ->
    let ids = pattern_remove pat in
    let* body' = expr_to_mexpr body in
    return @@ Pe_EFun (ids, body')
  | ELetIn (rec_flag, pat, e1, e2) ->
    let ids = pattern_remove pat in
    let* e1' = expr_to_mexpr e1 in
    let* e2' = expr_to_mexpr e2 in
    let rec_flag = rec_flags rec_flag in
    (match ids with
     | [id] -> return @@ Pe_ELet (rec_flag, id, e1', e2')
     | _ -> failwith "Only simple let bindings with 1 identifier are supported after alpha conversion")
  | ETuple exprs ->
    let* exprs' = RList.fold_left exprs ~init:(return []) ~f:(fun acc e ->
      let* e' = expr_to_mexpr e in
      return (acc @ [e'])) in
    return @@ Pe_ETuple exprs'
  | EIf (cond, then_, else_) ->
    let* cond' = expr_to_mexpr cond in
    let* then_' = expr_to_mexpr then_ in
    let* else_' = expr_to_mexpr else_ in
    return @@ Pe_EIf (cond', then_', else_')
  | ECons (hd, tl) ->
    let* hd' = expr_to_mexpr hd in
    let* tl' = expr_to_mexpr tl in
    return @@ Pe_ECons (hd', tl')
  | EMatch (e, branches) ->
    desugar_match e branches

and desugar_match e branches =
  let* e' = expr_to_mexpr e in
  match branches with
  | [] -> failwith "Empty match expression"
  | (pat, expr_rhs) :: rest ->
    let* id_num = fresh in
    let tmp_var = get_new_id id_num "match_tmp" in
    let bound_expr = Pe_EIdentifier tmp_var in
    let* expr_rhs' = expr_to_mexpr expr_rhs in
    let rec pattern_to_condition expr pat =
      match pat with
      | PAny -> return @@ Pe_EConst (Pe_CBool true)
      | PUnit -> return @@ Pe_EIf (Pe_EApp (Pe_EIdentifier "is_unit", expr), Pe_EConst (Pe_CBool true), Pe_EConst (Pe_CBool false))
      | PConst c -> return @@ Pe_EApp (Pe_EApp (Pe_EIdentifier "(=)",
                                                 expr),
                                       Pe_EConst (const_to_pe_const c))
      | PIdentifier _ -> return @@ Pe_EConst (Pe_CBool true)
      | PNill -> return @@ Pe_EApp (Pe_EIdentifier "is_nil", expr)
      | PCons (hd, tl) ->
        let hd_expr = Pe_EApp (Pe_EIdentifier "hd", expr) in
        let tl_expr = Pe_EApp (Pe_EIdentifier "tl", expr) in
        let* cond_hd = pattern_to_condition hd_expr hd in
        let* cond_tl = pattern_to_condition tl_expr tl in
        return @@ Pe_EIf (Pe_EApp (Pe_EIdentifier "is_cons", expr),
                          Pe_EIf (cond_hd, cond_tl, Pe_EConst (Pe_CBool false)),
                          Pe_EConst (Pe_CBool false))
      | PTuple pats ->
        let* conds =
          RList.fold_left (List.mapi pats ~f:(fun i p -> (i, p)))
            ~init:(return [])
            ~f:(fun acc (i, p) ->
              let ith_expr = Pe_EApp (Pe_EIdentifier ("fst" ^ Int.to_string i), expr) in
              let* cond = pattern_to_condition ith_expr p in
              return (acc @ [cond])) in
        return @@ List.fold_right conds ~init:(Pe_EConst (Pe_CBool true))
                    ~f:(fun c acc -> Pe_EIf (c, acc, Pe_EConst (Pe_CBool false)))
      | PConstraint (p, _) -> pattern_to_condition expr p
    in

    let* cond = pattern_to_condition bound_expr pat in
    let* rest_expr =
      match rest with
      | [] -> return @@ Pe_EConst (Pe_CBool false) (* dummy fail case *)
      | _ -> desugar_match (EIdentifier tmp_var) rest
    in
    return @@ Pe_ELet (NoRec, tmp_var, e',
               Pe_EIf (cond, expr_rhs', rest_expr))
  

let decl_to_pe_decl decl =
  match decl with
  | NoRecDecl decls ->
    let* converted =
      RList.fold_left decls ~init:(return []) ~f:(fun acc (DDeclaration (pat, expr)) ->
        let ids = pattern_remove pat in
        match ids with
        | [id] ->
          let* e' = expr_to_mexpr expr in
          return ((id, e') :: acc)
        | _ -> failwith "Bang-Bang. Only simple declarations supported") in
    return @@ Pe_Nonrec (List.rev converted)
  | RecDecl decls ->
    let* converted =
      RList.fold_left decls ~init:(return []) ~f:(fun acc (DDeclaration (pat, expr)) ->
        let ids = pattern_remove pat in
        match ids with
        | [id] ->
          let* e' = expr_to_mexpr expr in
          return ((id, e') :: acc)
        | _ -> failwith "Bang-Bang. Only simple declarations supported") in
    return @@ Pe_Rec (List.rev converted)

let match_elimination prog  =
  StateMonad.run (
    RList.fold_left prog ~init:(return []) ~f:(fun acc decl ->
      let* d = decl_to_pe_decl decl in
      return (acc @ [d]))
  )