open AstLib.Ast

module Set = struct
  let empty = Base.Set.empty (module Base.String)
  let add = Base.Set.add
  let remove = Base.Set.remove
  let diff = Base.Set.diff
  let singleton = Base.Set.add empty
  let contains set key = Base.Set.find set ~f:(( = ) key)
  let union = Base.Set.union  
  let union_all = Base.Set.union_list (module Base.String)
end

module Env = struct end
open Set

type fun_to_lift_scope =
  | Nested of string list
  | Anonymous of string list

type fun_to_lift = fun_to_lift_scope * int

let init_env = Base.Map.empty (module Base.String)

let apply_to_list f =
  List.fold_left
    (fun total v ->
      let* total = total in
      let* res = f total v in
      return (union_all [ res; total ]))
    (return empty)
;;

let rec get_idents = function
  | PId id -> return @@ singleton id
  | PList ((hd, _), (tl, _)) -> apply_to_list (fun _ x -> get_idents x) [ hd; tl ]
  | PTuple ((p1, _), (p2, _), tl) ->
    let without_type = List.map (fun (pat, _) -> pat) tl in
    apply_to_list (fun _ x -> get_idents x) (p1 :: p2 :: without_type)
  | PConst _ -> return empty
;;

let rec close_function lts local_ctx global_ctx convert = function
  | EFun (pat, body) ->
    EFun (pat, close_function lts local_ctx global_ctx convert body)
  | expr -> convert lts local_ctx global_ctx expr
;;

let close =
  let rec helper local_ctx global_ctx = function
    | EConst _ | EId (IdentOfBaseOp _ | IdentOfDefinable (IdentOp _)) -> empty
    | EId (IdentOfDefinable (IdentLetters id)) ->
      if contains local_ctx id then singleton id else empty
    | EIf ((i, _), (t, _), (e, _)) ->
      let i_res = helper local_ctx global_ctx i in
      let t_res = helper local_ctx global_ctx t in
      let e_res = helper local_ctx global_ctx e in
      union_all [ i_res; t_res; e_res ]
    | EApp ((e1, _), (e2, _)) ->
      let e1_res = helper local_ctx global_ctx e1 in
      let e2_res = helper local_ctx global_ctx e2 in
      union e1_res e2_res
    | _ -> failwith "hui"
    ;;

let rec free_vars =
  let rec helper cxt = function
    | EConst _ | EId (IdentOfBaseOp _ | IdentOfDefinable (IdentOp _)) -> empty
    | EId (IdentOfDefinable (IdentLetters id)) -> return @@ singleton id
    | EIf ((i, _), (t, _), (e, _)) -> apply_to_list helper [ i; t; e ]
    | ETuple ((fst, _), (snd, _), xs) ->
      let without_type = List.map (fun (e, _) -> e) xs in
      let united = List.concat [ [ fst; snd ]; without_type ] in
      List.fold_left
        (fun env v ->
          let env = env in
          let res = helper env v in
          return (union_all [ res; env ]))
        (return empty)
        united
    | EApp ((e1, _), (e2, _)) ->
      let e1_res = helper cxt e1 in
      let e2_res = helper cxt e2 in
      union e1_res e2_res
    | EList ((hd, _), (tl, _)) ->
      let hd_res = helper cxt hd in
      let tl_res = helper cxt tl in
      union hd_res tl_res
    | EMatch ((e, _), hd, tl) ->
      let united = List.concat [ [ hd ]; tl ] in
      let exprs = List.map (fun (_, (e, _)) -> e) united in
      apply_to_list helper (e :: exprs)
    | EFun ((pat, _), (e, _)) ->
      let e_res = helper cxt e in
      let idents = get_idents pat in
       diff e_res idents
    | EClsr (DLet (rec_flag, ((POpPat pat, pat_typ), _)), ((EFun (_, _), _) as expr_typed))
      -> helper cxt (EFun ((pat, pat_typ), expr_typed))
    (* | EClsr
       ( DLetMut (_, ((POpPat pat1, pat1_typ), _), ((POpPat pat2, pat2_typ), _), tl)
       , ((EFun (_, _), _) as expr_typed) ) ->
       helper cxt (EFun ((pat, pat_typ), expr_typed)) *)
    (* |EClsr (decl, (e, _)) -> *)
    | _ -> failwith " "
  in
  helper empty

let closure_conversion prog =
  let closure_converted = List.map (fun decl -> free_vars decl) prog in
  closure_converted
;;
