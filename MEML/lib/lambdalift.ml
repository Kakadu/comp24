open Cast
open Base
open Ast
open Llast

let set_empty = Set.empty (module String)
let map_empty = Map.empty (module String)

let get_uniq_name gvars name =
  let rec helper gvars id =
    let name = String.concat [ name; Int.to_string id ] in
    if Set.mem gvars name then helper gvars (id + 1) else name
  in
  if Set.mem gvars name then helper gvars 0 else name
;;

let update_name name local_vars all_vars =
  let new_name = get_uniq_name all_vars name in
  let update_lvars = Map.set local_vars ~key:name ~data:new_name in
  let update_all_vars = Set.add all_vars new_name in
  new_name, update_lvars, update_all_vars
;;

let update_all_name_list name_list vars all_vars =
  let new_name_list, new_vars, new_all_vars =
    List.fold
      ~init:([], vars, all_vars)
      ~f:(fun (acc, acc_lvars, acc_fvars) n ->
        let new_name, new_vars, new_all_vars =
          if Poly.( = ) n "()"
          then update_name "unit" acc_lvars acc_fvars
          else update_name n acc_lvars acc_fvars
        in
        new_name :: acc, new_vars, new_all_vars)
      name_list
  in
  List.rev new_name_list, new_vars, new_all_vars
;;

let update_name_list name_list vars all_vars =
  let new_name_list, new_vars, new_all_vars =
    List.fold
      ~init:([], vars, all_vars)
      ~f:(fun (acc, acc_vars, acc_all_vars) n ->
        let new_name, new_vars, new_all_vars =
          if Poly.( = ) n "()"
          then n, acc_vars, acc_all_vars
          else update_name n acc_vars acc_all_vars
        in
        new_name :: acc, new_vars, new_all_vars)
      name_list
  in
  List.rev new_name_list, new_vars, new_all_vars
;;

let rec pattern_to_string_list = function
  | PWild -> []
  | PConst _ -> []
  | PVar (name, _) -> [ name ]
  | PTuple patterns -> List.concat_map patterns ~f:pattern_to_string_list
  | PCon (hd, tl) -> pattern_to_string_list hd @ pattern_to_string_list tl
;;

let rec lift_llexpression new_lllet gvars fvars lvars = function
  | CVar v ->
    let new_vars =
      match Map.find lvars v with
      | Some p -> LLVar p
      | None ->
        (match Map.find gvars v with
         | Some p -> LLVar p
         | None -> LLVar v)
    in
    new_vars, new_lllet, fvars
  | CConst c -> LLConst c, new_lllet, fvars
  | CIfElse (i, t, e) ->
    let check_i, new_lllet, fvars = lift_llexpression new_lllet gvars fvars lvars i in
    let check_t, new_lllet, fvars = lift_llexpression new_lllet gvars fvars lvars t in
    let check_e, new_lllet, fvars = lift_llexpression new_lllet gvars fvars lvars e in
    LLIfElse (check_i, check_t, check_e), new_lllet, fvars
  | CEbinOp (op, e1, e2) ->
    let check_e1, new_lllet, fvars = lift_llexpression new_lllet gvars fvars lvars e1 in
    let check_e2, new_lllet, fvars = lift_llexpression new_lllet gvars fvars lvars e2 in
    LLEbinOp (op, check_e1, check_e2), new_lllet, fvars
  | CApp (l, r) ->
    let check_l, new_lllet, fvars = lift_llexpression new_lllet gvars fvars lvars l in
    let check_r, new_lllet, fvars = lift_llexpression new_lllet gvars fvars lvars r in
    LLApp (check_l, check_r), new_lllet, fvars
  | CLetIn (r, n_list, args, e, ine) ->
    let count_unit =
      List.fold_left
        ~init:0
        ~f:(fun acc name -> if String.( = ) name "()" then acc + 1 else acc)
        n_list
    in
    let new_names, new_lvars, new_fvars =
      if count_unit = List.length n_list
      then update_all_name_list n_list lvars fvars
      else update_name_list n_list lvars fvars
    in
    let lle, new_lllet, new_fvars =
      if Poly.( = ) r Rec
      then lift_llexpression new_lllet gvars new_fvars new_lvars e
      else lift_llexpression new_lllet gvars new_fvars lvars e
    in
    let new_lllet =
      if List.length new_names = count_unit || List.length new_names = 1
      then LLLet (r, new_names, args, lle) :: new_lllet
      else new_lllet
    in
    let drop_letin, new_lllet, fvars =
      lift_llexpression new_lllet gvars new_fvars new_lvars ine
    in
    let new_body =
      if List.length new_names <> count_unit
      then drop_letin
      else List.fold ~init:drop_letin ~f:(fun acc n -> LLVars (LLVar n, acc)) @@ List.rev new_names
    in
    let new_lle =
      if List.length new_names = count_unit || List.length new_names = 1
      then new_body
      else LLLetIn (new_names, args, lle, new_body)
    in
    new_lle, new_lllet, fvars
  | CTuple t ->
    let llt, new_lllet, fvars =
      List.fold
        ~init:([], new_lllet, fvars)
        ~f:(fun (acc, acc_new_lllet, fvars) c ->
          let ll, new_let, fvars = lift_llexpression acc_new_lllet gvars fvars lvars c in
          ll :: acc, new_let, fvars)
        t
    in
    LLTuple (List.rev llt), new_lllet, fvars
  | CMatch (m, b) ->
    let llm, new_lllet, fvars = lift_llexpression new_lllet gvars fvars lvars m in
    let llb, new_lllet, fvars =
      List.fold
        ~init:([], new_lllet, fvars)
        ~f:(fun (acc, acc_nllet, fvars) (p, ce) ->
          let lle, new_lllet, fvars = lift_llexpression acc_nllet gvars fvars lvars ce in
          (p, lle) :: acc, new_lllet, fvars)
        b
    in
    LLMatch (llm, llb), new_lllet, fvars
  | CList (hd, tl) ->
    let llhd, new_lllet, fvars = lift_llexpression new_lllet gvars fvars lvars hd in
    let lltl, new_lllet, fvars = lift_llexpression new_lllet gvars fvars lvars tl in
    LLList (llhd, lltl), new_lllet, fvars
;;

let lift_llbindings gvars fvars = function
  | CLet llbindings ->
    let new_lets, old_lets, gvars, fvars =
      List.fold
        ~init:([], [], gvars, fvars)
        ~f:(fun (acc_new, acc_old, gvars, fvars) (r, n_list, args, e) ->
          let new_names, gvars, fvars =
            if Poly.( = ) r Rec
            then update_all_name_list n_list gvars fvars
            else n_list, gvars, fvars
          in
          let str_args =
            List.fold ~init:[] ~f:(fun acc arg -> acc @ pattern_to_string_list arg) args
          in
          let new_args, lvars, new_fvars = update_name_list str_args map_empty fvars in
          let old_lets, new_lets, update_fvars =
            lift_llexpression acc_new gvars new_fvars lvars e
          in
          let fvars = Set.union fvars (Set.diff update_fvars new_fvars) in
          let new_names, gvars, fvars =
            if Poly.( <> ) r Rec
            then update_all_name_list n_list gvars fvars
            else new_names, gvars, fvars
          in
          let new_args =
            if Poly.( = ) new_args str_args
            then args
            else
              List.fold ~init:[] ~f:(fun acc arg -> PVar (arg, TUnknown) :: acc) new_args
          in
          ( acc_new @ new_lets
          , LLLet (r, new_names, new_args, old_lets) :: acc_old
          , gvars
          , fvars ))
        llbindings
    in
    List.rev new_lets @ old_lets, gvars, fvars
  | CExpression _ -> failwith "las;"
;;

let lift_statments statments =
  List.fold
    ~init:([], map_empty, set_empty)
    ~f:(fun (acc, gvars, fvars) llbinding ->
      let llbindings, gvars, fvars = lift_llbindings gvars fvars llbinding in
      acc @ llbindings, gvars, fvars)
    statments
;;

let rec find_free gvars new_app = function
  | LLVar v ->
    let a =
      match Map.find new_app v with
      | Some p -> List.fold ~init:(LLVar v) ~f:(fun acc a -> LLApp (acc, LLVar a)) p
      | None -> LLVar v
    in
    a, Set.add gvars v
  | LLConst c -> LLConst c, gvars
  | LLApp (e1, e2) ->
    let new_e1, update_gvars = find_free gvars new_app e1 in
    let new_e2, update_gvars = find_free update_gvars new_app e2 in
    LLApp (new_e1, new_e2), update_gvars
  | LLEbinOp (op, e1, e2) ->
    let new_e1, new_gvars = find_free gvars new_app e1 in
    let new_e2, new_gvars = find_free new_gvars new_app e2 in
    LLEbinOp (op, new_e1, new_e2), new_gvars
  | LLIfElse (i, t, e) ->
    let new_i, new_gvars = find_free gvars new_app i in
    let new_t, new_gvars = find_free new_gvars new_app t in
    let new_e, new_gvars = find_free new_gvars new_app e in
    LLIfElse (new_i, new_t, new_e), new_gvars
  | LLTuple t ->
    let new_t, new_gvars =
      List.fold
        ~init:([], gvars)
        ~f:(fun (acc_t, acc_gvars) e ->
          let new_e, new_gvars = find_free acc_gvars new_app e in
          new_e :: acc_t, new_gvars)
        t
    in
    LLTuple (List.rev new_t), new_gvars
  | LLVars (hd, tl) ->
    let new_hd, new_gvars = find_free gvars new_app hd in
    let new_tl, new_gvars = find_free new_gvars new_app tl in
    LLVars (new_hd, new_tl), new_gvars
  | LLList (hd, tl) ->
    let new_hd, new_gvars = find_free gvars new_app hd in
    let new_tl, new_gvars = find_free new_gvars new_app tl in
    LLList (new_hd, new_tl), new_gvars
  | LLMatch (m, b) ->
    let new_m, new_gvars = find_free gvars new_app m in
    let new_b, new_gvars =
      List.fold
        ~init:([], new_gvars)
        ~f:(fun (acc, acc_gvars) (p, e) ->
          let sp = pattern_to_string_list p in
          let new_acc_gvars =
            List.fold ~init:acc_gvars ~f:(fun acc_gvars pn -> Set.add acc_gvars pn) sp
          in
          let new_e, new_gvars = find_free new_acc_gvars new_app e in
          let new_gvars =
            List.fold ~init:new_gvars ~f:(fun acc_gvars pn -> Set.remove acc_gvars pn) sp
          in
          (p, new_e) :: acc, new_gvars)
        b
    in
    LLMatch (new_m, new_b), new_gvars
  | LLLetIn (n_list, args, lle, llein) ->
    let new_lle, new_gvars = find_free gvars new_app lle in
    let new_llein, new_gvars = find_free new_gvars new_app llein in
    let new_gvars =
      List.fold ~init:new_gvars ~f:(fun new_gvars n -> Set.remove new_gvars n) n_list
    in
    LLLetIn (n_list, args, new_lle, new_llein), new_gvars
;;

let add_free gvars add_app = function
  | LLLet (r, n_list, args, lle) ->
    let new_llbindings =
      let args_str =
        List.fold
          ~init:[]
          ~f:(fun acc_args arg -> acc_args @ pattern_to_string_list arg)
          args
      in
      let new_args_str = if Poly.( = ) r Rec then n_list @ args_str else args_str in
      let new_gvars =
        List.fold ~init:gvars ~f:(fun acc_gvars n -> Set.add acc_gvars n) new_args_str
      in
      let new_body, new_gvars_args = find_free new_gvars add_app lle in
      let update_args = Set.diff new_gvars_args new_gvars in
      let new_args =
        List.fold
          ~init:args
          ~f:(fun acc_args arg -> [ PVar (arg, TUnknown) ] @ acc_args)
          (List.rev @@ Set.to_list update_args)
      in
      let new_app =
        if not (Set.is_empty update_args)
        then
          List.fold
            ~init:add_app
            ~f:(fun update_app name ->
              Map.set update_app ~key:name ~data:(Set.to_list update_args))
            n_list
        else add_app
      in
      let new_gvars =
        List.fold ~init:gvars ~f:(fun acc_gvars n -> Set.add acc_gvars n) n_list
      in
      (r, n_list, new_args, new_body), new_gvars, new_app
    in
    let (r, n, a, lle), b, c = new_llbindings in
    LLLet (r, n, a, lle), b, c
  | LLExpression _ -> failwith ""
;;

let lambda_lift statments =
  let a, _, _ = lift_statments statments in
  let gvars = Set.add set_empty "print_int" in
  let gvars = Set.add gvars "()" in
  let b, _, _ =
    List.fold
      ~init:([], gvars, map_empty)
      ~f:(fun (acc, gvars, new_app) llbinding ->
        let a, b, c = add_free gvars new_app llbinding in
        a :: acc, b, c)
      a
  in
  List.rev b
;;
