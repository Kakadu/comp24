open Cast
open Base
open Ast
open Llast

let set_empty = Set.empty (module String)
let map_empty = Map.empty (module String)

let rec get_name gvars id =
  let name = String.concat [ "unit_"; Int.to_string id ] in
  if Set.mem gvars name then get_name gvars (id + 1) else name, id
;;

let rec lift_llexpression new_lllet units id = function
  | CVar v -> LLVar v, new_lllet, id
  | CConst c -> LLConst c, new_lllet, id
  | CIfElse (i, t, e) ->
    let check_i, new_lllet, id = lift_llexpression new_lllet units id i in
    let check_t, new_lllet, id = lift_llexpression new_lllet units id t in
    let check_e, new_lllet, id = lift_llexpression new_lllet units id e in
    LLIfElse (check_i, check_t, check_e), new_lllet, id
  | CEbinOp (op, e1, e2) ->
    let check_e1, new_lllet, id = lift_llexpression new_lllet units id e1 in
    let check_e2, new_lllet, id = lift_llexpression new_lllet units id e2 in
    LLEbinOp (op, check_e1, check_e2), new_lllet, id
  | CApp (l, r) ->
    let check_l, new_lllet, id = lift_llexpression new_lllet units id l in
    let check_r, new_lllet, id = lift_llexpression new_lllet units id r in
    LLApp (check_l, check_r), new_lllet, id
  | CLetIn (r, n_list, args, e, ine) ->
    let lle, new_lllet, id = lift_llexpression new_lllet units id e in
    let use_names, _ =
      List.fold
        ~init:([], id)
        ~f:(fun (acc, acc_id) n ->
          if Poly.( = ) n "()"
          then (
            let new_name, new_id = get_name units acc_id in
            new_name :: acc, new_id)
          else acc, id)
        n_list
    in
    let new_names, new_id, units =
      List.fold
        ~init:([], id, units)
        ~f:(fun (acc, acc_id, units) n ->
          if Poly.( = ) n "()"
          then (
            let unit_name, new_id = get_name units acc_id in
            let units = Set.add units unit_name in
            unit_name :: acc, new_id, units)
          else n :: acc, acc_id, units)
        n_list
    in
    let new_lllet =
      if List.length new_names = List.length use_names || List.length new_names = 1
      then LLLet [ r, List.rev new_names, args, lle ] :: new_lllet
      else new_lllet
    in
    let drop_letin, new_lllet, new_id = lift_llexpression new_lllet units new_id ine in
    let new_body =
      if List.length use_names = 0
      then drop_letin
      else List.fold ~init:drop_letin ~f:(fun acc n -> LLVars (LLVar n, acc)) use_names
    in
    let new_body =
      if List.length new_names = List.length use_names || List.length new_names = 1
      then new_body
      else LLLetIn (List.rev new_names, args, lle, new_body)
    in
    new_body, new_lllet, new_id
  | CTuple t ->
    let llt, new_lllet, id =
      List.fold
        ~init:([], new_lllet, id)
        ~f:(fun (acc, acc_new_lllet, acc_id) c ->
          let ll, new_let, id = lift_llexpression acc_new_lllet units acc_id c in
          ll :: acc, new_let, id)
        t
    in
    LLTuple (List.rev llt), new_lllet, id
  | CMatch (m, b) ->
    let llm, new_lllet, id = lift_llexpression new_lllet units id m in
    let llb, new_lllet, id =
      List.fold
        ~init:([], new_lllet, id)
        ~f:(fun (acc, acc_nllet, acc_id) (p, ce) ->
          let lle, new_lllet, id = lift_llexpression acc_nllet units acc_id ce in
          (p, lle) :: acc, new_lllet, id)
        b
    in
    LLMatch (llm, llb), new_lllet, id
  | CList (hd, tl) ->
    let llhd, new_lllet, id = lift_llexpression new_lllet units id hd in
    let lltl, new_lllet, id = lift_llexpression new_lllet units id tl in
    LLList (llhd, lltl), new_lllet, id
;;

let lift_llbindings gvars = function
  | CLet llbindings ->
    let new_lets, old_lets =
      List.fold
        ~init:([], [])
        ~f:(fun (acc_new, acc_old) (r, n_list, args, e) ->
          let old_lets, new_lets, _ = lift_llexpression acc_new gvars 0 e in
          acc_new @ new_lets, (r, n_list, args, old_lets) :: acc_old)
        llbindings
    in
    List.rev @@ (LLLet old_lets :: new_lets)
  | CExpression _ -> failwith "las;"
;;

let lift_statments statments =
  List.fold
    ~init:[]
    ~f:(fun acc llbinding -> acc @ lift_llbindings set_empty llbinding)
    statments
;;

let rec pattern_to_string_list = function
  | PWild -> []
  | PConst _ -> []
  | PVar (name, _) -> [ name ]
  | PTuple patterns -> List.concat_map patterns ~f:pattern_to_string_list
  | PCon (hd, tl) -> pattern_to_string_list hd @ pattern_to_string_list tl
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
          let new_gvars = List.fold ~init:new_gvars ~f:(fun acc_gvars pn -> Set.remove acc_gvars pn) sp in
          (p, new_e) :: acc, new_gvars)
        b
    in
    LLMatch (new_m, List.rev new_b), new_gvars
  | LLLetIn (n_list, args, lle, llein) ->
    let new_lle, new_gvars = find_free gvars new_app lle in
    let new_llein, new_gvars = find_free new_gvars new_app llein in
    let new_gvars =
      List.fold ~init:new_gvars ~f:(fun new_gvars n -> Set.remove new_gvars n) n_list
    in
    LLLetIn (n_list, args, new_lle, new_llein), new_gvars
;;

let add_free gvars add_app = function
  | LLLet llbindings ->
    let new_llbindings =
      List.fold
        ~init:([], gvars, add_app)
        ~f:(fun (acc, gvars, acc_add_app) (r, n_list, args, lle) ->
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
          let new_body, new_gvars_args = find_free new_gvars acc_add_app lle in
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
                ~init:acc_add_app
                ~f:(fun update_app name ->
                  Map.set update_app ~key:name ~data:(Set.to_list update_args))
                n_list
            else acc_add_app
          in
          let new_gvars =
            List.fold ~init:gvars ~f:(fun acc_gvars n -> Set.add acc_gvars n) n_list
          in
          (r, n_list, new_args, new_body) :: acc, new_gvars, new_app)
        llbindings
    in
    let a, b, c = new_llbindings in
    LLLet (List.rev a), b, c
  | LLExpression _ -> failwith ""
;;

let lambda_lift statments =
  let a = lift_statments statments in
  let gvars = Set.add set_empty "print_int" in
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
