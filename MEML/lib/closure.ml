open Ast
open Base
open Llast

module NAMECOUNTER = struct
  let return value id = value, id

  let ( let* ) m f id =
    let value, mid = m id in
    f mid value
  ;;

  let curent_id id = id, id
  let update_id id _ = id + 1, ()
  let run f = f
end

open NAMECOUNTER

let set_empty = Set.empty (module String)
let map_empty = Map.empty (module String)

let rec get_name gvars =
  let* id = curent_id in
  let* () = update_id id in
  let name = String.concat [ "lambada"; Int.to_string id ] in
  if Set.mem gvars name then get_name gvars else return name
;;

let rec arguments acc = function
  | EFun (p, e) -> arguments (p :: acc) e
  | e -> List.rev acc, e
;;

let rec create_let acc = function
  | [] -> acc
  | (n, args, lle) :: tl -> LLLetIn (Notrec, [ n ], args, lle, create_let acc tl)
;;

let rec closure_expression id gvars new_lets = function
  | EVar (v, _) -> LLVar v, new_lets, id
  | EConst c -> LLConst c, new_lets, id
  | EBinaryOp (op, e1, e2) ->
    let lle1, lets1, id = closure_expression id gvars new_lets e1 in
    let lle2, lets2, id = closure_expression id gvars new_lets e2 in
    LLEbinOp (op, lle1, lle2), new_lets @ lets1 @ lets2, id
  | EApp (e1, e2, _) ->
    let lle1, lets1, id = closure_expression id gvars new_lets e1 in
    let lle2, lets2, id = closure_expression id gvars new_lets e2 in
    LLApp (lle1, lle2), new_lets @ lets1 @ lets2, id
  | EList (hd, tl) ->
    let lle1, lets1, id = closure_expression id gvars new_lets hd in
    let lle2, lets2, id = closure_expression id gvars new_lets tl in
    LLList (lle1, lle2), new_lets @ lets1 @ lets2, id
  | ETuple t ->
    let llt, lets, id =
      List.fold
        ~init:([], [], id)
        ~f:(fun (acc, let_acc, id) e ->
          let c, l, id = closure_expression id gvars new_lets e in
          c :: acc, l @ let_acc, id)
        t
    in
    LLTuple (List.rev llt), new_lets @ lets, id
  | EIfElse (i, t, e) ->
    let lli, letsi, id = closure_expression id gvars new_lets i in
    let llt, letst, id = closure_expression id gvars new_lets t in
    let lle, letse, id = closure_expression id gvars new_lets e in
    LLIfElse (lli, llt, lle), new_lets @ letsi @ letst @ letse, id
  | ELetIn (r, n_list, e, ine) ->
    let args, e_body = arguments [] e in
    let lle, letse, id = closure_expression id gvars new_lets e_body in
    let lline, letsine, id = closure_expression id gvars [] ine in
    create_let (LLLetIn (r, n_list, args, lle, lline)) (new_lets @ letse), letsine, id
  | EFun (p, e) ->
    let args, next_e = arguments [] (EFun (p, e)) in
    let name, new_id = run (get_name gvars) id in
    let clo, lets, id = closure_expression new_id gvars new_lets next_e in
    let llletin = name, args, clo in
    LLVar name, new_lets @ lets @ [ llletin ], id
  | EMatch (m, a) ->
    let lla, letsa, id =
      List.fold
        ~init:([], new_lets, id)
        ~f:(fun (acc, acc_lets, id) (p, e) ->
          let lle, letse, id = closure_expression id gvars new_lets e in
          (p, lle) :: acc, acc_lets @ letse, id)
        a
    in
    let llm, letsm, id = closure_expression id gvars new_lets m in
    LLMatch (llm, List.rev lla), letsa @ letsm, id
;;

let existence set_vars v = Set.mem set_vars v
let find gvars lvars v = existence lvars v || existence gvars v

let rec pattern_to_string_list = function
  | PWild -> []
  | PConst _ -> []
  | PVar (name, _) -> [ name ]
  | PTuple patterns -> List.concat_map patterns ~f:pattern_to_string_list
  | PCon (hd, tl) -> pattern_to_string_list hd @ pattern_to_string_list tl
;;

let first_element_in_other lst1 lst2 =
  match lst1 with
  | [] -> false
  | hd :: _ -> List.mem lst2 hd ~equal:String.( = )
;;

let rec free_vars gvars lvars uvars lambadi = function
  | LLConst c -> LLConst c, gvars, lvars, uvars, []
  | LLVar v ->
    let new_app, new_vars =
      match Map.find uvars v with
      | Some p -> List.fold ~init:(LLVar v) ~f:(fun acc a -> LLApp (acc, LLVar a)) p, p
      | None -> LLVar v, [ v ]
    in
    let new_args =
      List.fold
        ~init:[]
        ~f:(fun acc new_v -> if find gvars lvars v then acc else new_v :: acc)
        new_vars
    in
    new_app, gvars, lvars, uvars, new_args
  | LLApp (l, r) ->
    let l, gvars, lvars, uvars, new_argsl = free_vars gvars lvars uvars lambadi l in
    let r, gvars, lvars, uvars, new_argsr = free_vars gvars lvars uvars lambadi r in
    LLApp (l, r), gvars, lvars, uvars, new_argsl @ new_argsr
  | LLEbinOp (op, l, r) ->
    let l, gvars, lvars, uvars, new_argsl = free_vars gvars lvars uvars lambadi l in
    let r, gvars, lvars, uvars, new_argsr = free_vars gvars lvars uvars lambadi r in
    LLEbinOp (op, l, r), gvars, lvars, uvars, new_argsl @ new_argsr
  | LLIfElse (i, t, e) ->
    let i, gvars, lvars, uvars, new_argsi = free_vars gvars lvars uvars lambadi i in
    let t, gvars, lvars, uvars, new_argst = free_vars gvars lvars uvars lambadi t in
    let e, gvars, lvars, uvars, new_argse = free_vars gvars lvars uvars lambadi e in
    LLIfElse (i, t, e), gvars, lvars, uvars, new_argsi @ new_argst @ new_argse
  | LLLetIn (r, n_list, args, lle, lline) ->
    let string_args =
      List.rev (List.fold ~init:[] ~f:(fun acc p -> pattern_to_string_list p @ acc) args)
    in
    let new_lvars =
      List.fold string_args ~init:lvars ~f:(fun acc arg -> Set.add acc arg)
    in
    let new_gvars = List.fold ~init:gvars ~f:(fun acc n -> Set.add acc n) n_list in
    let lle, _, _, uvars, new_argslle = free_vars gvars new_lvars uvars lambadi lle in
    let new_uvars =
      if Poly.( <> ) new_argslle [] && first_element_in_other n_list lambadi
      then
        List.fold
          ~init:uvars
          ~f:(fun acc n -> Map.set acc ~key:n ~data:new_argslle)
          n_list
      else uvars
    in
    let new_args =
      if first_element_in_other n_list lambadi
      then
        List.fold ~init:[] ~f:(fun acc v -> PVar (v, TUnknown) :: acc) new_argslle @ args
      else args
    in
    let lline, _, _, new_uvars, _ =
      free_vars new_gvars new_lvars new_uvars lambadi lline
    in
    LLLetIn (r, n_list, new_args, lle, lline), new_gvars, lvars, new_uvars, []
  | LLList (hd, tl) -> LLList (hd, tl), gvars, lvars, uvars, []
  | LLTuple t ->
    let new_t, new_uvars, new_args =
      List.fold
        ~init:([], uvars, [])
        ~f:(fun (acct, acc_uvars, acc_args) e ->
          let new_t, _, _, new_uvars, new_arg = free_vars gvars lvars acc_uvars lambadi e in
          new_t :: acct, new_uvars, acc_args @ new_arg)
        t
    in
    LLTuple (List.rev new_t), gvars, lvars, new_uvars, new_args
  | LLMatch (m, b) ->
    let new_m, gvars, lvars, uvars, new_args = free_vars gvars lvars uvars lambadi m in
    let new_b, new_gvars, new_lvars, new_uvars, new_args =
      List.fold
           ~init:([], gvars, lvars, uvars, new_args)
           ~f:(fun (acc, gacc, lacc, uacc, aacc) (p, e) ->
             let check_e, new_gacc, new_lacc, new_uacc, new_aacc =
               free_vars gacc lacc uacc lambadi e
             in
             (p, check_e) :: acc, new_gacc, new_lacc, new_uacc, aacc @ new_aacc)
           b
    in
    LLMatch (new_m, List.rev new_b), new_gvars, new_lvars, new_uvars, new_args
;;

let closure_bindings id = function
  | Let bindings ->
    let llbindings, _ =
      List.fold
        ~init:([], id)
        ~f:(fun (lacc, id) (r, n_list, e) ->
          let args, e_body = arguments [] e in
          let llet, letfun, id = closure_expression id set_empty [] e_body in
          let llet = create_let llet letfun in
          let gvars =
            if Poly.( = ) r Notrec
            then set_empty
            else List.fold ~init:set_empty ~f:(fun acc n -> Set.add acc n) n_list
          in
          let str_args =
            List.fold ~init:[] ~f:(fun acc p -> acc @ pattern_to_string_list p) args
          in
          let lvars =
            List.fold ~init:set_empty ~f:(fun acc arg -> Set.add acc arg) str_args
          in
          let lambadi =
            List.rev @@ List.fold ~init:[] ~f:(fun acc (name, _, _) -> name :: acc) letfun
          in
          let not_free_llet, _, _, _, _ = free_vars gvars lvars map_empty lambadi llet in
          let lllet = r, n_list, args, not_free_llet in
          lllet :: lacc, id)
        bindings
    in
    LLLet (List.rev llbindings)
  | _ -> failwith ""
;;

let closure_statments statements =
  List.fold
    ~init:([], 0)
    ~f:(fun (acc, id) bindings -> closure_bindings id bindings :: acc, id)
    statements
;;

let closure statements =
  let closure, _ = closure_statments statements in
  List.rev closure
;;
