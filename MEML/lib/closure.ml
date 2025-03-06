open Ast
open Base
open Cast

let set_empty = Set.empty (module String)
let map_empty = Map.empty (module String)

let rec get_name gvars id =
  let name = String.concat [ "lambada"; Int.to_string id ] in
  if Set.mem gvars name then get_name gvars (id + 1) else name, id
;;

let rec arguments acc = function
  | EFun (p, e) -> arguments (p :: acc) e
  | e -> List.rev acc, e
;;

let rec create_let acc = function
  | [] -> acc
  | (n, args, lle) :: tl -> CLetIn (Notrec, n, args, lle, create_let acc tl)
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

let rec closure_expression id gvars lvars new_lets = function
  | EVar (v, _) -> CVar v, new_lets, id, gvars, lvars, []
  | EConst c -> CConst c, new_lets, id, gvars, lvars, []
  | EBinaryOp (op, e1, e2) ->
    let lle1, lets1, id, gvars, lvars, lambadi1 =
      closure_expression id gvars lvars new_lets e1
    in
    let lle2, lets2, id, gvars, lvars, lambadi2 =
      closure_expression id gvars lvars new_lets e2
    in
    ( CEbinOp (op, lle1, lle2)
    , new_lets @ lets1 @ lets2
    , id
    , gvars
    , lvars
    , lambadi1 @ lambadi2 )
  | EApp (e1, e2, _) ->
    let lle1, lets1, id, gvars, lvars, lambadi1 =
      closure_expression id gvars lvars new_lets e1
    in
    let lle2, lets2, id, gvars, lvars, lambadi2 =
      closure_expression id gvars lvars new_lets e2
    in
    CApp (lle1, lle2), new_lets @ lets1 @ lets2, id, gvars, lvars, lambadi1 @ lambadi2
  | EList (hd, tl) ->
    let lle1, lets1, id, gvars, lvars, lambadi1 =
      closure_expression id gvars lvars new_lets hd
    in
    let lle2, lets2, id, gvars, lvars, lambadi2 =
      closure_expression id gvars lvars new_lets tl
    in
    CList (lle1, lle2), new_lets @ lets1 @ lets2, id, gvars, lvars, lambadi1 @ lambadi2
  | ETuple t ->
    let llt, lets, id, gvars, lvars, lambadi =
      List.fold
        ~init:([], [], id, gvars, lvars, [])
        ~f:(fun (acc, let_acc, id, gvars, lvars, lamb) e ->
          let c, l, id, gv, _, la = closure_expression id gvars lvars new_lets e in
          c :: acc, l @ let_acc, id, Set.union gvars gv, lvars, lamb @ la)
        t
    in
    CTuple (List.rev llt), new_lets @ lets, id, gvars, lvars, lambadi
  | EIfElse (i, t, e) ->
    let lli, letsi, id, gvars, lvars, lambadi1 =
      closure_expression id gvars lvars new_lets i
    in
    let llt, letst, id, gvars, lvars, lambadi2 =
      closure_expression id gvars lvars new_lets t
    in
    let lle, letse, id, gvars, lvars, lambadi3 =
      closure_expression id gvars lvars new_lets e
    in
    ( CIfElse (lli, llt, lle)
    , new_lets @ letsi @ letst @ letse
    , id
    , gvars
    , lvars
    , lambadi1 @ lambadi2 @ lambadi3 )
  | ELetIn (r, n_list, e, ine) ->
    let args, e_body = arguments [] e in
    let str_args =
      List.fold ~init:[] ~f:(fun acc arg -> acc @ pattern_to_string_list arg) args
    in
    let lle, letse, id, gvars, lvars, lambadi1 =
      closure_expression id gvars str_args new_lets e_body
    in
    let lline, letsine, id, gvars, lvars, lambadi2 =
      closure_expression id gvars lvars [] ine
    in
    ( create_let (CLetIn (r, n_list, args, lle, lline)) (new_lets @ letse)
    , letsine
    , id
    , gvars
    , lvars
    , lambadi1 @ lambadi2 )
  | ELetPatIn (names, e, ine) ->
    let lle, letse, id, gvars, lvars, lambadi1 =
      closure_expression id gvars [] new_lets e
    in
    let lline, letsine, id, gvars, lvars, lambadi2 =
      closure_expression id gvars lvars [] ine
    in
    ( create_let (CPatLetIn (names, lle, lline)) (new_lets @ letse)
    , letsine
    , id
    , gvars
    , lvars
    , lambadi1 @ lambadi2 )
  | EFun (p, e) ->
    let args, next_e = arguments [] (EFun (p, e)) in
    let name, new_id = get_name gvars id in
    let gvars = Set.add gvars name in
    let clo, lets, id, gvars, lvars, lambadi =
      closure_expression new_id gvars lvars new_lets next_e
    in
    let llletin = name, args, clo in
    CVar name, new_lets @ lets @ [ llletin ], id, gvars, lvars, name :: lambadi
  | EMatch (m, a) ->
    let lla, letsa, id, gvars, lvars, lambadi =
      List.fold
        ~init:([], new_lets, id, gvars, lvars, [])
        ~f:(fun (acc, acc_lets, id, acc_g, lv, lambadi) (p, e) ->
          let lle, letse, id, gv, _, la = closure_expression id acc_g lvars new_lets e in
          (p, lle) :: acc, acc_lets @ letse, id, Set.union gv acc_g, lv, lambadi @ la)
        a
    in
    let llm, letsm, id, gvars, lvars, ll = closure_expression id gvars lvars new_lets m in
    CMatch (llm, List.rev lla), letsa @ letsm, id, gvars, lvars, lambadi @ ll
;;

let rec free_vars gvars lvars uvars lambadi = function
  | CConst c -> CConst c, gvars, lvars, uvars, []
  | CVar v ->
    let new_app, new_vars =
      match Map.find uvars v with
      | Some p -> List.fold ~init:(CVar v) ~f:(fun acc a -> CApp (acc, CVar a)) p, p
      | None -> CVar v, [ v ]
    in
    let new_args =
      List.fold
        ~init:[]
        ~f:(fun acc new_v -> if find gvars lvars v then acc else new_v :: acc)
        new_vars
    in
    new_app, gvars, lvars, uvars, new_args
  | CApp (l, r) ->
    let l, gvars, lvars, uvars, new_argsl = free_vars gvars lvars uvars lambadi l in
    let r, gvars, lvars, uvars, new_argsr = free_vars gvars lvars uvars lambadi r in
    CApp (l, r), gvars, lvars, uvars, new_argsl @ new_argsr
  | CEbinOp (op, l, r) ->
    let l, gvars, lvars, uvars, new_argsl = free_vars gvars lvars uvars lambadi l in
    let r, gvars, lvars, uvars, new_argsr = free_vars gvars lvars uvars lambadi r in
    CEbinOp (op, l, r), gvars, lvars, uvars, new_argsl @ new_argsr
  | CIfElse (i, t, e) ->
    let i, gvars, lvars, uvars, new_argsi = free_vars gvars lvars uvars lambadi i in
    let t, gvars, lvars, uvars, new_argst = free_vars gvars lvars uvars lambadi t in
    let e, gvars, lvars, uvars, new_argse = free_vars gvars lvars uvars lambadi e in
    CIfElse (i, t, e), gvars, lvars, uvars, new_argsi @ new_argst @ new_argse
  | CPatLetIn (names, lle, lline) ->
    let string_names = pattern_to_string_list names in
    let new_gvars = List.fold ~init:gvars ~f:(fun acc n -> Set.add acc n) string_names in
    let lle, _, new_lvars, uvars, _ = free_vars gvars lvars uvars lambadi lle in
    let lline, _, _, new_uvars, _ = free_vars new_gvars new_lvars uvars lambadi lline in
    CPatLetIn (names, lle, lline), new_gvars, lvars, new_uvars, []
  | CLetIn (r, n, args, lle, lline) ->
    let string_args =
      List.rev (List.fold ~init:[] ~f:(fun acc p -> pattern_to_string_list p @ acc) args)
    in
    let new_lvars =
      List.fold string_args ~init:lvars ~f:(fun acc arg -> Set.add acc arg)
    in
    let new_gvars = Set.add gvars n in
    let lle, _, new_lvars, uvars, new_argslle =
      free_vars gvars new_lvars uvars lambadi lle
    in
    let new_uvars =
      if Poly.( <> ) new_argslle [] && List.mem lambadi n ~equal:String.( = )
      then Map.set uvars ~key:n ~data:new_argslle
      else uvars
    in
    let new_args =
      if List.mem lambadi n ~equal:String.( = )
      then
        List.fold
          ~init:[]
          ~f:(fun acc v -> PVar (v, TUnknown) :: acc)
          (List.rev new_argslle)
        @ args
      else args
    in
    let lline, _, _, new_uvars, _ =
      free_vars new_gvars new_lvars new_uvars lambadi lline
    in
    CLetIn (r, n, new_args, lle, lline), new_gvars, lvars, new_uvars, []
  | CList (hd, tl) -> CList (hd, tl), gvars, lvars, uvars, []
  | CTuple t ->
    let new_t, new_uvars, new_args =
      List.fold
        ~init:([], uvars, [])
        ~f:(fun (acct, acc_uvars, acc_args) e ->
          let new_t, _, _, new_uvars, new_arg =
            free_vars gvars lvars acc_uvars lambadi e
          in
          new_t :: acct, new_uvars, acc_args @ new_arg)
        t
    in
    CTuple (List.rev new_t), gvars, lvars, new_uvars, new_args
  | CMatch (m, b) ->
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
    CMatch (new_m, List.rev new_b), new_gvars, new_lvars, new_uvars, new_args
;;

let closure_bindings id = function
  | Let (r, bindings) ->
    let names, _ = List.hd_exn bindings in
    let gvars =
      if Poly.( = ) r Notrec
      then set_empty
      else
        List.fold ~init:set_empty ~f:(fun set name -> Set.add set name)
        @@ pattern_to_string_list names
    in
    let llbindings, _ =
      List.fold
        ~init:([], id)
        ~f:(fun (lacc, id) let_type ->
          match let_type with
          | PVar (n, _), e ->
            let args, e_body = arguments [] e in
            let llet, letfun, id, _, _, lambadi =
              closure_expression id set_empty [] [] e_body
            in
            let llet = create_let llet letfun in
            let not_free_llet, _, _, _, _ =
              free_vars gvars set_empty map_empty lambadi llet
            in
            let lllet = CLet (n, args, not_free_llet) in
            lllet :: lacc, id
          | names, e ->
            let llet, letfun, id, _, _, lambadi =
              closure_expression id set_empty [] [] e
            in
            let llet = create_let llet letfun in
            let not_free_llet, _, _, _, _ =
              free_vars set_empty set_empty map_empty lambadi llet
            in
            let lllet = CLetPat (names, not_free_llet) in
            lllet :: lacc, id)
        bindings
    in
    CLets (Rec, List.rev llbindings)
  | Expression e ->
    let llet, letfun, _, _, _, lambadi = closure_expression id set_empty [] [] e in
    let llet = create_let llet letfun in
    let not_free_llet, _, _, _, _ =
      free_vars set_empty set_empty map_empty lambadi llet
    in
    CExpression not_free_llet
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
