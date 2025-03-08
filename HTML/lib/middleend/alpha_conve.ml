open AstLib.Ast
open Alpha_conv_utils

let get_base_name name = List.hd (String.split_on_char '.' name)

type ident_typ =
  | TopLevel
  | LetIn
  | Pat

type ident_counters =
  { top_level : int
  ; let_in : int
  ; pat : int
  }

module R = struct
  module Env = struct
    let empty = Base.Map.empty (module Base.String)
    let update map key value = Base.Map.update map key ~f:(fun _ -> value)
    let lookup_env key map = Base.Map.find map key
  end

  open Base.Result

  type env = (string, ident_counters, Base.String.comparator_witness) Base.Map.t
  type 'a t = env -> env * ('a, string) Result.t

  let fail error env = env, fail error
  let return value env = env, return value

  let ( >>= ) (monad : 'a t) f env =
    let env, result = monad env in
    match result with
    | Error e -> env, Error e
    | Ok value -> f value env
  ;;

  let ( let* ) = ( >>= )
  let ( >>| ) m f = m >>= fun x -> return @@ f x
  let run m = snd @@ m Env.empty

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

  let zero = { top_level = 0; let_in = 0; pat = 0 }

  let lookup_env_counters name env =
    let n = Env.lookup_env name env in
    match n with
    | Some n -> env, Ok n
    | None -> env, Ok zero
  ;;

  let incr_counter_typ counters ident_typ =
    match ident_typ with
    | LetIn -> { counters with let_in = counters.let_in + 1 }
    | TopLevel -> { counters with top_level = counters.top_level + 1 }
    | Pat -> { counters with pat = counters.pat + 1 }
  ;;

  let incr_counter name ident_typ env =
    let name = get_base_name name in
    let incr_counter counters env =
      let counters = incr_counter_typ counters ident_typ in
      let env = Env.update env name counters in
      env, Ok ()
    in
    let env, counters = lookup_env_counters name env in
    match counters with
    | Ok counters -> incr_counter counters env
    | Error msg -> fail msg env
  ;;

  let update key value env = Env.update env key value, Ok ()
  let get_env env = env, Ok env
end

open R
(*
   let identifier_to_str = function
   | IdConstraint (s, _) -> s
   | Id s -> s
   ;; *)

open Common.Ident_utils

let gen_name_by_counters name counters = function
  | LetIn -> name ^ ".l" ^ string_of_int counters.let_in
  | TopLevel -> name ^ "." ^ string_of_int counters.top_level
  | Pat -> name ^ ".p" ^ string_of_int counters.pat
;;

let gen_name name ident_typ =
  let name = get_base_name name in
  let* counters = lookup_env_counters name in
  return @@ gen_name_by_counters name counters ident_typ
;;

let map_ident_to_str = List.map ident_to_string

let rec get_names_of_pop =
  let rec names_of_pat = function
    | PId id -> [ id ]
    | PTuple (p1, p2, ps) -> List.concat @@ List.map names_of_pat (p1 :: p2 :: ps)
    | PList (hd, tl) -> List.concat @@ List.map names_of_pat [ hd; tl ]
    | PConstraint (pat, _) -> names_of_pat pat
    | _ -> []
  in
  function
  | POpPat pat -> names_of_pat pat
  | POpOp s -> [ s ]
  | POrOpConstraint (p, _) -> get_names_of_pop p
;;

let get_names_of_decl =
  let get_idents_of_lb (pop, _) = get_names_of_pop pop in
  function
  | DLet (_, lb) -> get_idents_of_lb lb
  | DLetMut (_, lb1, lb2, lbs) ->
    List.concat @@ List.map get_idents_of_lb (lb1 :: lb2 :: lbs)
;;

let rec replace_names_of_pop get_new_name =
  let rec replace_names_of_pat = function
    | PId id ->
      let* id' = get_new_name id in
      return @@ pid id'
    | PTuple (p1, p2, ps) ->
      let* p1' = replace_names_of_pat p1 in
      let* p2' = replace_names_of_pat p2 in
      let* ps' = map replace_names_of_pat ps in
      return @@ ptuple p1' p2' ps'
    | PConst _ as p -> return p
    | PList (p1, p2) ->
      let* p1' = replace_names_of_pat p1 in
      let* p2' = replace_names_of_pat p2 in
      return @@ plist p1' p2'
    | PConstraint (p, _) -> replace_names_of_pat p
  in
  function
  | POpPat pat ->
    let* pat' = replace_names_of_pat pat in
    return @@ pop_pat pat'
  | POpOp s ->
    let* s' = get_new_name s in
    return @@ pop_op s'
  | POrOpConstraint (p, _) -> replace_names_of_pop get_new_name p
;;

let get_new_name name ident_typ old_name =
  if old_name = get_base_name name
  then
    let* _ = incr_counter name ident_typ in
    let* new_name = gen_name name ident_typ in
    return new_name
  else return old_name
;;

let rec find_replace_occurs_expr name new_name ident_typ e =
  let rec helper = function
    | a when get_base_name name <> name -> return a
    | EConst _ as e -> return e
    | EId id ->
      let name_of_id = ident_to_string id in
      let* new_id' =
        if name_of_id = name
        then (
          let new_id = get_new_id new_name id in
          return new_id)
        else return id
      in
      return @@ eid new_id'
    | EIf (e1, e2, e3) ->
      let* e1' = helper e1 in
      let* e2' = helper e2 in
      let* e3' = helper e3 in
      return @@ eif e1' e2' e3'
    | EApp (e1, e2) ->
      let* e1' = helper e1 in
      let* e2' = helper e2 in
      return @@ eapp e1' e2'
    | ETuple (e1, e2, es) ->
      let* e1' = helper e1 in
      let* e2' = helper e2 in
      let* es' = map helper es in
      return @@ etuple e1' e2' es'
    | EList (e1, e2) ->
      let* e1' = helper e1 in
      let* e2' = helper e2 in
      return @@ elist e1' e2'
    | EConstraint (e, typ) ->
      let* e' = helper e in
      return @@ e_typed ~typ:(Some typ) e'
    | EFun (pat, e) ->
      let* pat', e' = find_replace_occurs_lb_pat name new_name Pat (pat, e) in
      return @@ efun pat' e'
    | EMatch (e, br, brs) ->
      let* e' = helper e in
      let* br', brs' =
        let find_replace_occurs_lb_pat = find_replace_occurs_lb_pat name new_name Pat in
        let* br' = find_replace_occurs_lb_pat br in
        let* brs' = map find_replace_occurs_lb_pat brs in
        return (br', brs')
      in
      return @@ ematch e' br' brs'
    | EClsr ((DLet (rec_flag, (pop, e1)) as decl), e2) ->
      let names_letin = get_names_of_decl decl @ [ "" ] in
      let get_lb = function
        | DLet (_, lb) -> return lb
        | _ -> fail "Unexpected mut rec decl"
      in
      let rec iter_over_names decl e2 = function
        | next_name :: tl ->
          if next_name = name
          then
            let* e1' = find_replace_occurs_expr name new_name LetIn e1 in
            let* pop' = replace_names_of_pop (get_new_name name LetIn) pop in
            let* new_name = gen_name name LetIn in
            let* e2' = find_replace_occurs_expr name new_name LetIn e2 in
            iter_over_names (dlet rec_flag (pop', e1')) e2' tl
          else
            let* pop', e1 = get_lb decl in
            let* pop' = replace_names_of_pop (get_new_name next_name LetIn) pop' in
            let* new_next_name_letin =
              if next_name = "" then return "" else gen_name next_name LetIn
            in
            let* e1' = find_replace_occurs_expr name new_name ident_typ e1 in
            let* e2' = find_replace_occurs_expr name new_name ident_typ e2 in
            let* e2' =
              if next_name = ""
              then return e2'
              else find_replace_occurs_expr next_name new_next_name_letin LetIn e2'
            in
            let decl'' = dlet rec_flag (pop', e1') in
            iter_over_names decl'' e2' tl
        | [] -> return @@ eclsr decl e2
      in
      iter_over_names decl e2 names_letin
    | _ -> fail "Not implemented"
  in
  helper e

and find_replace_occurs_lb_pat name new_name ident_typ ((pat : pattern), expr) =
  let pop_from_pat = POpPat pat in
  let* pop', e' = find_replace_occurs_lb name new_name ident_typ (pop_from_pat, expr) in
  let* pat' =
    match pop' with
    | POpPat pat -> return pat
    | _ ->
      fail
      @@ Format.asprintf
           "Non-pattern value was returned from find_replace_occurs_lb with %a?!"
           AstLib.Pp_ast.pp_pattern
           pat
  in
  return (pat', e')

and find_replace_occurs_lb name new_name ident_typ ((pop : pattern_or_op), expr) =
  let* pop' = replace_names_of_pop (get_new_name name ident_typ) pop in
  let* e' = find_replace_occurs_expr name new_name ident_typ expr in
  return (pop', e')

and find_replace_occurs_decl name ident_typ = function
  | a when get_base_name name <> name -> return a
  | DLet (Not_recursive, lb) ->
    let* new_name = gen_name name ident_typ in
    let* lb' = find_replace_occurs_lb name new_name ident_typ lb in
    return @@ dlet Not_recursive lb'
  | DLet (Recursive, ((pop, _) as lb)) ->
    let a = get_names_of_pop pop in
    let* name_of_decl =
      match a with
      | hd :: [] -> return hd
      | _ -> fail @@ Format.sprintf "Tuples and lists are not supported"
    in
    let* counters = lookup_env_counters name in
    let* counters =
      if name_of_decl = name
      then (
        let counters = incr_counter_typ counters ident_typ in
        return counters)
      else return counters
    in
    let new_name = gen_name_by_counters name counters ident_typ in
    let* lb' = find_replace_occurs_lb name new_name ident_typ lb in
    return @@ dlet Recursive lb'
  | DLetMut (Recursive, lb1, lb2, lbs) as dlet ->
    let names = get_names_of_decl dlet in
    if not @@ List.mem name names
    then return dlet
    else
      let* counters = lookup_env_counters name in
      let counters = incr_counter_typ counters ident_typ in
      let new_name = gen_name_by_counters name counters TopLevel in
      let* lb1' = find_replace_occurs_lb name new_name ident_typ lb1 in
      let* lb2' = find_replace_occurs_lb name new_name ident_typ lb2 in
      let* lb_tl' = map (find_replace_occurs_lb name new_name ident_typ) lbs in
      return @@ dletmut Recursive lb1' lb2' lb_tl'
  | _ -> fail "Non recursive mutual decls are not supported"
;;

let find_replace_occurs_decls name prog =
  let rec helper acc = function
    | decl :: tl ->
      let names = get_names_of_decl decl in
      if List.mem name names
      then return (List.concat [ List.rev tl; decl :: acc ])
      else
        let* decl' = find_replace_occurs_decl name TopLevel decl in
        let acc = decl' :: acc in
        helper acc tl
    | [] -> return acc
  in
  let* res_inv = helper [] prog in
  return (List.rev res_inv)
;;

let alpha_convert_prog prog =
  let rec helper acc = function
    | decl :: decls ->
      let names = get_names_of_decl decl in
      let rec fold_over_idents decl decls = function
        | name :: tl ->
          let* decl' = find_replace_occurs_decl name TopLevel decl in
          let* decls' = find_replace_occurs_decls name decls in
          fold_over_idents decl' decls' tl
        | [] -> return (decl, decls)
      in
      let* decl', decls' = fold_over_idents decl decls names in
      let acc = decl' :: acc in
      helper acc decls'
    | [] -> return acc
  in
  let* res_inv = helper [] prog in
  return (List.rev res_inv)
;;

let init_env = Common.Stdlib.stdlib

let alpha_convert_prog prog =
  let run_env_filling =
    let rec fill_env = function
      | hd :: tl ->
        let* _ = update hd zero in
        fill_env tl
      | [] -> return ()
    in
    fill_env init_env
  in
  run
    (let* _ = run_env_filling in
     let* res = alpha_convert_prog prog in
     return (List.map Alpha_conv_utils.revert_decl res))
;;
