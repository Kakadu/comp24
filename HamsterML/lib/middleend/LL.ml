open Ast
open Base

type ll_expr =
  | LLConst of value
  | LLVar of id
  | LLOperation of op
  | LLTuple of ll_expr * ll_expr * ll_expr list
  | LLList of ll_expr list
  | LLListConcat of ll_expr * ll_expr
  | LLConstraint of ll_expr * dataType
  | LLApplication of ll_expr * ll_expr
  | LLLet of funType * ll_bind list * ll_expr option
  | LLIf of ll_expr * ll_expr * ll_expr option
  | LLMatch of ll_expr * ll_case list
[@@deriving show]

and ll_bind = pattern * args * ll_expr
and ll_case = pattern * ll_expr
and ll_prog = ll_expr list

module R = struct
  type 'a t = int -> 'a * int

  let fresh st = st, st + 1

  let ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t =
    fun m f state ->
    let value, st = m state in
    (f value) st
  ;;

  let ( >>| ) : 'a t -> ('a -> 'b) -> 'b t =
    fun m f state ->
    let value, st = m state in
    f value, st
  ;;

  let ( let* ) = ( >>= )
  let bind m f = m >>= f
  let return value st = value, st
  let run monad = fst @@ monad 0
end

type id = string

module NameSet = struct
  type t = (id, String.comparator_witness) Set.t

  let empty : t = Set.empty (module String)
  let extend (name : id) (set : t) : t = Set.add set name
  let union : t -> t -> t = Set.union
  let find (name : id) (set : t) = Set.find set ~f:(fun x -> String.equal x name)

  (* (a, b, c) => Var a, Var b, Var c *)
  let to_args (set : t) : args = Set.fold set ~init:[] ~f:(fun acc id -> Var id :: acc)

  let rec generate_name (set : t) =
    let open R in
    let* fresh_num = R.fresh in
    let varname = "LL_arg_" ^ Int.to_string fresh_num in
    match find varname set with
    | None -> R.return (extend varname set, varname)
    | Some _ -> generate_name set
  ;;
end

module NameEnv = struct
  type t = (id, id, String.comparator_witness) Map.t

  let empty : t = Map.empty (module String)
  let find key (env : t) = Map.find env key
  let extend (k, v) (env : t) = Map.set env ~key:k ~data:v

  let rec generate_name (env : t) (name : id) =
    let open R in
    let* fresh_num = R.fresh in
    let varname = "LL_fun_" ^ Int.to_string fresh_num in
    match find varname env with
    | None -> R.return (extend (name, varname) env, varname)
    | Some _ -> generate_name env name
  ;;
end

open R

(* Simplify weird patterns in function arguments *)
let simplify_arguments p_args expr : (expr * NameSet.t) R.t =
  let rec helper (acc : expr) (names : NameSet.t) = function
    | [] -> R.return (acc, names)
    | Var id :: tl -> helper acc (NameSet.extend id names) tl
    | head_p :: tl ->
      let* names, new_name = NameSet.generate_name names in
      (* match <new_name> with head_p -> acc *)
      helper (Match (EVar new_name, [ head_p, acc ])) names tl
  in
  helper expr NameSet.empty p_args
;;

(* 'Let' constructions that should be lifted *)
type lifted_lets = ll_expr list

let rec ll_bind
  (acc : lifted_lets)
  (env : NameEnv.t)
  ((name, args, expr) : string * args * expr)
  =
  match args with
  | [] ->
    let* llexpr, lifted = ll_expr acc env expr in
    let (new_bind : ll_bind) = Var name, [], llexpr in
    R.return (env, new_bind, lifted)
  | args ->
    let* env, new_name = NameEnv.generate_name env name in
    let* expr, nameset = simplify_arguments args expr in
    let* llexpr, lifted = ll_expr acc env expr in
    let (new_bind : ll_bind) = Var new_name, NameSet.to_args nameset, llexpr in
    R.return (env, new_bind, lifted)

and ll_expr (lifted : lifted_lets) (env : NameEnv.t) (expr : expr)
  : (ll_expr * lifted_lets) R.t
  =
  match expr with
  | Let (rec_flag, (Var name, args, expr) :: tl_bind, in_scope) ->
    let* env, new_bind, lifted = ll_bind lifted env (name, args, expr) in
    let* env, new_binds, lifted =
      List.fold
        tl_bind
        ~init:(R.return (env, [ new_bind ], lifted))
        ~f:(fun acc bnd ->
          let* env, binds, lifted = acc in
          match bnd with
          | Var name, args, expr ->
            let* env, new_bind, lifted = ll_bind lifted env (name, args, expr) in
            R.return (env, new_bind :: binds, lifted)
          | _, _, _ -> failwith "Incorrect 'Let' pattern was encountered during LL")
    in
    (match in_scope with
     | Some scope ->
       let* ll_scope, lifted = ll_expr lifted env scope in
       R.return (LLLet (rec_flag, new_binds, Some ll_scope), lifted)
     | None -> R.return (LLLet (rec_flag, new_binds, None), lifted))
  | EConst v -> R.return (LLConst v, lifted)
  | EVar id ->
    (match NameEnv.find id env with
     | None -> R.return (LLVar id, lifted)
     | Some new_id -> R.return (LLVar new_id, lifted))
  | EList exprs ->
    let* ll_exprs, lifted =
      List.fold
        exprs
        ~init:(R.return ([], lifted))
        ~f:(fun acc cur_expr ->
          let* exprs, lifted = acc in
          let* ll_cur_expr, lifted = ll_expr lifted env cur_expr in
          R.return (ll_cur_expr :: exprs, lifted))
    in
    R.return (LLList ll_exprs, lifted)
  | ETuple (e1, e2, tl) ->
    let* ll_e1, lifted = ll_expr lifted env e1 in
    let* ll_e2, lifted = ll_expr lifted env e2 in
    let* ll_tl, lifted =
      List.fold
        tl
        ~init:(R.return ([], lifted))
        ~f:(fun acc cur_expr ->
          let* exprs, lifted = acc in
          let* ll_cur_expr, lifted = ll_expr lifted env cur_expr in
          R.return (ll_cur_expr :: exprs, lifted))
    in
    R.return (LLTuple (ll_e1, ll_e2, ll_tl), lifted)
  | EListConcat (lexpr, rexpr) ->
    let* ll_lexpr, lifted = ll_expr lifted env lexpr in
    let* ll_rexpr, lifted = ll_expr lifted env rexpr in
    R.return (LLListConcat (ll_lexpr, ll_rexpr), lifted)
  | EConstraint (expr, dt) ->
    let* ll_expr, lifted = ll_expr lifted env expr in
    R.return (LLConstraint (ll_expr, dt), lifted)
  | If (cond, th, Some el) ->
    let* ll_cond, lifted = ll_expr lifted env cond in
    let* ll_th, lifted = ll_expr lifted env th in
    let* ll_else, lifted = ll_expr lifted env el in
    return (LLIf (ll_cond, ll_th, Some ll_else), lifted)
  | If (cond, th, None) ->
    let* ll_cond, lifted = ll_expr lifted env cond in
    let* ll_th, lifted = ll_expr lifted env th in
    return (LLIf (ll_cond, ll_th, None), lifted)
  | Match (e, cases) ->
    let* ll_e, lifted = ll_expr lifted env e in
    let* ll_cases, lifted =
      List.fold
        cases
        ~init:(R.return ([], lifted))
        ~f:(fun acc case ->
          let* cases, lifted = acc in
          let pat, expr = case in
          let* ll_expr, lifted = ll_expr lifted env expr in
          R.return (((pat, ll_expr) : ll_case) :: cases, lifted))
    in
    R.return (LLMatch (ll_e, ll_cases), lifted)
  | Application (lexpr, rexpr) ->
    let* ll_lexpr, lifted = ll_expr lifted env lexpr in
    let* ll_rexpr, lifted = ll_expr lifted env rexpr in
    R.return (LLApplication (ll_lexpr, ll_rexpr), lifted)
  | _ -> failwith "Incorrect expression was encountered during LL"
;;

(* let rec pattern_names (set : NameSet.t) =
   let env_fold (set : NameSet.t) pts =
   List.fold pts ~init:set ~f:(fun set p -> NameSet.union set (pattern_names set p))
   in
   function
   | Const _ | Wildcard | Operation _ -> set
   | Var id -> NameSet.extend id set
   | Tuple (a, b, tl) ->
   let pts = a :: b :: tl in
   env_fold set pts
   | List pts -> env_fold set pts
   | ListConcat (l_pt, r_pt) ->
   let l_env = pattern_names set l_pt in
   pattern_names l_env r_pt
   | Constraint (p, _) -> pattern_names set p
   ;; *)
