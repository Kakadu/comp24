open Ast
open Base

type ll_expr =
  | LL_Const of value
  | LL_Var of id
  | LL_Operation of op
  | LL_Tuple of ll_expr list
  | LL_List of ll_expr list
  | LL_ListConcat of ll_expr * ll_expr
  | LL_Constraint of ll_expr * dataType
  | LL_Application of ll_expr * ll_expr
  | LL_Let of funType * bind list * ll_expr option
  | LL_If of ll_expr * ll_expr * ll_expr option
  | LL_Match of ll_expr * case list

module R = struct
  type 'a t = int -> 'a * int

  let fresh last = last, last + 1

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

type var_name = string

module LL_Env = struct
  type t = (var_name, String.comparator_witness) Set.t

  let empty : t = Set.empty (module String)
  let extend (name : var_name) (env : t) : t = Set.add env name
  let union : t -> t -> t = Set.union

  let find_exn (name : var_name) (env : t) =
    Set.find_exn env ~f:(fun x -> String.equal x name)
  ;;

  let find (name : var_name) (env : t) = Set.find env ~f:(fun x -> String.equal x name)

  let rec gen_name (env : t) =
    let open R in
    let* fresh = R.fresh in
    let varname = "LL_" ^ Int.to_string fresh in
    match find varname env with
    | None -> R.return (extend varname env, varname)
    | Some _ -> gen_name env
  ;;
end

let rec lift_pattern (env : LL_Env.t) = function
  | Const _ | Wildcard -> env
  | Var id -> LL_Env.extend id env
  | Tuple pts ->
    List.fold pts ~init:env ~f:(fun env p -> LL_Env.union env (lift_pattern env p))
  | List   
  | _ -> failwith "rofl"
;;
(*
| ListConcat
| Constraint 
| Operation *)
