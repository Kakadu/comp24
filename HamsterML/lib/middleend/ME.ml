open Base
open Ast
open LL

type me_expr =
  | MEConst of value
  | MEVar of id
  | MEOperation of op
  | METuple of me_expr * me_expr * me_expr list
  | MEList of me_expr list
  | MEListConcat of me_expr * me_expr
  | MEConstraint of me_expr * dataType
  | MEApplication of me_expr * me_expr
  | MELet of funType * me_bind list * me_expr option
  | MEIf of me_expr * me_expr * me_expr option
[@@deriving show]

and me_bind = pattern * args * me_expr
and me_prog = me_expr list

module ListConverter : sig
  val tail : me_expr -> me_expr
  val len : me_expr -> me_expr
  val head : me_expr -> me_expr
  val get : me_expr -> int -> me_expr
end = struct
  let ensure_list (lst : me_expr) =
    match lst with
    | MEList _ -> lst
    | _ -> failwith "Attempt to perform a list operation on a non-list"
  ;;

  let len lst = MEApplication (MEVar "list_length", ensure_list lst)

  let get lst i =
    MEApplication (MEApplication (MEVar "list_get", ensure_list lst), MEConst (Int i))
  ;;

  let head lst = MEApplication (MEVar "list_head", ensure_list lst)
  let tail lst = MEApplication (MEVar "list_tail", ensure_list lst)
end

module TupleConverter : sig
  val len : me_expr -> me_expr
  val get : me_expr -> int -> me_expr
end = struct
  let ensure_tuple (tpl : me_expr) =
    match tpl with
    | METuple _ -> tpl
    | _ -> failwith "Attempt to perform a tuple operation on a non-tuple"
  ;;

  let len lst = MEApplication (MEVar "tuple_length", ensure_tuple lst)

  let get lst i =
    MEApplication (MEApplication (MEVar "tuple_get", ensure_tuple lst), MEConst (Int i))
  ;;
end

(* AND for conditions *)
let ( <|> ) a b =
  match a, b with
  | MEConst (Bool true), _ -> b
  | _, MEConst (Bool true) -> a
  | _ -> MEApplication (MEApplication (MEOperation (Binary AND), a), b)
;;

let bin_op op a b = MEApplication (MEApplication (MEOperation (Binary op), a), b)
let _if i t e = MEIf (i, t, e)
let _true = MEConst (Bool true)
let _false = MEConst (Bool false)
let _int num = MEConst (Int num)
let _let name body scope = MELet (Nonrecursive, [ name, [], body ], Some scope)

let rec match_compare (matched : me_expr) (pattern : pattern) =
  match pattern with
  | Const v -> bin_op EQ (MEConst v) matched
  | Var id -> bin_op EQ (MEVar id) matched
  | Wildcard | Operation _ -> _true
  | Constraint (p, _) -> match_compare matched p
  | Tuple (p1, p2, pts) ->
    List.foldi (p1 :: p2 :: pts) ~init:_true ~f:(fun i cond pattern_i ->
      cond <|> match_compare (TupleConverter.get matched i) pattern_i)
  | ListConcat (hd, tl) ->
    let result =
      match_compare (ListConverter.head matched) hd
      <|> match_compare (ListConverter.tail matched) tl
    in
    let matched_is_correct = bin_op GTE (ListConverter.len matched) (_int 2) in
    _if matched_is_correct result (Some _false)
  | List pts ->
    let pattern_list_len = _int (List.length pts) in
    let matched_list_len = ListConverter.len matched in
    let correctness = bin_op EQ pattern_list_len matched_list_len in
    let result =
      List.foldi pts ~init:_true ~f:(fun i cond pattern_i ->
        cond <|> match_compare (TupleConverter.get matched i) pattern_i)
    in
    _if correctness result (Some _false)
;;

(* match [matched] with [pattern] -> [action] *)
let rec convert_match_case (matched : me_expr) (pattern : pattern) (action : me_expr) =
  match pattern with
  | Var _ as var -> _let var matched action
  | Wildcard | Operation _ | Const _ -> action
  | Constraint (p, _) -> convert_match_case matched p action
  | Tuple (p1, p2, pts) ->
    List.foldi (p1 :: p2 :: pts) ~init:action ~f:(fun i action pattern_i ->
      convert_match_case (TupleConverter.get matched i) pattern_i action)
  | ListConcat (hd, tl) ->
    convert_match_case (ListConverter.tail matched) tl action
    |> convert_match_case (ListConverter.head matched) hd
  | List pts ->
    List.foldi pts ~init:action ~f:(fun i action pattern_i ->
      convert_match_case (TupleConverter.get matched i) pattern_i action)
;;

let rec convert_match (matched : ll_expr) (cases : ll_case list) =
  match cases with
  | [] -> failwith "No cases provided"
  | (pattern, action) :: tail_cases ->
    let me_action = convert_expr action in
    let me_matched = convert_expr matched in
    let branch = convert_match_case me_matched pattern me_action in
    (match tail_cases with
     | [] -> branch
     | _ ->
       _if
         (match_compare me_matched pattern)
         branch
         (Some (convert_match matched tail_cases)))

and convert_expr (expr : ll_expr) : me_expr =
  match expr with
  | LLConst v -> MEConst v
  | LLVar id -> MEVar id
  | LLOperation op -> MEOperation op
  | LLConstraint (l, dt) -> MEConstraint (convert_expr l, dt)
  | LLListConcat (l, r) -> MEListConcat (convert_expr l, convert_expr r)
  | LLApplication (l, r) -> MEApplication (convert_expr l, convert_expr r)
  | LLList lst -> MEList (List.map lst ~f:convert_expr)
  | LLTuple (a, b, tl) ->
    let a = convert_expr a in
    let b = convert_expr b in
    let tl = List.map tl ~f:convert_expr in
    METuple (a, b, tl)
  | LLIf (i, t, Some e) -> _if (convert_expr i) (convert_expr t) (Some (convert_expr e))
  | LLIf (i, t, None) -> _if (convert_expr i) (convert_expr t) None
  | LLLet (rec_flag, binds, Some scope) ->
    let binds =
      Base.List.fold binds ~init:[] ~f:(fun acc (name, args, body) ->
        (name, args, convert_expr body) :: acc)
    in
    MELet (rec_flag, List.rev binds, Some (convert_expr scope))
  | LLLet (rec_flag, binds, None) ->
    let binds =
      Base.List.fold binds ~init:[] ~f:(fun acc (name, args, body) ->
        (name, args, convert_expr body) :: acc)
    in
    MELet (rec_flag, List.rev binds, None)
  | LLMatch (matched, cases) -> convert_match matched cases
;;

let convert_prog (prog : ll_prog) : me_prog = List.map prog ~f:convert_expr
