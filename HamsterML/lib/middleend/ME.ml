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

module ListConverter = struct
  let get lst i = MEApplication (MEApplication (MEVar "list_get", lst), MEConst (Int i))
  let len lst = MEApplication (MEVar "list_length", lst)
  let head lst = MEApplication (MEVar "list_head", lst)
  let tail lst = MEApplication (MEVar "list_tail", lst)
end

module TupleConverter = struct
  let get lst i = MEApplication (MEApplication (MEVar "tuple_get", lst), MEConst (Int i))
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

let rec match_compare_pattern (matched : me_expr) (pattern : pattern) =
  match pattern with
  | Const v -> bin_op EQ (MEConst v) matched
  | Var id -> bin_op EQ (MEVar id) matched
  | Wildcard | Operation _ -> _true
  | Constraint (p, _) -> match_compare_pattern matched p
  | Tuple (p1, p2, pts) ->
    List.foldi (p1 :: p2 :: pts) ~init:_true ~f:(fun i cond pattern_i ->
      cond <|> match_compare_pattern (TupleConverter.get matched i) pattern_i)
  | ListConcat (hd, tl) ->
    let matched_hd = ListConverter.head matched in
    let matched_tl = ListConverter.tail matched in
    let matched_len = ListConverter.len matched in
    _let hd matched_hd
    @@ _let tl matched_tl
    @@ (bin_op GTE matched_len (_int 2)
        <|> match_compare_pattern matched_hd hd
        <|> match_compare_pattern matched_tl tl)
  | List pts ->
    let pattern_list_len = _int (List.length pts) in
    let matched_list_len = ListConverter.len matched in
    let correctness = bin_op EQ pattern_list_len matched_list_len in
    let result =
      List.foldi pts ~init:_true ~f:(fun i cond pattern_i ->
        cond <|> match_compare_pattern (ListConverter.get matched i) pattern_i)
    in
    _if correctness result (Some _false)
;;

(* match [matched] with [pattern] -> [action] *)
let rec convert_match_case (matched : me_expr) (pattern : pattern) (action : me_expr) =
  (* match arg_0 with arg_1 :: arg_2 -> arg_1 *)
  match pattern with
  | Wildcard | Operation _ | Const _ | Var _ -> action
  | Constraint (p, _) -> convert_match_case matched p action
  | Tuple (p1, p2, pts) ->
    List.foldi (p1 :: p2 :: pts) ~init:action ~f:(fun i action pattern_i ->
      convert_match_case (TupleConverter.get matched i) pattern_i action)
  | ListConcat (hd, tl) ->
    convert_match_case (ListConverter.tail matched) tl action
    |> convert_match_case (ListConverter.head matched) hd
  | List pts ->
    List.foldi pts ~init:action ~f:(fun i action pattern_i ->
      convert_match_case (ListConverter.get matched i) pattern_i action)
;;

let rec get_match_context (matched : me_expr) (pattern : pattern) : me_expr -> me_expr =
  (* match x with *)
  match pattern with
  | Var _ as var -> _let var matched
  | Wildcard | Operation _ | Const _ -> fun x -> x
  | Constraint (p, _) -> convert_match_case matched p
  | Tuple (p1, p2, pts) ->
    let patterns = p1 :: p2 :: pts in
    fun id ->
      List.foldi patterns ~init:id ~f:(fun i acc pattern_i ->
        get_match_context (TupleConverter.get matched i) pattern_i acc)
  | List pts ->
    fun act ->
      List.foldi pts ~init:act ~f:(fun i acc pattern_i ->
        get_match_context (ListConverter.get matched i) pattern_i acc)
  | ListConcat (hd, tl) ->
    let matched_hd = ListConverter.head matched in
    let matched_tl = ListConverter.tail matched in
    fun id -> _let hd matched_hd id |> _let tl matched_tl
;;

let rec convert_match (matched : ll_expr) (cases : ll_case list) =
  match cases with
  | [] -> failwith "No cases provided"
  | (pattern, action) :: tail_cases ->
    let me_action = convert_expr action in
    let me_matched = convert_expr matched in
    let branch = convert_match_case me_matched pattern me_action in
    get_match_context me_matched pattern
    @@
      (match tail_cases with
      | [] -> branch
      | _ ->
        _if
          (match_compare_pattern me_matched pattern)
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
  | LLMatch (matched, cases) -> List.rev cases |> convert_match matched
;;

let convert_prog (prog : ll_prog) : me_prog = List.map prog ~f:convert_expr
