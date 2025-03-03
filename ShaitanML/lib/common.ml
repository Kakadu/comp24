open Ast
open Pat_elim_ast
open Base

module StrMap = struct
  type 'a t = (string, 'a, String.comparator_witness) Map.t

  let empty = Map.empty (module String)
  let singleton str = Map.singleton (module String) str
  let find m str = Map.find m str
  let add = Map.add
  let update = Map.update
  let merge_two fst snd = Map.merge_skewed fst snd ~combine:(fun ~key:_ _ v2 -> v2)
end

module StrSet = struct
  type t = (string, String.comparator_witness) Set.t

  let empty = Set.empty (module String)
  let singleton str = Set.singleton (module String) str
  let union = Set.union
  let union_list lst = Set.union_list (module String) lst
  let find s str = Set.mem s str
  let add = Set.add
  let to_list = Set.to_list
  let of_list = Set.of_list (module String)
  let fold = Set.fold
  let diff = Set.diff
end

type bindings = (int, Int.comparator_witness) Set.t

let rec get_binds_pat =
  let open Ast in
  function
  | PConstraint (pat, _) -> get_binds_pat pat
  | PAny | PConst _ -> StrSet.empty
  | PVar ident -> StrSet.singleton ident
  | PCons (p1, p2) -> StrSet.union (get_binds_pat p1) (get_binds_pat p2)
  | PTuple pl ->
    Base.List.fold pl ~init:StrSet.empty ~f:(fun acc p ->
      StrSet.union acc (get_binds_pat p))
;;

let rec get_binds_expr = function
  | EConstraint (e, _) -> get_binds_expr e
  | EConst _ -> StrSet.empty
  | EVar ident -> StrSet.singleton ident
  | ECons (e1, e2) -> StrSet.union (get_binds_expr e1) (get_binds_expr e2)
  | EApply (e1, e2) -> StrSet.union (get_binds_expr e1) (get_binds_expr e2)
  | EFun (pat, e) -> StrSet.union (get_binds_pat pat) (get_binds_expr e)
  | EIf (e1, e2, e3) ->
    StrSet.union_list [ get_binds_expr e1; get_binds_expr e2; get_binds_expr e3 ]
  | ELet (_, (p, e1), e2) ->
    StrSet.union (get_binds_pat p) (StrSet.union (get_binds_expr e1) (get_binds_expr e2))
  | EMatch (e, p_list) ->
    StrSet.union_list
      (get_binds_expr e
       :: List.map p_list ~f:(fun (p, e) ->
         StrSet.union (get_binds_pat p) (get_binds_expr e)))
  | ETuple e_list -> StrSet.union_list @@ List.map e_list ~f:get_binds_expr

and get_binds_case (pat, e) = StrSet.union (get_binds_pat pat) (get_binds_expr e)

and get_binds_str_item = function
  | SValue (_, bl) ->
    List.fold bl ~init:StrSet.empty ~f:(fun acc b -> StrSet.union acc (get_binds_case b))
;;

let make_binds structure =
  let make_id id =
    let is_digit = function
      | '0' .. '9' -> true
      | _ -> false
    in
    let char_to_digit c = Char.to_int c - Char.to_int '0' in
    let rec helper acc = function
      | [] -> Some acc
      | hd :: tl ->
        if is_digit hd then helper ((acc * 10) + char_to_digit hd) tl else None
    in
    let char_list = String.to_list id in
    match List.length char_list with
    | x when x >= 2 && x <= 10 && Char.equal (List.hd_exn char_list) 'a' ->
      helper 0 (List.tl_exn char_list)
    | _ -> None
  in
  let idents =
    List.fold_left structure ~init:StrSet.empty ~f:(fun acc t ->
      StrSet.union acc (get_binds_str_item t))
  in
  Set.filter_map (module Int) idents ~f:make_id
;;

let contains ng id =
  match Set.find ng ~f:(Int.equal id) with
  | Some _ -> true
  | None -> false
;;

let pp_bindings ppf ng =
  Stdlib.Format.fprintf ppf "[";
  Set.iter ng ~f:(fun key -> Stdlib.Format.fprintf ppf " %i" key);
  Stdlib.Format.fprintf ppf " ]"
;;

module MonadCounter = struct
  open Base

  type 'a t = bindings * int -> bindings * int * 'a

  let return x (nh, var) = nh, var, x

  let fresh (nh, var) =
    let rec helper num = if contains nh num then helper (num + 1) else num in
    let next = helper var in
    nh, next + 1, next
  ;;

  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    fun tup ->
    let nh, var, x = m tup in
    f x (nh, var)
  ;;

  let ( >>= ) = bind
  let ( let* ) = bind

  let ( >>| ) (m : 'a t) (f : 'a -> 'b) : 'b t =
    fun tup ->
    let nh, var, x = m tup in
    nh, var, f x
  ;;

  let run (m : 'a t) nh start = m (nh, start)

  let map (xs : 'a list) ~(f : 'a -> 'b t) : 'b list t =
    let* xs =
      List.fold xs ~init:(return []) ~f:(fun acc x ->
        let* acc = acc in
        let* x = f x in
        return (x :: acc))
    in
    return @@ List.rev xs
  ;;

  let fold_left (xs : 'a list) ~(init : 'b t) ~(f : 'b -> 'a -> 'b t) : 'b t =
    List.fold xs ~init ~f:(fun acc x ->
      let* acc = acc in
      f acc x)
  ;;
end

module MonadCounterError = struct
  open Base

  type ('a, 'e) t = bindings * int -> bindings * int * ('a, 'e) Result.t

  let return x (nh, var) = nh, var, Result.return x
  let fail e (nh, var) = nh, var, Result.fail e

  let fresh (nh, var) =
    let rec helper num = if contains nh num then helper (num + 1) else num in
    let next = helper var in
    nh, next + 1, Result.return next
  ;;

  let ( >>= ) (m : ('a, 'e) t) (f : 'a -> ('b, 'e) t) : ('b, 'e) t =
    fun tup ->
    match m tup with
    | nh, var, Result.Error err -> nh, var, Result.fail err
    | nh, var, Result.Ok x -> f x (nh, var)
  ;;

  let bind = ( >>= )

  let ( >>| ) (m : ('a, 'e) t) (f : 'a -> 'b) : ('b, 'e) t =
    fun tup ->
    match m tup with
    | nh, var, Result.Error err -> nh, var, Result.fail err
    | nh, var, Result.Ok x -> nh, var, Result.return (f x)
  ;;

  let ( let* ) = bind

  let fold mp ~init ~f =
    Map.fold mp ~init ~f:(fun ~key:k ~data:v acc ->
      let* acc = acc in
      f acc k v)
  ;;

  let fold_left xs ~init ~f =
    List.fold_left xs ~init ~f:(fun acc x ->
      let* acc = acc in
      f acc x)
  ;;

  let fold_right xs ~init ~f =
    List.fold_right xs ~init ~f:(fun x acc ->
      let* acc = acc in
      f x acc)
  ;;

  let map (xs : 'a list) ~(f : 'a -> ('b, 'e) t) : ('b list, 'e) t =
    let* xs =
      List.fold xs ~init:(return []) ~f:(fun acc x ->
        let* acc = acc in
        let* x = f x in
        return (x :: acc))
    in
    return @@ List.rev xs
  ;;

  let run m nh init = m (nh, init)
end

let builtins =
  [ "( + )"
  ; "( - )"
  ; "( / )"
  ; "( * )"
  ; "( < )"
  ; "( > )"
  ; "( <= )"
  ; "( >= )"
  ; "( <> )"
  ; "( = )"
  ; "( != )"
  ; "( && )"
  ; "( || )"
  ; "print_int"
  ; "list_head"
  ; "list_tail"
  ; "list_len"
  ; "tuple_element"
  ; "fail_match"
  ]
;;

let empty = Base.Map.empty (module Base.String)
let get_id i = "a" ^ Int.to_string i

let make_apply op expr1 expr2 = PEEApp (PEEApp (PEEVar op, expr1), expr2)