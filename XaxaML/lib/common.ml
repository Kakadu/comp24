(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

module StrSet = struct
  open Base

  type t = (string, String.comparator_witness) Set.t

  let empty = Set.empty (module String)
  let singleton str = Set.singleton (module String) str
  let union = Set.union
  let union_list lst = Set.union_list (module String) lst

  let find s str =
    match Set.binary_search s ~compare:String.compare `First_equal_to str with
    | Some _ -> true
    | None -> false
  ;;

  let add = Set.add
  let to_list = Set.to_list
  let of_list = Set.of_list (module String)
  let fold = Set.fold
  let diff = Set.diff
end

let rec get_idents = function
  | Ast.P_typed (pat, _) -> get_idents pat
  | P_any | P_const _ -> StrSet.empty
  | P_val ident -> StrSet.singleton ident
  | P_cons_list (p1, p2) -> StrSet.union (get_idents p1) (get_idents p2)
  | P_tuple (hd, tl) -> StrSet.union (get_idents hd) (get_idents_from_list tl)

and get_idents_from_list pat_list =
  Base.List.fold pat_list ~init:StrSet.empty ~f:(fun acc p ->
    StrSet.union acc (get_idents p))
;;

module NamesHolder = struct
  open Ast
  open Base

  let rec expr = function
    | E_typed (e, _) -> expr e
    | E_const _ -> StrSet.empty
    | E_ident ident -> StrSet.singleton ident
    | E_cons_list (e1, e2) -> StrSet.union (expr e1) (expr e2)
    | E_app (e1, e2) -> StrSet.union (expr e1) (expr e2)
    | E_fun (first, other, e) ->
      StrSet.union (get_idents_from_list (first :: other)) (expr e)
    | E_ite (e1, e2, e3) -> StrSet.union_list [ expr e1; expr e2; expr e3 ]
    | E_let (d, e) -> StrSet.union (decl d) (expr e)
    | E_match (e, p_list) ->
      StrSet.union_list
        (expr e :: List.map p_list ~f:(fun (p, e) -> StrSet.union (get_idents p) (expr e)))
    | E_tuple (e, e_list) -> StrSet.union_list @@ List.map (e :: e_list) ~f:expr

  and decl = function
    | Non_rec db -> decl_body db
    | Rec db_list ->
      List.fold db_list ~init:StrSet.empty ~f:(fun acc db ->
        StrSet.union acc (decl_body db))

  and decl_body (pat, _, e) = StrSet.union (get_idents pat) (expr e)

  let toplevel = function
    | Expr e -> expr e
    | Let_decl d -> decl d
  ;;

  type t = (int, Int.comparator_witness) Set.t

  let empty = Set.empty (module Int)

  let create p =
    let process_ident ident =
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
      let char_list = String.to_list ident in
      match List.length char_list with
      | x when x >= 2 && x <= 10 && Char.equal (List.hd_exn char_list) 'a' ->
        helper 0 (List.tl_exn char_list)
      | _ -> None
    in
    let idents =
      List.fold_left p ~init:StrSet.empty ~f:(fun acc t -> StrSet.union acc (toplevel t))
    in
    Set.filter_map (module Int) idents ~f:process_ident
  ;;

  let contains ng id =
    match Set.find ng ~f:(Int.equal id) with
    | Some _ -> true
    | None -> false
  ;;

  let pp_names_holder ppf ng =
    Stdlib.Format.fprintf ppf "[";
    Set.iter ng ~f:(fun key -> Stdlib.Format.fprintf ppf " %i" key);
    Stdlib.Format.fprintf ppf " ]"
  ;;
end

module MonadCounter = struct
  open Base

  type 'a t = NamesHolder.t * int -> NamesHolder.t * int * 'a

  let return x (nh, var) = nh, var, x

  let fresh (nh, var) =
    let rec helper num = if NamesHolder.contains nh num then helper (num + 1) else num in
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

  module RList = struct
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
end

module MonadCounterError = struct
  open Base

  type ('a, 'e) t = NamesHolder.t * int -> NamesHolder.t * int * ('a, 'e) Result.t

  let return x (nh, var) = nh, var, Result.return x
  let fail e (nh, var) = nh, var, Result.fail e

  let fresh (nh, var) =
    let rec helper num = if NamesHolder.contains nh num then helper (num + 1) else num in
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

  module RMap = struct
    let fold mp ~init ~f =
      Map.fold mp ~init ~f:(fun ~key:k ~data:v acc ->
        let* acc = acc in
        f acc k v)
    ;;
  end

  module RList = struct
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
  end

  let run m nh init = m (nh, init)
end

let parse_expr_and_print_banned str =
  match Parser.run_parser_expr str with
  | Result.Ok expr ->
    Stdlib.Format.printf
      "%a\n"
      NamesHolder.pp_names_holder
      (NamesHolder.create [ Expr expr ])
  | Result.Error str -> Stdlib.Format.printf "Parsing error%s\n" str
;;

let%expect_test "allowed" =
  parse_expr_and_print_banned {|(aa, aaa, b, a0a, aa0, aa1, a_, a, _a, __, a1000000000)|};
  [%expect {|
    [ ] |}]
;;

let%expect_test "banned" =
  parse_expr_and_print_banned {|(a0, a1, a10, a99, a101, a999999999)|};
  [%expect {|
    [ 0 1 10 99 101 999999999 ] |}]
;;
