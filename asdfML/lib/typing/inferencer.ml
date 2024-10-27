(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

(* Based on https://gitlab.com/Kakadu/fp2020course-materials/-/blob/master/code/miniml*)

open Types
open Pp_typing
open Base
open Utils

module R : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold_left
      :  ('k, 'v, 'a) Map.t
      -> init:'acc t
      -> f:('k -> 'v -> 'acc -> 'acc t)
      -> 'acc t
  end

  (** Creation of a fresh name from internal state *)
  val fresh : int t

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Result.t
end = struct
  (* A compositon: State monad after Result monad *)
  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Error x
    | Ok a -> f a last
  ;;

  let fail e st = st, Result.fail e
  let return x last = last, Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun x f st ->
    match x st with
    | st, Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.Error e
  ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  module RMap = struct
    let fold_left xs ~init ~f =
      let f ~key ~data acc =
        let open Syntax in
        let* acc = acc in
        f key data acc
      in
      Map.fold xs ~init ~f
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

module VarSet = struct
  include Set

  let empty = Set.empty (module Int)

  let fold_left_m f init set =
    fold set ~init ~f:(fun acc x ->
      let open R.Syntax in
      let* acc = acc in
      f acc x)
  ;;
end

module Type = struct
  type t = ty

  let rec occurs_in v = function
    | TVar b -> b = v
    | TArrow (l, r) -> occurs_in v l || occurs_in v r
    | TGround _ -> false
    | TTuple xs -> List.exists xs ~f:(occurs_in v)
    | TList t -> occurs_in v t
  ;;

  let free_vars =
    let rec helper acc = function
      | TVar b -> Set.add acc b
      | TArrow (l, r) -> helper (helper acc l) r
      | TGround _ -> acc
      | TTuple xs -> List.fold xs ~init:VarSet.empty ~f:helper
      | TList t -> helper acc t
    in
    helper VarSet.empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : fresh -> ty -> t R.t
  val find : t -> fresh -> ty option
  val apply : t -> ty -> ty
  val unify : ty -> ty -> t R.t

  (** Compositon of substitutions *)
  val compose : t -> t -> t R.t

  val compose_all : t list -> t R.t
  val remove : t -> fresh -> t
  val pp : Format.formatter -> t -> unit
end = struct
  open R
  open R.Syntax

  (* an association list. In real world replace it by a finite map *)
  type t = (fresh, ty, Int.comparator_witness) Map.t

  let pp fmt subst =
    let open Format in
    fprintf
      fmt
      "[ %a ]"
      (pp_print_list
         ~pp_sep:(fun fmt () -> fprintf fmt ", ")
         (* (fun fmt (k, v) -> fprintf fmt "%d -> %a" k pp_typ v)) *)
           (fun fmt (k, v) -> fprintf fmt "'%s -> %a" (type_id_to_name k) pp_typ v))
      (Map.to_alist subst)
  ;;

  let empty = Map.empty (module Int)
  let mapping k v = if Type.occurs_in k v then fail `Occurs_check else return (k, v)

  let singleton k v =
    let* k, v = mapping k v in
    Map.set empty ~key:k ~data:v |> return
  ;;

  let find xs k = Map.find xs k
  let remove xs k = Map.remove xs k

  let apply s =
    let rec helper = function
      | TVar b as ty ->
        (match find s b with
         | None -> ty
         | Some x -> x)
      | TArrow (l, r) -> TArrow (helper l, helper r)
      | other -> other
    in
    helper
  ;;

  let rec unify l r =
    (* dbg "Unifing %a and %a\n" pp_typ l pp_typ r; *)
    match l, r with
    | TGround l, TGround r when equal_ground l r -> return empty
    | TGround _, TGround _ -> fail (`Unification_failed (l, r))
    | TVar a, TVar b when Int.equal a b -> return empty
    | TVar b, t | t, TVar b -> singleton b t
    | TArrow (l1, r1), TArrow (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | TTuple t1, TTuple t2 ->
      (match
         List.fold2 t1 t2 ~init:(return empty) ~f:(fun acc l r ->
           let* acc = acc in
           let* sub = unify l r in
           compose acc sub)
       with
       | List.Or_unequal_lengths.Ok x -> x
       | List.Or_unequal_lengths.Unequal_lengths -> fail (`Unification_failed (l, r)))
    | TList t1, TList t2 -> unify t1 t2
    | _ -> fail (`Unification_failed (l, r))

  and extend k v s =
    match find s k with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      RMap.fold_left s ~init:(return s2) ~f:(fun k v acc ->
        let v = apply s2 v in
        let* k, v = mapping k v in
        Map.update acc k ~f:(fun _ -> v) |> return)
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2

  and compose s1 s2 = RMap.fold_left s2 ~init:(return s1) ~f:extend

  let compose_all ss =
    List.fold_left ss ~init:(return empty) ~f:(fun acc x ->
      let* acc = acc in
      compose acc x)
  ;;
end

module Scheme = struct
  type t = scheme

  let occurs_in v = function
    | xs, t -> (not (VarSet.mem xs v)) && Type.occurs_in v t
  ;;

  let free_vars = function
    | bs, t -> VarSet.diff (Type.free_vars t) bs
  ;;

  let apply sub (names, ty) =
    let sub' = VarSet.fold names ~init:sub ~f:Subst.remove in
    names, Subst.apply sub' ty
  ;;
end

module TypeEnv = struct
  include Map

  type t = (Ast.id, scheme, String.comparator_witness) Map.t

  let empty = Map.empty (module String)

  let free_vars : t -> (var_id, Int.comparator_witness) VarSet.t =
    fold ~init:VarSet.empty ~f:(fun ~key:_ ~data acc ->
      VarSet.union acc (Scheme.free_vars data))
  ;;

  let extend env id scheme = set env ~key:id ~data:scheme

  let rec extend_pat (env : t) (pat : Ast.pattern) (scheme : Scheme.t) : t R.t =
    let open R in
    let open R.Syntax in
    match pat, scheme with
    | PWild, _ -> return env
    | PIdent x, _ -> extend env x scheme |> return
    | PTuple xs, (vars, (TTuple ys as ty)) ->
      List.fold2 xs ys ~init:(return env) ~f:(fun acc x y ->
        let* acc = acc in
        extend_pat acc x (vars, y))
      |> (function
       | List.Or_unequal_lengths.Ok env' -> env'
       | _ -> fail (`Arg_num_mismatch (pat, ty)))
    | PAnn (x, _), _ -> extend_pat env x scheme
    | _ ->
      fail
        (`Syntax_error
          "only identifiers, tuples, wildcards and type annotations are supported in let \
           bindings")
  ;;

  let apply env sub = map env ~f:(Scheme.apply sub)

  let bin_op_list =
    (* TODO: var *)
    let var = TVar (-1) in
    [ "( - )", int_typ ^-> int_typ ^-> int_typ
    ; "( + )", int_typ ^-> int_typ ^-> int_typ
    ; "( / )", int_typ ^-> int_typ ^-> int_typ
    ; "( * )", int_typ ^-> int_typ ^-> int_typ
    ; "( > )", var ^-> var ^-> bool_typ
    ; "( < )", var ^-> var ^-> bool_typ
    ; "( >= )", var ^-> var ^-> bool_typ
    ; "( <= )", var ^-> var ^-> bool_typ
    ; "( == )", var ^-> var ^-> bool_typ
    ; "( != )", var ^-> var ^-> bool_typ
    ; "( && )", bool_typ ^-> bool_typ ^-> bool_typ
    ; "( || )", bool_typ ^-> bool_typ ^-> bool_typ
    ; "[ - ]", int_typ ^-> int_typ
    ; "not", bool_typ ^-> bool_typ
    ]
  ;;

  let default =
    List.fold bin_op_list ~init:empty ~f:(fun env (op, ty) ->
      let fv = VarSet.diff (Type.free_vars ty) (free_vars env) in
      extend env op (fv, ty))
  ;;

  let _pp ?(no_default = true) fmt (xs : t) =
    Format.fprintf fmt "{|\n";
    (if no_default then default |> Map.keys |> List.fold ~init:xs ~f:Map.remove else xs)
    |> Map.iteri ~f:(fun ~key:n ~data:s -> Format.fprintf fmt "%s -> %a\n" n pp_scheme s);
    Format.fprintf fmt "|}%!"
  ;;

  let pp = _pp ~no_default:true
end

open R
open R.Syntax

let unify = Subst.unify
let fresh_var = fresh >>| fun n -> TVar n

let instantiate : scheme -> ty R.t =
  fun (set, t) ->
  VarSet.fold_left_m
    (fun typ name ->
      let* f1 = fresh_var in
      let* s = Subst.singleton name f1 in
      return (Subst.apply s typ))
    (return t)
    set
;;

let generalize : TypeEnv.t -> Type.t -> scheme =
  fun env ty ->
  let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  free, ty
;;

let lookup_env e xs =
  match TypeEnv.find xs e with
  | None -> fail (`No_variable e)
  | Some scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans)
;;

let pp_env sub fmt env =
  let env : TypeEnv.t = Map.map env ~f:(Scheme.apply sub) in
  TypeEnv.pp fmt env
;;

open Types

let infer =
  let infer_const = function
    | Ast.CBool _ -> bool_typ
    | Ast.CInt _ -> int_typ
    | Ast.CUnit -> unit_typ
  in
  let rec infer_pattern : TypeEnv.t -> Ast.pattern -> (TypeEnv.t * ty) R.t =
    fun env -> function
    | PConst c -> return (env, infer_const c)
    | PWild -> fresh_var >>| fun v -> env, v
    | PIdent x ->
      let* fv = fresh_var in
      let env' = TypeEnv.extend env x (VarSet.empty, fv) in
      return (env', fv)
    | PTuple xs ->
      List.fold_right
        xs
        ~init:(return (env, []))
        ~f:(fun x acc ->
          let* env, fvs = acc in
          let* env', fv = infer_pattern env x in
          return (env', fv :: fvs))
      >>| fun (env, fvs) -> env, TTuple fvs
    | PList _ -> fail (`TODO "infer_pattern PList")
    | PCons (_, _) -> fail (`TODO "infer_pattern PCons")
    | PAnn (pat, ann_ty) ->
      let* pat_env, ty = infer_pattern env pat in
      let* sub = unify ty (an_ty_to_ty ann_ty) in
      return (TypeEnv.apply pat_env sub, Subst.apply sub ty)
  in
  let rec (infer_expr : TypeEnv.t -> Ast.expr -> (Subst.t * ty) R.t) =
    fun env -> function
    | EConst c -> return (Subst.empty, infer_const c)
    | EVar x -> lookup_env x env
    | EApp (left, right) ->
      let* subst_left, typ_left = infer_expr env left in
      let* subst_right, typ_right = infer_expr (TypeEnv.apply env subst_left) right in
      let* tvar = fresh_var in
      let* subst = unify (Subst.apply subst_right typ_left) (TArrow (typ_right, tvar)) in
      let final_typ = Subst.apply subst tvar in
      let* final_subst = Subst.compose_all [ subst; subst_right; subst_left ] in
      return (final_subst, final_typ)
    | EIfElse (cond, th, el) ->
      let* s1, t1 = infer_expr env cond in
      let* s2, t2 = infer_expr env th in
      let* s3, t3 = infer_expr env el in
      let* s4 = unify t1 bool_typ in
      let* s5 = unify t2 t3 in
      let* final_subst = Subst.compose_all [ s5; s4; s3; s2; s1 ] in
      return (final_subst, Subst.apply s5 t2)
    | EFun (pat, exp) ->
      let* env', pat_ty = infer_pattern env pat in
      let* s, exp_ty = infer_expr env' exp in
      let ty = Subst.apply s (pat_ty ^-> exp_ty) in
      return (s, ty)
    | ELetIn (def, expr) ->
      let* let_env, let_sub, _ = infer_def env def in
      let* exp_sub, exp_ty = infer_expr let_env expr in
      let* sub = Subst.compose let_sub exp_sub in
      return (sub, exp_ty)
    | ETuple xs ->
      List.fold_right
        xs
        ~init:(return (Subst.empty, []))
        ~f:(fun x acc ->
          let* acc_sub, acc_t = acc in
          let* s, t = infer_expr env x in
          let* s' = Subst.compose acc_sub s in
          return (s', t :: acc_t))
      >>| fun (s, t) -> s, TTuple (List.map t ~f:(Subst.apply s))
    | EList xs ->
      (match xs with
       | [] ->
         let* fv = fresh_var in
         return (Subst.empty, TList fv)
       | hd :: tl ->
         List.fold tl ~init:(infer_expr env hd) ~f:(fun acc x ->
           let* acc_sub, acc_t = acc in
           let* s, t = infer_expr env x in
           let* s' = unify acc_t t in
           let* sub = Subst.compose_all [ acc_sub; s; s' ] in
           return (sub, acc_t)))
      >>| fun (s, t) -> s, TList t
    | EMatch (e, pe) ->
      let* match_sub, match_ty = infer_expr env e in
      let* fv = fresh_var in
      let* s', t' =
        List.fold
          pe
          ~init:(return (match_sub, fv))
          ~f:(fun acc (p, e) ->
            let* acc_sub, acc_ty = acc in
            let* pat_env, pat_ty = infer_pattern env p in
            let* pat_sub = unify match_ty pat_ty in
            let* exp_sub, exp_ty = infer_expr pat_env e in
            let* sub'' = unify acc_ty exp_ty in
            let* final_subst = Subst.compose_all [ acc_sub; pat_sub; exp_sub; sub'' ] in
            return (final_subst, Subst.apply final_subst acc_ty))
      in
      let* final_subst = Subst.compose match_sub s' in
      return (final_subst, Subst.apply final_subst t')
  and (infer_def : TypeEnv.t -> Ast.definition -> (TypeEnv.t * Subst.t * ty) R.t) =
    fun env -> function
    | DLet (NonRec, pat, expr) ->
      (* TODO: limit to wild, idnet, tuple, ann (in parser?). no const, list, cons *)
      let* exp_sub, exp_ty = infer_expr env expr in
      (* dbg "ty: %a\nSUB: %a\n" pp_typ exp_ty  Subst.pp exp_sub; *)
      let env' = TypeEnv.apply env exp_sub in
      let scheme = generalize env' exp_ty in
      (* dbg "scheme: %a\n" pp_scheme scheme; *)
      let* pat_env, pat_ty = infer_pattern env pat in
      let* pat_env' = TypeEnv.extend_pat pat_env pat scheme in
      (* dbg "before: %a\nafter:  %a\n" TypeEnv.pp pat_env TypeEnv.pp pat_env'; *)
      let* sub = Subst.unify pat_ty exp_ty in
      (* dbg "sub: %a\n" Subst.pp sub; *)
      let* final_sub = Subst.compose sub exp_sub in
      let final_env = TypeEnv.apply pat_env' final_sub in
      let final_ty = Subst.apply final_sub exp_ty in
      return (final_env, final_sub, final_ty)
    | DLet (Rec, (PIdent _ as pat), expr) | DLet (Rec, (PAnn (PIdent _, _) as pat), expr)
      ->
      (* TODO: check (copy-paste from NonRec case) *)
      let* pat_env, pat_ty = infer_pattern env pat in
      let* exp_sub, exp_ty = infer_expr pat_env expr in
      let env' = TypeEnv.apply env exp_sub in
      let scheme = generalize env' exp_ty in
      let* pat_env' = TypeEnv.extend_pat pat_env pat scheme in
      let* sub = Subst.unify pat_ty exp_ty in
      let* final_sub = Subst.compose exp_sub sub in
      let final_env = TypeEnv.apply pat_env' final_sub in
      let final_ty = Subst.apply final_sub exp_ty in
      return (final_env, final_sub, final_ty)
    | DLet (_, pat, _) ->
      fail
        (`TODO (Format.asprintf "Can't use %a in let rec expression" Ast.pp_pattern pat))
  in
  infer_def
;;

let rec ids_from_pattern pat =
  let open Format in
  match pat with
  | Ast.PWild -> "_"
  | PIdent x -> x
  | PTuple xs ->
    xs
    |> List.map ~f:ids_from_pattern
    |> List.intersperse ~sep:", "
    |> List.fold ~init:"" ~f:( ^ )
    |> asprintf "(%s)"
  | PAnn (x, _) -> asprintf "%s" (ids_from_pattern x)
  | _ -> failwith "unreachable?"
;;

let infer_program (prog : Ast.definition list) =
  let rec helper env = function
    | head :: tail ->
      (match head with
       | Ast.DLet (_, pat, _) ->
         let* env', _, ty = infer env head in
         let* tail = helper env' tail in
         let id = ids_from_pattern pat in
         return ((id, ty) :: tail)
         (* | Ast.DLet (NonRec, PWild, _) ->
            let* _ = infer env head in
            let* tail = helper env tail in
            return tail *))
    | [] -> return []
  in
  let env = TypeEnv.default in
  helper env prog
;;

let inference_program prog =
  (* run (infer_program prog) |> Result.map ~f:(List.map ~f:(fun x -> snd (snd x))) *)
  run (infer_program prog)
;;

let test code =
  let open Format in
  let pa = false in
  let ast = Result.ok_or_failwith (Parser.parse_program ~print_ast:pa code) in
  match inference_program ast with
  | Ok t ->
    printf
      "%s"
      (t
       |> List.map ~f:(fun (s, t) -> asprintf "%s: %a" s pp_typ t)
       |> String.concat ~sep:"\n")
  | Error e -> eprintf "%a" pp_error e
;;

let%expect_test _ =
  test {| let x = () |};
  [%expect {| x: () |}]
;;

let%expect_test _ =
  test {| let x = true |};
  [%expect {| x: bool |}]
;;

let%expect_test _ =
  test {| let x = 42 |};
  [%expect {| x: int |}]
;;

let%expect_test _ =
  test {| let x = [1; 2; 3] |};
  [%expect {| x: int list |}]
;;

let%expect_test _ =
  test {| let x = [true; 2; 3] |};
  [%expect {| Unification failed on bool and int |}]
;;

let%expect_test _ =
  test {| let x = [[1]; [2]; [3]] |};
  [%expect {| x: int list list |}]
;;

let%expect_test _ =
  test {| let x = (1, true, fun x -> x, (1, 2)) |};
  [%expect {| x: (int, bool, 'a -> 'a, (int, int)) |}]
;;

let%expect_test _ =
  test {| let x = 1 + 2 |};
  [%expect {| x: int |}]
;;

let%expect_test _ =
  test {| let x = 1 + 2 <= 3 |};
  [%expect {| x: bool |}]
;;

let%expect_test _ =
  test {| let id = fun x -> x |};
  [%expect {| id: 'a -> 'a |}]
;;

let%expect_test _ =
  test {| let const = fun x -> 42 |};
  [%expect {| const: 'a -> int |}]
;;

let%expect_test _ =
  test {| let plus_one = fun x -> x + 1 |};
  [%expect {| plus_one: int -> int |}]
;;

let%expect_test _ =
  test {| let muladd = fun x -> fun y -> fun z -> x * y + z |};
  [%expect {| muladd: int -> int -> int -> int |}]
;;

let%expect_test _ =
  test {| let apply = func arg |};
  [%expect {| Undefined variable 'func' |}]
;;

let%expect_test _ =
  test {| let apply = let plus_one = (fun x -> x + 1) in plus_one 2 |};
  [%expect {| apply: int |}]
;;

let%expect_test _ =
  test
    {|
    let compose = 
      let plus_one = fun x -> x + 1 in
      let is_neg = fun x -> x < 0 in
      fun x -> is_neg (plus_one x)
  |};
  [%expect {| compose: int -> bool |}]
;;

let%expect_test _ =
  test {| let compose = let func = fun x -> 42 in let func = fun x -> true in func |};
  [%expect {| compose: 'e -> bool |}]
;;

let%expect_test _ =
  test {| let cond = if true then 42 else false |};
  [%expect {| Unification failed on int and bool |}]
;;

let%expect_test _ =
  test {| let cond = if true then 42 else 0 |};
  [%expect {| cond: int |}]
;;

let%expect_test _ =
  test {| let rec fact = fun x -> if x < 2 then 1 else x * fact (x - 1) |};
  [%expect {| fact: int -> int |}]
;;

let%expect_test _ =
  test {| let rec oc = fun x -> x x |};
  [%expect {| Occurs check failed |}]
;;

let%expect_test _ =
  test {|
    let plus_one = fun x -> x + 1
    let is_neg = fun x -> x < 0
  |};
  [%expect {|
    plus_one: int -> int
    is_neg: int -> bool 
  |}]
;;

let%expect_test _ =
  test
    {|
    let add = fun x -> fun y -> x + y
    let add_one = add 1
    let add_two = add 2
    let x = add_one 3
    let y = add_two 3
  |};
  [%expect
    {|
    add: int -> int -> int
    add_one: int -> int
    add_two: int -> int
    x: int
    y: int
  |}]
;;

let%expect_test _ =
  test
    {|      
    let rec helper = fun n -> fun cont ->
      if n < 2 then 
        cont 1
      else 
        helper (n - 1) (fun res -> cont (n * res)) 

    let fact = fun n ->
      helper n (fun x -> x)

    let x = fact 5
  |};
  [%expect {|
    helper: int -> (int -> 'o) -> 'o
    fact: int -> int
    x: int
  |}]
;;

let%expect_test _ =
  test
    {|
      let fact_2 = fun n -> 
         let rec helper = fun n -> fun acc -> fun cont ->
          if n < 2 then
              cont acc
          else 
              helper (n - 1) (n * acc) cont 
         in

         helper n 1 (fun x -> x)
      
      let x = fact_2 5
    |};
  [%expect {|
    fact_2: int -> int
    x: int
  |}]
;;

let%expect_test _ =
  test {| let (x: int) = 42 |};
  [%expect {| x: int |}]
;;

let%expect_test _ =
  test {| let (x: int) = true |};
  [%expect {| Unification failed on int and bool |}]
;;

let%expect_test _ =
  test {| let (id: int->int) = fun x -> x |};
  [%expect {| id: int -> int |}]
;;

let%expect_test _ =
  test {| let (id: int->int) = fun (x: int) -> x |};
  [%expect {| id: int -> int |}]
;;

let%expect_test _ =
  test {| let (id: int->int) = fun (x: bool) -> x |};
  [%expect {| Unification failed on int and bool |}]
;;

let%expect_test _ =
  test {| let id = fun (x: int) -> x |};
  [%expect {| id: int -> int |}]
;;

let%expect_test _ =
  test {| let (const: int) = (fun x -> 42) () |};
  [%expect {| const: int |}]
;;

(* TODO: more tests for patterns and type annotations *)

let%expect_test _ =
  test {|let f = fun (f: int -> int -> int) -> fun x -> fun y -> f x y|};
  [%expect {| f: (int -> int -> int) -> int -> int -> int |}]
;;

let%expect_test _ =
  test {|let compose = fun f -> fun g -> fun x -> f (g x)|};
  [%expect {| compose: ('d -> 'e) -> ('c -> 'd) -> 'c -> 'e  |}]
;;

let%expect_test _ =
  test {|let choose = fun l -> fun r -> fun b -> if b then l else r|};
  [%expect {| choose: 'b -> 'b -> bool -> 'b |}]
;;

let%expect_test _ =
  test {|let choose = fun (l: bool) -> fun r -> fun b -> if b then l else r|};
  [%expect {| choose: bool -> bool -> bool -> bool |}]
;;

let%expect_test _ =
  test
    {|
  let (+) = fun a -> fun b -> a || b 
  let a = true + false
  let (*) = fun a -> fun b -> fun c -> a * b * c 
  let b = (2 * 3) 4
  |};
  [%expect
    {|
    ( + ): bool -> bool -> bool
    a: bool
    ( * ): int -> int -> int -> int
    b: int |}]
;;

let%expect_test _ =
  test {| 
  let x = 1 
  let y = -x
  let z = true
  let w = not z
  |};
  [%expect {|
    x: int
    y: int
    z: bool
    w: bool |}]
;;

let%expect_test _ =
  test
    {| 
      let rec fib = fun (n: int) -> match n with
      | 0 -> 0
      | 1 -> 1
      | _ -> (fib (n - 1)) + (fib (n - 2))
  |};
  [%expect {| fib: int -> int |}]
;;

let%expect_test _ =
  test {| let rec fix = fun f -> fun x -> f (fix f) x |};
  [%expect {| fix: (('c -> 'f) -> 'c -> 'f) -> 'c -> 'f |}]
;;

let%expect_test _ =
  test
    {| 
    let (x, y, z) = (not true, 42, fun x -> if x > 0 then true else false)
    let a = x
    let b = y
    let c = z
  |};
  [%expect
    {|
    (x, y, z): (bool, int, int -> bool)
    a: bool
    b: int
    c: int -> bool |}]
;;

let%expect_test _ =
  test {| let (x, y, z) = (not true, 42) |};
  [%expect {| Unification failed on (x, y, z) and (bool, int) |}]
;;
