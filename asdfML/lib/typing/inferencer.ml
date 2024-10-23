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

  let pp fmt (xs : t) =
    Format.fprintf fmt "{| ";
    Map.iteri xs ~f:(fun ~key:n ~data:s -> Format.fprintf fmt "%s -> %a; " n pp_scheme s);
    Format.fprintf fmt "|}%!"
  ;;
end

open R
open R.Syntax

let unify = Subst.unify

let unify_ann an_ty ty =
  match an_ty with
  | Some an_ty ->
    let* u = unify (an_ty_to_ty an_ty) ty in
    return (Subst.apply u ty)
  | None -> return ty
;;

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
  let infer_pattern : TypeEnv.t -> Ast.pattern -> (TypeEnv.t * ty) R.t =
    fun env -> function
    | PConst c -> return (env, infer_const c)
    | PWild -> fresh_var >>| fun v -> env, v
    | PIdent (x, ta) -> fail (`TODO "")
    | PTuple xs -> fail (`TODO "")
    | PList x -> fail (`TODO "")
    | PCons (l, r) -> fail (`TODO "")
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
    | EFun (PIdent (x, an_ty), e1) ->
      let* tv = fresh_var in
      let* tv = unify_ann an_ty tv in
      let env2 = TypeEnv.extend env x (VarSet.empty, tv) in
      let* s, ty = infer_expr env2 e1 in
      let trez = TArrow (Subst.apply s tv, ty) in
      return (s, trez)
    | ELetIn ((DLet (NonRec, PIdent (id, an_ty), _) as def), expr) ->
      let* subst_def, typ_def = infer_def env def in
      let* typ_def = unify_ann an_ty typ_def in
      (* let () = dbg "subst_def: %a\n" Subst.pp subst_def in *)
      let env' = TypeEnv.apply env subst_def in
      let typ_id = generalize env' typ_def in
      let env'' = TypeEnv.extend env' id typ_id in
      let* subst_expr, typ_expr = infer_expr env'' expr in
      let* final_subst = Subst.compose subst_def subst_expr in
      return (final_subst, typ_expr)
    | ELetIn ((DLet (Rec, PIdent (id, an_ty), _) as def), expr) ->
      let* tvar = fresh_var in
      let env = TypeEnv.extend env id (VarSet.empty, tvar) in
      let* s1, t1 = infer_def env def in
      let* t1 = unify_ann an_ty t1 in
      let* s2 = unify (Subst.apply s1 tvar) t1 in
      let* s = Subst.compose s2 s1 in
      let env = TypeEnv.apply env s in
      let t2 = generalize env (Subst.apply s tvar) in
      let* s2, t2 = infer_expr TypeEnv.(extend (apply env s) id t2) expr in
      let* final_subst = Subst.compose s s2 in
      return (final_subst, t2)
    | ELetIn (DLet (_, non_id, _), _) ->
      fail
        (`TODO (Format.asprintf "Can't use %a in let expression" Ast.pp_pattern non_id))
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
    (* | EFun ((PWild | PConst _ | PTuple _ | PList _ | PCons (_, _)), _) -> *)
    | _ -> fail (`TODO "unimplemented (infer_expr)")
  and (infer_def : TypeEnv.t -> Ast.definition -> (Subst.t * ty) R.t) =
    fun env -> function
    | DLet (_, _, expr) ->
      let* subst, typ_expr = infer_expr env expr in
      return (subst, typ_expr)
  in
  infer_def
;;

let infer_program (prog : Ast.definition list) =
  let rec helper env = function
    | head :: tail ->
      (match head with
       | Ast.DLet (NonRec, PIdent (id, an_ty), _) ->
         let* _, ty = infer env head in
         let* ty = unify_ann an_ty ty in
         let t = generalize env ty in
         let env = TypeEnv.extend env id t in
         let* tail = helper env tail in
         return ((id, t) :: tail)
       | Ast.DLet (Rec, PIdent (id, an_ty), _) ->
         let* type_variable = fresh_var in
         let env = TypeEnv.extend env id (VarSet.empty, type_variable) in
         let* subst, ty = infer env head in
         let* ty = unify_ann an_ty ty in
         let* subst' = unify (Subst.apply subst type_variable) ty in
         let* final_subst = Subst.compose subst' subst in
         let env = TypeEnv.apply env final_subst in
         let generalized_type = generalize env (Subst.apply final_subst type_variable) in
         let* tail = helper (TypeEnv.extend env id generalized_type) tail in
         return ((id, generalized_type) :: tail)
       | Ast.DLet (NonRec, PWild, _) ->
         let* tail = helper env tail in
         return tail
       | Ast.DLet (_, non_id_wild, _) ->
         failwith
           (Format.asprintf "Can't use %a in let expression" Ast.pp_pattern non_id_wild))
    | [] -> return []
  in
  let env = TypeEnv.default in
  helper env prog
;;

let inference_definition ast = Result.map (run (infer TypeEnv.empty ast)) snd

let inference_program prog =
  run (infer_program prog) |> Result.map ~f:(List.map ~f:(fun x -> snd (snd x)))
;;

let test code =
  let open Format in
  let pa = false in
  let ast = Result.ok_or_failwith (Parser.parse_program ~print_ast:pa code) in
  match inference_program ast with
  | Ok t ->
    printf "%s" (t |> List.map ~f:(asprintf "%a" pp_typ) |> String.concat ~sep:"\n")
  | Error e -> eprintf "%a" pp_error e
;;

let%expect_test _ =
  test {| let x = () |};
  [%expect {| () |}]
;;

let%expect_test _ =
  test {| let x = true |};
  [%expect {| bool |}]
;;

let%expect_test _ =
  test {| let x = 42 |};
  [%expect {| int |}]
;;

let%expect_test _ =
  test {| let x = [1; 2; 3] |};
  [%expect {| int list |}]
;;

let%expect_test _ =
  test {| let x = [true; 2; 3] |};
  [%expect {| Unification failed on bool and int |}]
;;

let%expect_test _ =
  test {| let x = [[1]; [2]; [3]] |};
  [%expect {| int list list |}]
;;

let%expect_test _ =
  test {| let x = (1, true, fun x -> x, (1, 2)) |};
  [%expect {| (int, bool, 'a -> 'a, (int, int)) |}]
;;

let%expect_test _ =
  test {| let x = 1 + 2 |};
  [%expect {| int |}]
;;

let%expect_test _ =
  test {| let x = 1 + 2 <= 3 |};
  [%expect {| bool |}]
;;

let%expect_test _ =
  test {| let id = fun x -> x |};
  [%expect {| 'a -> 'a |}]
;;

let%expect_test _ =
  test {| let const = fun x -> 42 |};
  [%expect {| 'a -> int |}]
;;

let%expect_test _ =
  test {| let plus_one = fun x -> x + 1 |};
  [%expect {| int -> int |}]
;;

let%expect_test _ =
  test {| let muladd = fun x -> fun y -> fun z -> x * y + z |};
  [%expect {| int -> int -> int -> int |}]
;;

let%expect_test _ =
  test {| let apply = func arg |};
  [%expect {| Undefined variable 'func' |}]
;;

let%expect_test _ =
  test {| let apply = let plus_one = (fun x -> x + 1) in plus_one 2 |};
  [%expect {| int |}]
;;

let%expect_test _ =
  test
    {|
    let compose = 
      let plus_one = fun x -> x + 1 in
      let is_neg = fun x -> x < 0 in
      fun x -> is_neg (plus_one x)
  |};
  [%expect {| int -> bool |}]
;;

let%expect_test _ =
  test {| let compose = let func = fun x -> 42 in let func = fun x -> true in func |};
  [%expect {| 'c -> bool |}]
;;

let%expect_test _ =
  test {| let cond = if true then 42 else false |};
  [%expect {| Unification failed on int and bool |}]
;;

let%expect_test _ =
  test {| let cond = if true then 42 else 0 |};
  [%expect {| int |}]
;;

let%expect_test _ =
  test {| let rec fact = fun x -> if x < 2 then 1 else x * fact (x - 1) |};
  [%expect {| int -> int |}]
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
    int -> int
    int -> bool 
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
  [%expect {|
    int -> int -> int
    int -> int
    int -> int
    int
    int
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
    int -> (int -> 'o) -> 'o
    int -> int
    int
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
    int -> int
    int
  |}]
;;

let%expect_test _ =
  test {| let (x:int) = 42 |};
  [%expect {| int |}]
;;

let%expect_test _ =
  test {| let (x:int) = true |};
  [%expect {| Unification failed on int and bool |}]
;;

let%expect_test _ =
  test {| let (id:int->int) = fun (x:int) -> x |};
  [%expect {| int -> int |}]
;;

let%expect_test _ =
  test {| let (id:int->int) = fun x -> x |};
  [%expect {| int -> int |}]
;;

let%expect_test _ =
  test {| let id = fun (x:int) -> x |};
  [%expect {| int -> int |}]
;;

let%expect_test _ =
  test {| let (const:int) = (fun x -> 42) () |};
  [%expect {| int |}]
;;

(* TODO: more tests for patterns and type annotations *)

let%expect_test _ =
  test {|let f = fun (f: int -> int -> int) -> fun x -> fun y -> f x y|};
  [%expect {| (int -> int -> int) -> int -> int -> int |}]
;;

let%expect_test _ =
  test {|let compose = fun f -> fun g -> fun x -> f (g x)|};
  [%expect {| ('d -> 'e) -> ('c -> 'd) -> 'c -> 'e  |}]
;;

let%expect_test _ =
  test {|let choose = fun l -> fun r -> fun b -> if b then l else r|};
  [%expect {| 'b -> 'b -> bool -> 'b |}]
;;

let%expect_test _ =
  test {|let choose = fun (l:bool) -> fun r -> fun b -> if b then l else r|};
  [%expect {| bool -> bool -> bool -> bool |}]
;;

let%expect_test _ =
  test {|
  let (+) = fun a -> fun b -> a || b 
  let a = true + false
  |};
  [%expect {|
    bool -> bool -> bool
    bool |}]
;;

let%expect_test _ =
  test {| 
  let x = 1 
  let y = -x
  let z = true
  let w = not z
  |};
  [%expect {|
    int
    int
    bool
    bool |}]
;;

(* let%expect_test _ =
  test {| let (x, y) = (not true, not false) |};
  [%expect
    {|
     |}]
;; *)

let%expect_test _ =
  test
    {| 
      let rec fib = fun (n:int) -> match n with
      | 0 -> 0
      | 1 -> 1
      | _ -> (fib (n - 1)) + (fib (n - 2))
  |};
  [%expect {| |}]
;;
