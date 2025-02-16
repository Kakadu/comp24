open Base
module Format = Stdlib.Format

type var_id = int [@@deriving show]

(* Inferred types *)
type inf_type =
  | TInt
  | TBool
  | TString
  | TUnit
  | TList of inf_type (* 'a list *)
  | TTuple of inf_type list (* ('a, 'b, ...) *)
  | TArrow of inf_type * inf_type (* 'a -> 'a *)
  | TPVar of var_id
[@@deriving show]

type error =
  | Variable_not_found of string
  | Unification_failed of inf_type * inf_type
  | Illegal_pattern of Ast.pattern
  | Is_not_function of inf_type
  | Unsupported_type
  | Empty_program
[@@deriving show]

module VarSet = struct
  (** Set of variable ids *)
  include Stdlib.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

module R = struct
  type 'a t = var_id -> var_id * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Error x
    | Ok a -> f a last
  ;;

  let fail e st = st, Result.fail e
  let return x last = last, Result.return x
  let bind x f = x >>= f

  module R = struct
    type 'a t = var_id -> var_id * ('a, error) Result.t

    let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
      fun m f st ->
      let last, r = m st in
      match r with
      | Result.Error x -> last, Error x
      | Ok a -> f a last
    ;;

    let fail e st = st, Result.fail e
    let return x last = last, Result.return x
    let bind x f = x >>= f

    let fold lst ~init ~f =
      List.fold lst ~init ~f:(fun acc e -> bind acc (fun acc -> f acc e))
    ;;

    let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
      fun x f st ->
      match x st with
      | st, Ok x -> st, Ok (f x)
      | st, Result.Error e -> st, Result.Error e
    ;;

    module Syntax = struct
      let ( let* ) = bind
      let ( >>= ) = bind
    end

    let fresh : int t = fun last -> last + 1, Result.Ok last
    let get_fresh : int t = fun last -> last, Result.Ok last
    let run m = snd (m 0)
  end

  let fold lst ~init ~f =
    List.fold lst ~init ~f:(fun acc e -> bind acc (fun acc -> f acc e))
  ;;

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun x f st ->
    match x st with
    | st, Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.Error e
  ;;

  module Syntax = struct
    let ( let* ) = bind
    let ( >>= ) = bind
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let get_fresh : int t = fun last -> last, Result.Ok last
  let run m = snd (m 0)
end

module Type = struct
  (** Basic work with types *)
  type t = inf_type

  let rec occurs_in (id : var_id) = function
    (* Does the variable occur in the passed type *)
    | TBool | TInt | TString | TUnit -> false
    | TList x -> occurs_in id x
    | TTuple tl -> List.fold tl ~init:false ~f:(fun acc t -> acc || occurs_in id t)
    | TArrow (x, y) -> occurs_in id x || occurs_in id y
    | TPVar v_id -> id = v_id
  ;;

  let free_vars t =
    (* Collect free variables in the passed type *)
    let rec helper acc = function
      | TBool | TInt | TString | TUnit -> acc
      | TPVar v -> VarSet.add v acc
      | TArrow (x, y) -> helper (helper acc x) y
      | TList x -> helper acc x
      | TTuple tl -> List.fold tl ~init:acc ~f:helper
    in
    helper VarSet.empty t
  ;;
end

module Subst = struct
  (** Substitution logic *)
  open R

  open R.Syntax

  (* var_id => inf_type (substitute it) *)
  type t = (var_id, inf_type, Int.comparator_witness) Map.t

  let empty : t = Map.empty (module Int)
  let singleton k v = Map.singleton (module Int) k v

  let singleton_checked id t =
    if Type.occurs_in id t
    then R.fail (Variable_not_found (Int.to_string id))
    else R.return (singleton id t)
  ;;

  let find key s = Map.find s key
  let remove k s = Map.remove s k

  let apply t s =
    (* Apply a substitutions to type, return new type *)
    let rec helper x =
      match x with
      | TPVar v ->
        let find_type = find v s in
        (match find_type with
         | Some ft -> ft
         | _ -> x)
      | TArrow (x, e) -> TArrow (helper x, helper e)
      | TList x -> TList (helper x)
      | TTuple tl -> TTuple (List.map ~f:helper tl)
      | _ -> x
    in
    helper t
  ;;

  let rec unify t1 t2 =
    (* Returns the substitutions needed to unify types *)
    match t1, t2 with
    | TBool, TBool | TInt, TInt | TString, TString | TUnit, TUnit -> return empty
    | TPVar x, TPVar y when x = y -> return empty
    | TPVar x, (_ as y) | (_ as y), TPVar x -> singleton_checked x y
    | TArrow (x, xs), TArrow (y, ys) ->
      let* s1 = unify x y in
      let* s2 = unify (apply xs s1) (apply ys s1) in
      compose s1 s2
    | TList x, TList y -> unify x y
    | TTuple xs, TTuple ys ->
      let unify_lists l1 l2 =
        let subs =
          List.fold2 l1 l2 ~init:(return empty) ~f:(fun subs a b ->
            let* subs = subs in
            let sa = apply a subs in
            let sb = apply b subs in
            let* sub1 = unify sa sb in
            compose subs sub1)
        in
        match subs with
        | Ok res -> res
        | Unequal_lengths -> fail (Unification_failed (t1, t2))
      in
      unify_lists xs ys
    | _ -> fail (Unification_failed (t1, t2))

  and extend (id, t) s =
    (* Add new substitution *)
    match find id s with
    | Some t2 ->
      let* unified = unify t t2 in
      compose s unified
    | None ->
      let t = apply t s in
      let* new_s = singleton_checked id t in
      Map.fold s ~init:(return new_s) ~f:(fun ~key ~data acc ->
        let* acc = acc in
        let data = apply data acc in
        let* _ = singleton_checked key data in
        Map.set acc ~key ~data |> return)

  (* Combine two substitutions into one *)
  and compose before after =
    Map.fold after ~init:(return before) ~f:(fun ~key ~data acc ->
      let* acc = acc in
      extend (key, data) acc)
  ;;

  let compose_all ss =
    List.fold ss ~init:(return empty) ~f:(fun acc s ->
      let* acc = acc in
      compose acc s)
  ;;

  let pp ppf (subst : t) =
    let subs_list = Map.to_alist subst in
    Format.fprintf
      ppf
      "{ %a }"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
         (fun ppf (k, v) -> Format.fprintf ppf "%d => %a" k pp_inf_type v))
      subs_list
  ;;
end

let fresh_var =
  let open R in
  fresh >>= fun n -> return (TPVar n)
;;

(* ∀α. α -> α *)
(* Scheme([1], TArrow(TVar(1), TVar(1))) *)
type scheme = Scheme of VarSet.t * inf_type

module Scheme = struct
  type t = scheme

  let create (t : inf_type) : t = Scheme (VarSet.empty, t)

  (* find free variables that are not in varset  *)
  let free_vars = function
    | Scheme (bs, t) -> VarSet.diff (Type.free_vars t) bs
  ;;

  (* apply substitution to scheme *)
  (* apply {α => int} (∀α. α -> α) => int -> int *)
  let apply subst = function
    | Scheme (bs, t) ->
      let partial_subst = VarSet.fold Subst.remove bs subst in
      Scheme (bs, Subst.apply t partial_subst)
  ;;

  (* Replaces all generic variables (α) with new unique variables *)
  let instantiate = function
    | Scheme (bs, t) ->
      let open R.Syntax in
      VarSet.fold
        (fun b acc ->
           let* acc = acc in
           let* fr = fresh_var in
           let subst = Subst.singleton b fr in
           Subst.apply acc subst |> R.return)
        bs
        (R.return t)
  ;;

  let pp fmt = function
    | Scheme (bs, t) ->
      VarSet.pp fmt bs;
      pp_inf_type fmt t
  ;;
end

type var_name = string

module BinOperator = struct
  let list : Ast.bop list =
    [ ADD; SUB; MUL; DIV; EQ; ID_EQ; NEQ; GT; GTE; LT; LTE; AND; OR; CONCAT ]
  ;;

  let to_string : Ast.bop -> string = function
    | ADD -> "+"
    | SUB -> "-"
    | MUL -> "*"
    | DIV -> "/"
    | EQ -> "="
    | ID_EQ -> "=="
    | NEQ -> "!="
    | GT -> ">"
    | GTE -> ">="
    | LT -> "<"
    | LTE -> "<="
    | AND -> "&&"
    | OR -> "||"
    | CONCAT -> "^"
  ;;

  let to_inf_type : Ast.bop -> inf_type = function
    | ADD | SUB | MUL | DIV -> TArrow (TInt, TArrow (TInt, TInt))
    | EQ | ID_EQ | NEQ | GT | GTE | LT | LTE -> TArrow (TInt, TArrow (TInt, TBool))
    | AND | OR -> TArrow (TBool, TArrow (TBool, TBool))
    | CONCAT -> TArrow (TString, TArrow (TString, TString))
  ;;

  let to_scheme (bop : Ast.bop) : scheme = Scheme (VarSet.empty, to_inf_type bop)
end

module TypeEnv = struct
  (** Environment: name + its schema *)

  type t = (var_name, Scheme.t, String.comparator_witness) Map.t

  let empty : t = Map.empty (module String)

  let default : t =
    let bop_schemes =
      List.map BinOperator.list ~f:(fun bop ->
        BinOperator.to_string bop, BinOperator.to_scheme bop)
    in
    (* default environment data *)
    let data =
      [ "print_string", Scheme (VarSet.empty, TArrow (TString, TUnit))
      ; "print_int", Scheme (VarSet.empty, TArrow (TInt, TUnit))
      ]
      @ bop_schemes
    in
    List.fold data ~init:empty ~f:(fun acc (k, v) -> Map.set acc ~key:k ~data:v)
  ;;

  (* find names in env *)
  let find_exn (name : var_name) (env : t) = Map.find_exn env name
  let find (name : var_name) (env : t) = Map.find env name

  (* get free vars from all schemes *)
  let free_vars (env : t) =
    Map.fold env ~init:VarSet.empty ~f:(fun ~key:_ ~data:scheme acc ->
      VarSet.union acc (Scheme.free_vars scheme))
  ;;

  (* extend environment by new name and scheme *)
  let extend (name : var_name) (scheme : scheme) (env : t) : t =
    match Map.add env ~key:name ~data:scheme with
    | `Ok m -> m
    | `Duplicate -> Map.set env ~key:name ~data:scheme
  ;;

  (* apply substitution *)
  let apply (subst : Subst.t) (env : t) =
    Map.fold env ~init:empty ~f:(fun ~key ~data acc ->
      let refined = Scheme.apply subst data in
      extend key refined acc)
  ;;

  (* Create scheme for type using environment *)
  let generalize (t : inf_type) (env : t) =
    let quantified = VarSet.diff (Type.free_vars t) (free_vars env) in
    Scheme (quantified, t)
  ;;

  (* name = expr *)
  let rec generalize_pattern (name_p : Ast.pattern) (expr_t : inf_type) (env : t) =
    match name_p, expr_t with
    | Const _, _ -> env
    | Wildcard, _ -> env
    | Var id, t -> extend id (generalize t env) env
    | ListConcat (hd, tl), TList lst_t ->
      let env = generalize_pattern hd lst_t env in
      generalize_pattern tl expr_t env
    | List lst, TList lst_t ->
      List.fold lst ~init:env ~f:(fun acc p -> generalize_pattern p lst_t acc)
    | Tuple tls, TTuple tls_t ->
      List.fold2_exn tls tls_t ~init:env ~f:(fun acc p t -> generalize_pattern p t acc)
    | Constraint (p, PInt), _ -> generalize_pattern p TInt env
    | Constraint (p, PString), _ -> generalize_pattern p TString env
    | Constraint (p, PBool), _ -> generalize_pattern p TBool env
    | Operation (Binary op), t -> extend (BinOperator.to_string op) (generalize t env) env
    | _ -> failwith "Incorrect generalize pattern"
  ;;

  (* Find scheme by name *)
  let lookup (name : var_name) (env : t) =
    let open R.Syntax in
    match find name env with
    | None -> R.fail (Variable_not_found name)
    | Some scheme ->
      let* t = Scheme.instantiate scheme in
      R.return (Subst.empty, t)
  ;;

  let pp fmt (env : t) =
    let pp_binding fmt (var_name, scheme) =
      Format.fprintf fmt "@[<h>%s : %a@]" var_name Scheme.pp scheme
    in
    Format.fprintf fmt "@[<v>";
    Map.iteri env ~f:(fun ~key ~data ->
      Format.fprintf fmt "%a@;" pp_binding (key, data);
      Format.fprintf fmt "@]")
  ;;
end

module Infer = struct
  open Ast
  open R
  open R.Syntax

  let infer_value : Ast.value -> (Subst.t * inf_type) R.t = function
    | Int _ -> return (Subst.empty, TInt)
    | Bool _ -> return (Subst.empty, TBool)
    | String _ -> return (Subst.empty, TString)
    | Unit -> return (Subst.empty, TUnit)
  ;;

  let infer_data_type : Ast.dataType -> (Subst.t * inf_type) R.t = function
    | PInt -> return (Subst.empty, TInt)
    | PBool -> return (Subst.empty, TBool)
    | PString -> return (Subst.empty, TString)
  ;;

  let infer_pattern (env : TypeEnv.t) (v : Ast.pattern) : (TypeEnv.t * inf_type) R.t =
    let rec helper (env : TypeEnv.t) = function
      | Const v ->
        let* _, t = infer_value v in
        R.return (env, t)
      | Wildcard ->
        let* fr = fresh_var in
        R.return (env, fr)
        (* let f _ _ : 'a -> 'b *)
      | Var name ->
        let* fr = fresh_var in
        let new_env = TypeEnv.extend name (Scheme.create fr) env in
        R.return (new_env, fr)
      | List ps ->
        (match ps with
         | [] ->
           let* fr = fresh_var in
           R.return (env, TList fr)
         | p :: tl ->
           let* env, p_t = helper env p in
           let* env, tl_t =
             R.fold
               tl
               ~init:(R.return (env, p_t))
               ~f:(fun (env, prev_t) cur ->
                 let* env, cur_t = helper env cur in
                 let* u = Subst.unify cur_t prev_t in
                 R.return (TypeEnv.apply u env, cur_t))
           in
           R.return (env, TList tl_t))
      | ListConcat (hd, tl) ->
        (* 'a :: 'a list *)
        let* env, hd_t = helper env hd in
        let* env, tl_t = helper env tl in
        let* u = Subst.unify (TList hd_t) tl_t in
        let tl_t = Subst.apply tl_t u in
        R.return (TypeEnv.apply u env, tl_t)
      | Tuple ps ->
        (match ps with
         | [] | [ _ ] -> R.fail Unsupported_type
         | p :: tl ->
           let* env, p_t = helper env p in
           let* env, tl_t =
             R.fold
               tl
               ~init:(R.return (env, [ p_t ]))
               ~f:(fun (env, acc_t) p ->
                 let* env, p_t = helper env p in
                 R.return (env, [ p_t ] @ acc_t))
           in
           R.return (env, TTuple (List.rev tl_t)))
      | Constraint (p, dt) ->
        let* env, p_t = helper env p in
        let* _, dt_t = infer_data_type dt in
        let* s = Subst.unify p_t dt_t in
        R.return (TypeEnv.apply s env, dt_t)
      | Operation (Binary bop) ->
        let name = BinOperator.to_string bop in
        let* fr = fresh_var in
        let new_env = TypeEnv.extend name (Scheme.create fr) env in
        R.return (new_env, fr)
      | Operation (Unary uop) ->
        R.return
          ( env
          , match uop with
            | UMINUS | UPLUS -> TArrow (TInt, TInt)
            | NOT -> TArrow (TBool, TBool) )
    in
    helper env v
  ;;

  let rec infer_args (env : TypeEnv.t) (ps : Ast.pattern list)
    : (TypeEnv.t * inf_type) R.t
    =
    match ps with
    | [] -> R.fail Unsupported_type
    | [ p ] -> infer_pattern env p
    | p :: ps ->
      let* env, v_t = infer_pattern env p in
      let* env, vs_t = infer_args env ps in
      R.return (env, TArrow (v_t, vs_t))
  ;;

  (* 'a -> 'b + int <=> 'a -> 'b -> int *)
  let rec build_arrow arr end_t =
    match arr with
    | TArrow (t1, t2) -> TArrow (t1, build_arrow t2 end_t)
    | x -> TArrow (x, end_t)
  ;;

  let infer_expr (env : TypeEnv.t) (expr : Ast.expr) : (Subst.t * inf_type) R.t =
    let rec helper (env : TypeEnv.t) (expr : Ast.expr) =
      match expr with
      | EConst v -> infer_value v
      | EVar id -> TypeEnv.lookup id env
      | EOperation (Binary bop) ->
        let id = BinOperator.to_string bop in
        TypeEnv.lookup id env
      | EOperation (Unary uop) ->
        (match uop with
         | UPLUS | UMINUS -> R.return (Subst.empty, TArrow (TInt, TInt))
         | NOT -> R.return (Subst.empty, TArrow (TBool, TBool)))
      | EList lst ->
        (match lst with
         | [] ->
           (* 'a list *)
           let* fr = fresh_var in
           R.return (Subst.empty, TList fr)
         | [ p ] ->
           let* p_s, p_t = helper env p in
           R.return (p_s, TList p_t)
         | p :: tl ->
           let* p_s, p_t = helper env p in
           let* tl_s, tl_t =
             R.fold
               tl
               ~init:(R.return (p_s, p_t))
               ~f:(fun (prev_s, prev_t) cur ->
                 let* cur_s, cur_t = helper (TypeEnv.apply prev_s env) cur in
                 let* u = Subst.unify cur_t prev_t in
                 let* res_s = Subst.compose_all [ u; cur_s; prev_s ] in
                 R.return (res_s, cur_t))
           in
           R.return (tl_s, TList tl_t))
      | ETuple tls ->
        (match tls with
         | [] | [ _ ] -> R.fail Unsupported_type
         | p :: tl ->
           let* p_s, p_t = helper env p in
           let* tl_s, tl_t =
             R.fold
               tl
               ~init:(R.return (p_s, [ p_t ]))
               ~f:(fun (acc_s, acc_lst) p ->
                 let* s, p_t = helper (TypeEnv.apply acc_s env) p in
                 let* subs = Subst.compose acc_s s in
                 R.return (subs, [ p_t ] @ acc_lst))
           in
           R.return (tl_s, TTuple (List.rev tl_t)))
      | EListConcat (l, r) ->
        let* l_s, l_t = helper env l in
        let* r_s, r_t = helper (TypeEnv.apply l_s env) r in
        let* u = Subst.unify (TList l_t) r_t in
        let* subs = Subst.compose_all [ l_s; r_s; u ] in
        R.return (subs, r_t)
      | EConstraint (e, dt) ->
        let* e_s, e_t = helper env e in
        let* dt_s, dt_t = infer_data_type dt in
        let* u = Subst.unify e_t dt_t in
        let* subs = Subst.compose_all [ e_s; dt_s; u ] in
        R.return (subs, dt_t)
      | If (i, th, el) ->
        let* if_s, if_t = helper env i in
        let* if_u = Subst.unify if_t TBool in
        let* if_s = Subst.compose if_u if_s in
        let* then_s, then_t = helper (TypeEnv.apply if_s env) th in
        let* then_s = Subst.compose then_s if_s in
        (match el with
         | Some el ->
           let* else_s, else_t = helper (TypeEnv.apply then_s env) el in
           let* else_s = Subst.compose else_s then_s in
           let* then_else_u = Subst.unify then_t else_t in
           let* fin_s = Subst.compose_all [ if_s; else_s; then_else_u ] in
           R.return (fin_s, Subst.apply else_t fin_s)
         | None ->
           let* th_u = Subst.unify then_t TUnit in
           let* fin_s = Subst.compose_all [ if_s; th_u ] in
           R.return (fin_s, TUnit))
      | Fun (args, expr) ->
        let* env, args_t = infer_args env args in
        let* expr_s, expr_t = helper env expr in
        let args_t = Subst.apply args_t expr_s in
        R.return (expr_s, build_arrow args_t expr_t)
      | Let (Nonrecursive, binds, scope) ->
        (match binds with
         | [] -> R.fail Unsupported_type
         (* let f = ... [IN ...] *)
         | (name, args, expr) :: tl ->
           let infer_bind env name args expr =
             match args with
             (* let a,b = 1,2 *)
             | [] ->
               let* expr_s, expr_t = helper env expr in
               let* env, name_t = infer_pattern env name in
               let* u = Subst.unify name_t expr_t in
               let* subs = Subst.compose u expr_s in
               let expr_t = Subst.apply expr_t subs in
               let env = TypeEnv.generalize_pattern name expr_t env in
               R.return (env, subs, expr_t)
             (* (match scope with
                | None -> R.return (subs, expr_t)
                (* IN <scope> *)
                | Some scope ->
                  let* scope_s, scope_t = helper env scope in
                  let* subs = Subst.compose subs scope_s in
                  R.return (subs, scope_t)) *)
             (* let f x y = x + y *)
             | args ->
               let* arg_env, args_t = infer_args env args in
               let* expr_s, expr_t = helper arg_env expr in
               let args_t = Subst.apply args_t expr_s in
               let env =
                 TypeEnv.generalize_pattern name (build_arrow args_t expr_t) env
               in
               R.return (env, expr_s, expr_t)
             (* (match scope with
                | None -> R.return (expr_s, build_arrow args_t expr_t)
                (* IN <scope> *)
                | Some scope ->
                  let* scope_s, scope_t = helper env scope in
                  R.return (scope_s, scope_t)) *)
           in
            let* env, fst_s, fst_e = infer_bind env (name, args, expr) in
            

           (match scope with
            | None -> R.return (expr_s, build_arrow args_t expr_t)
            (* IN <scope> *)
            | Some scope ->
              let* scope_s, scope_t = helper env scope in
              R.return (scope_s, scope_t)))
      | Application (l_app, r_app) ->
        let* l_app_s, l_app_t = helper env l_app in
        let* r_app_s, r_app_t = helper (TypeEnv.apply l_app_s env) r_app in
        let* subs = Subst.compose l_app_s r_app_s in
        let* body_t = fresh_var in
        (* f (x: int) <=> int -> 'a *)
        let fun_type = TArrow (r_app_t, body_t) in
        let* u = Subst.unify fun_type (Subst.apply l_app_t subs) in
        let* subs = Subst.compose subs u in
        R.return (subs, Subst.apply body_t subs)
      | Match (expr, cases) ->
        let* comp_s, comp_t = helper env expr in
        let* res_abs_t = fresh_var in
        R.fold
          cases
          ~init:(R.return (comp_s, res_abs_t))
          ~f:(fun (res_s, res_t) (p, e) ->
            let* env, p_t = infer_pattern (TypeEnv.apply res_s env) p in
            let* u1 = Subst.unify p_t comp_t in
            let* u1 = Subst.compose u1 res_s in
            let* e_s, e_t = helper (TypeEnv.apply u1 env) e in
            let* u2 = Subst.unify res_t e_t in
            let* subs = Subst.compose_all [ u1; u2; e_s; res_s ] in
            R.return (subs, Subst.apply res_t subs))
      | _ -> R.fail Unsupported_type
    in
    helper env expr
  ;;

  (* let infer_prog (env : TypeEnv.t) (prog : Ast.prog) : (Subst.t * inf_type) list R.t =
    let rec helper (acc : (Subst.t * inf_type) list) (env : TypeEnv.t) = function
      | expr :: tl ->
        let* subs, typ = infer_expr env expr in
        let env = TypeEnv.apply subs env in
        helper ([ subs, typ ] @ acc) env tl
      | [] -> R.return acc
    in
    match prog with
    | [] -> R.fail Empty_program
    | hd :: tl ->
      let* subs, typ = infer_expr env hd in
      let env = TypeEnv.apply subs env in
      let* h_list = helper [ subs, typ ] env tl in
      R.return (List.rev h_list)
  ;;
  *)
end
