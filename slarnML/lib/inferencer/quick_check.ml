open Ast

type ty =
  | TVar of tv ref
  | TInt
  | TBool
  | TUnit
  | TFun of ty * ty

and tv =
  | Unbound of int * int
  | Link of ty

type context = (string * ty) list

let fresh_id =
  let counter = ref 0 in
  fun () ->
    incr counter;
    !counter
;;

let fresh_var level = TVar (ref (Unbound (fresh_id (), level)))

let rec deref = function
  | TVar { contents = Link t } -> deref t
  | t -> t
;;

let rec string_of_ty t =
  let t = deref t in
  match t with
  | TVar { contents = Unbound (id, _) } -> "'" ^ string_of_int id
  | TVar { contents = Link t } -> string_of_ty t
  | TInt -> "int"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TFun (a, r) -> Printf.sprintf "(%s -> %s)" (string_of_ty a) (string_of_ty r)
;;

let rec unify t1 t2 =
  let t1 = deref t1 in
  let t2 = deref t2 in
  match t1, t2 with
  | ( TVar ({ contents = Unbound (_, l1) } as v1)
    , TVar ({ contents = Unbound (_, l2) } as v2) ) ->
    if v1 == v2 then () else if l1 < l2 then v2 := Link t1 else v1 := Link t2
  | TVar ({ contents = Unbound _ } as v), t | t, TVar ({ contents = Unbound _ } as v) ->
    if occurs_check v t then failwith "Occurs check failed" else v := Link t
  | TInt, TInt | TBool, TBool | TUnit, TUnit -> ()
  | TFun (a, b), TFun (c, d) ->
    unify a c;
    unify b d
  | _ ->
    failwith
      (Printf.sprintf "Type mismatch: %s vs %s" (string_of_ty t1) (string_of_ty t2))

and occurs_check v t =
  let rec check = function
    | TVar v' when v == v' -> true
    | TFun (a, r) -> check a || check r
    | _ -> false
  in
  check (deref t)
;;

let const_check = function
  | CInt _ -> TInt
  | CBool _ -> TBool
  | CUnit -> TUnit
;;

let type_check init_ctx expr =
  let get_decl_name = function
    | Decl (name, _) | DeclRec (name, _) -> name
  in
  let get_decl_args = function
    | Decl (_, args) | DeclRec (_, args) -> args
  in
  let new_ctx ctx t = function
    | Decl _ -> ctx
    | DeclRec (x, _) -> (x, t) :: ctx
  in
  (* let next_level_f d = if (List.length (get_decl_args d))=0 then (fun x->x) else (fun x->x+0) in *)
  let last_arg args =
    match List.rev args with
    | [] -> failwith (Printf.sprintf "Args can't be empty")
    | arg :: tail -> arg, List.rev tail
  in
  let rec check ?(get_level = fun x -> x) ctx level expr =
    match expr with
    | Const c -> const_check c, ctx
    | Id x ->
      let t = List.assoc_opt x ctx in
      (match t with
       | None -> failwith (Printf.sprintf "Variable '%s' not found in context" x)
       | Some t -> instantiate (get_level level) t, ctx)
    | Fun ([], body) ->
      let t, _ = check ctx level body in
      t, ctx
    | Fun (arg :: tail, body) ->
      let arg_t = fresh_var level in
      let body_t, ctx = check ((arg, arg_t) :: ctx) level (Fun (tail, body)) in
      TFun (arg_t, body_t), ctx
    | App (f, []) -> check ctx level f
    | App (f, args) ->
      let arg, tail = last_arg args in
      let f_t, _ = check ctx level (App (f, tail)) in
      let arg_t, _ = check ctx level arg in
      (* print_endline ((string_of_ty f_t)^" "^(string_of_ty arg_t)); *)
      let res_t = fresh_var level in
      unify f_t (TFun (arg_t, res_t));
      res_t, ctx
    | LetIn (d, e1, e2) ->
      let x = get_decl_name d in
      let exp_t = fresh_var (level + 1) in
      let e1_t, _ = check (new_ctx ctx exp_t d) (level + 1) (Fun (get_decl_args d, e1)) in
      unify exp_t e1_t;
      let generalized = generalize ctx (level + 1) e1_t in
      let ctx = (x, generalized) :: ctx in
      let t, _ = check ctx level e2 in
      t, ctx
    | Let (d, e) ->
      let x = get_decl_name d in
      let exp_t = fresh_var (level + 1) in
      let e_t, _ = check (new_ctx ctx exp_t d) (level + 1) (Fun (get_decl_args d, e)) in
      unify exp_t e_t;
      let generalized = generalize ctx (level + 1) e_t in
      generalized, (x, generalized) :: ctx
    | Not e -> check ctx level e
    | Or (e1, e2)
    | And (e1, e2)
    | Eq (e1, e2)
    | Lt (e1, e2)
    | Gt (e1, e2)
    | Gte (e1, e2)
    | Lte (e1, e2) ->
      let e1_t, _ = check ctx level e1 in
      let e2_t, _ = check ctx level e2 in
      unify e1_t e2_t;
      TBool, ctx
    | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Div (e1, e2) ->
      let e1_t, _ = check ctx level e1 in
      let e2_t, _ = check ctx level e2 in
      unify e1_t e2_t;
      e2_t, ctx
    | If (e1, e2, e3) ->
      let e1_t, _ = check ctx level e1 in
      let e2_t, _ = check ctx level e2 in
      let e3_t, _ = check ctx level e3 in
      unify e1_t TBool;
      unify e2_t e3_t;
      e2_t, ctx
  and generalize _ level t =
    let t = deref t in
    let vars = ref [] in
    let rec collect = function
      | TVar { contents = Unbound (id, l) } when l > level && not (List.memq id !vars) ->
        vars := id :: !vars
      | TFun (a, r) ->
        collect a;
        collect r
      | _ -> ()
    in
    collect t;
    List.fold_right (fun v acc -> TFun (TVar (ref (Unbound (v, level))), acc)) !vars t
  and instantiate level t =
    let t = deref t in
    match t with
    | TFun (a, r) ->
      TFun (instantiate level a, instantiate level r)
      (* | TVar({contents = Unbound(_, l)}) when l <= level ->  *)
      (* print_endline ((string_of_int level)^" "^(string_of_int level)); *)
      (* fresh_var level *)
    | t -> t
  in
  let t, ctx = check init_ctx 0 expr in
  let rec finalize = function
    | TVar { contents = Link t } -> finalize t
    | TVar c -> TVar c
    (* | TVar({contents = Unbound(_, _)}) -> TUnit *)
    | TFun (a, r) -> TFun (finalize a, finalize r)
    | t -> t
  in
  finalize t, ctx
;;

open Res

let type_check_res ctx ast =
  try
    let t, ctx = type_check ctx ast in
    Result ((ast, t), ctx)
  with
  | Failure msg -> Error msg
;;

let quick_check ast_lst =
  List.fold_left
    (fun res ast ->
      res
      >>= fun (types, ctx) ->
      type_check_res ctx ast >>= fun (t, ctx) -> Result (types @ [ t ], ctx))
    (Result ([], []))
    ast_lst
  >>= fun (lst, _) -> Result lst
;;

(* let () =
   let id = Fun(["x"], Id"x") in
   let t = type_check id in
   Printf.printf "Identity function type: %s\n" (string_of_ty t);

   let app1 = App(id, [Const (CInt 5)]) in
   let t = type_check app1 in
   Printf.printf "Applied to int: %s\n" (string_of_ty t);

   let app_err = App(App(id, [Const (CInt 5)]), [Const (CBool true)]) in
   try
   let _ = type_check app_err in
   print_endline "No error (WRONG!)"
   with Failure msg ->
   Printf.printf "Error caught: %s\n" msg
   ;; *)
(* let () =
   let ast =
   [ Let
        ( Decl ("fac", [ "n" ])
        , LetIn
            ( DeclRec ("help", [ "acc"; "n" ])
            , If
                ( Lte (Id "n", Const (CInt 1))
                , Id "acc"
                , App (Id "help", [ Sub (Id "n", Const (CInt 1)); Mul (Id "n", Id "acc") ])
                )
            , App (Id "help", [ Const (CInt 1); Id "n" ]) ) )
    ; Let
        ( Decl ("fac2", [ "n" ])
        , LetIn
            ( DeclRec ("help2", [ "n"; "f" ])
            , If
                ( Lte (Id "n", Const (CInt 1))
                , App (Id "f", [ Const (CInt 1) ])
                , App
                    ( Id "help2"
                    , [ Sub (Id "n", Const (CInt 1))
                      ; Fun ([ "x" ], Mul (App (Id "f", [ Id "n" ]), Id "x"))
                      ] ) )
            , App (Id "help2", [ Id "n"; Fun ([ "x" ], Id "x") ]) ) )
      (* Let(Decl("f",[]),LetIn(Decl("g", []),Fun(["x"],Id"x"), App(Id"g",[]))); *)
      (* Let(Decl("id",["x"]),(Fun(["y"],App(Id"y",[Id"x"])))); *)
      (* LetIn(Decl("id_i",[]),(Fun(["y"],Sub(Id"y",Const (CInt 1)))),
         LetIn(Decl("_",[]),
         App(Id"id_i", [Const (CBool true)]),
         App(Id"id_i",[Const (CInt 1)]))
         ); *)
      (* Let(Decl("id_b",["x"]),Id"x"); *)
      (* Let(Decl("f",["i"; "b"; "g"; "h"]),
         If(App(Id"h", [Id "b"]),
         (Id"i"),
         App(Id"g", [Id"i"])
         )); *)
      (* App(Id"id_i", [Const CUnit]); *)
      (* App(Id"f", [Const(CInt 1);Const(CBool true);Id"id_i";Id"id_b"]) *)
    ]
   in
   match quick_check ast with
   | Error e -> print_endline e
   | Result list ->
   print_endline
   (String.concat
   "\n"
   (List.map
   (fun (ast, t) -> Pprint_ast.pp_expr ast ^ "\n[" ^ string_of_ty t ^ "]\n")
   list))
   ;; *)

(* print_endline "\n\n\n" *)
