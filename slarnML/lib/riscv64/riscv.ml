open Riscv_ast
open Anf_ast

(* open Pprint_riscv *)
open List
open Res
open Call_define

let split_at_n n lst =
  let rec helper m l r =
    if m = 0
    then rev l, r
    else (
      match r with
      | [] -> rev l, r
      | head :: tail -> helper (m - 1) (head :: l) tail)
  in
  helper n [] lst
;;

let int_of_bool b = if b then 1 else 0
let get_offset = map (fun (offset, _, _, _, _, _) -> Result offset)
let get_regs = map (fun (_, regs, _, _, _, _) -> Result regs)
let get_offsets = map (fun (_, _, offsets, _, _, _) -> Result offsets)
let get_free = map (fun (_, _, _, free, _, _) -> Result free)
let get_cond = map (fun (_, _, _, _, cond, _) -> Result cond)

let update_env f f1 f2 f3 =
  map (fun (offset, regs, offsets, free, conds, funs) ->
    Result (f offset, f1 regs, f2 offset offsets, f3 free, conds, funs))
;;

let update_cond f =
  map (fun (offset, regs, offsets, free, conds, funs) ->
    Result (offset, regs, offsets, free, f conds, funs))
;;

let f_id x = x

let get_fun name =
  map (fun (_, _, _, _, _, funs) ->
    (* print_endline ("get_fun " ^ name ^ " " ^ (String.concat ", " (List.map (fun (f_name, cnt) -> f_name^"("^string_of_int cnt^")") funs))); *)
    Result (List.find_opt (fun (f_name, _) -> f_name = name) funs))
;;

let get_funs = map (fun (_, _, _, _, _, funs) -> Result funs)

let update_funs f =
  map (fun (offset, regs, offsets, free, conds, funs) ->
    Result (offset, regs, offsets, free, conds, f funs))
;;

let add_fun name args_cnt =
  map (fun (offset, regs, offsets, free, conds, funs) ->
    Result (offset, regs, offsets, free, conds, (name, args_cnt) :: funs))
;;

let replace_fun name args_cnt =
  map (fun (offset, regs, offsets, free, conds, funs) ->
    Result
      ( offset
      , regs
      , offsets
      , free
      , conds
      , (name, args_cnt) :: List.filter (fun (f_name, _) -> f_name <> name) funs ))
;;

let rec count_max_call_offset offset a =
  let count_offset_cexpr = function
    | ANot _
    | AOr _
    | AAnd _
    | AEq _
    | AGt _
    | AGte _
    | ALt _
    | ALte _
    | AAdd _
    | ASub _
    | AMul _
    | ADiv _
    | CImmExpr _ -> 0
    | AIf (_, e1, e2) ->
      max offset (max (count_max_call_offset offset e1) (count_max_call_offset offset e2))
    | AApp (_, args) -> max 0 (length args - 8) * 8
  in
  match a with
  | ACExpr e -> count_offset_cexpr e
  | ALet (_, c, a) ->
    max offset (max (count_offset_cexpr c) (count_max_call_offset offset a))
;;

let rec count_offset_aexpr a =
  let count_offset_cexpr = function
    | ANot _
    | AOr _
    | AAnd _
    | AEq _
    | AGt _
    | AGte _
    | ALt _
    | ALte _
    | AAdd _
    | ASub _
    | AMul _
    | ADiv _
    | CImmExpr _ -> 0
    | AApp (_, args) -> max 3 (length args - 5) * 8
    | AIf (_, e1, e2) -> count_offset_aexpr e1 + count_offset_aexpr e2
  in
  match a with
  | ACExpr e -> count_offset_cexpr e
  | ALet (_, c, a) -> count_offset_cexpr c + count_offset_aexpr a + 8
;;

let save_id f id reg res =
  map
    (fun env ->
      res
      |> get_offsets
      >>= fun offsets ->
      match List.find_opt (fun (i, _) -> i = id) offsets with
      | Some _ -> Result ([], env)
      | None ->
        res
        |> get_offset
        >>= fun offset ->
        res
        |> update_env f f_id (fun off os -> (id, off) :: os) f_id
        >>= fun n_env -> Result ([ Sd (reg, ImmInt offset, S 0) ], n_env))
    res
;;

let free_some_reg f res =
  map
    (fun env ->
      get_free res
      >>= fun free ->
      match free with
      | [] ->
        get_regs res
        >>= fun regs ->
        (match regs with
         | [] -> Error "Regs not exists"
         | (rid_opt, reg) :: n_regs ->
           res
           |> get_offset
           >>= fun env_offset ->
           res
           |> get_offsets
           >>= fun offsets ->
           let instr, f_offset, f_offsets =
             match List.find_opt (fun (i, _) -> Some i = rid_opt) offsets, rid_opt with
             | None, Some rid ->
               [ Sd (reg, ImmInt (-env_offset), S 0) ], f, fun off os -> (rid, off) :: os
             | Some _, _ | None, None -> [], f_id, fun _ x -> x
           in
           res
           |> update_env f_offset (fun _ -> n_regs) f_offsets (fun fr -> reg :: fr)
           >>= fun n_env -> Result (instr, n_env))
      | _ -> Result ([], env))
    res
;;

let free_reg f reg =
  map (fun env ->
    Result env
    |> get_regs
    >>= fun regs ->
    match List.find_opt (fun (_, r) -> r = reg) regs with
    | None -> Result ([], env)
    | Some (Some id, reg) ->
      Result env
      |> save_id f id reg
      >>= fun (instr, env) ->
      Result env
      |> update_env
           f_id
           (List.filter (fun (_, r) -> r <> reg))
           (fun _ x -> x)
           (fun fr -> reg :: fr)
      >>= fun env -> Result (instr, env)
    | Some (None, reg) ->
      update_env
        f_id
        (List.filter (fun (_, r) -> r <> reg))
        (fun _ x -> x)
        (fun fr -> reg :: fr)
        (Result env)
      >>= fun env -> Result ([], env))
;;

let block_reg f opt reg res =
  res
  |> get_regs
  >>= fun regs ->
  match List.find_opt (fun (i, r) -> i = opt && r = reg) regs with
  | Some _ -> res >>= fun env -> Result ([], env)
  | None ->
    res
    |> free_reg f reg
    >>= fun (instr, env) ->
    Result env
    |> update_env
         f_id
         (fun rs -> rs @ [ opt, reg ])
         (fun _ x -> x)
         (List.filter (fun f -> f <> reg))
    >>= fun env -> Result (instr, env)
;;

let get_free_reg f res =
  res
  |> free_some_reg f
  >>= fun (instr1, env) ->
  Result env
  |> get_free
  >>= fun free ->
  match free with
  | [] -> Error "Free is empty"
  | reg :: _ ->
    res |> free_reg f reg >>= fun (b_instr, env) -> Result (instr1 @ b_instr, reg, env)
;;

let find_id f id reg_opt res =
  map
    (fun env ->
      get_regs res
      >>= fun regs ->
      let q_opt = find_opt (fun (i, _) -> i = Some id) regs in
      match q_opt with
      | Some (_, q) ->
        (match reg_opt with
         | None ->
           Result env
           |> block_reg f (Some id) q
           >>= fun (instr, env) -> Result (instr, q, env)
         | Some r ->
           res
           |> block_reg f (Some id) r
           >>= fun (instr, env) -> Result (instr @ [ Mv (r, q) ], r, env))
      | None ->
        get_offsets res
        >>= fun offsets ->
        (match List.find_opt (fun (i, _) -> i = id) offsets with
         | None -> Error (id ^ " not found")
         | Some (_, offset) ->
           (match reg_opt with
            | None -> free_some_reg f res
            | Some r -> free_reg f r res)
           >>= fun (p_instr, env) ->
           (match reg_opt with
            | Some r ->
              Result env
              |> block_reg f (Some id) r
              >>= fun (instr, env) ->
              Result (p_instr @ instr @ [ Ld (r, ImmInt offset, S 0) ], r, env)
            | None ->
              Result env
              |> get_free_reg f
              >>= fun (instr0, r, env) ->
              Result env
              |> block_reg f (Some id) r
              >>= fun (instr, env) ->
              Result (p_instr @ instr0 @ instr @ [ Ld (r, ImmInt offset, S 0) ], r, env))))
    res
;;

let free_regs f res =
  res
  |> get_regs
  >>= fun regs ->
  res
  >>= fun env ->
  List.fold_left
    (fun res (_, r) ->
      res
      >>= fun (instr, env) ->
      Result env |> free_reg f r >>= fun (n_instr, env) -> Result (instr @ n_instr, env))
    (Result ([], env))
    regs
;;

let init_args args res =
  let f x = x - 8 in
  let s_left_arg cnt a res =
    match a with
    | AId a ->
      res |> find_id f a (Some (A cnt)) >>= fun (instr, _, env) -> Result (instr, env)
    | AInt i ->
      res
      |> block_reg f None (A cnt)
      >>= fun (instr, env) -> Result (instr @ [ Li (A cnt, ImmInt i) ], env)
    | ABool b ->
      res
      |> block_reg f None (A cnt)
      >>= fun (instr, env) -> Result (instr @ [ Li (A cnt, ImmInt (int_of_bool b)) ], env)
    | AUnit -> res >>= fun env -> Result ([], env)
  in
  let s_right_arg offset a res =
    match a with
    | AId a ->
      res
      |> find_id f a (Some (T 6))
      >>= fun (instr, _, env) -> Result (instr @ [ Sd (T 6, ImmInt offset, Sp) ], env)
    | AInt i ->
      res
      |> free_reg f (T 6)
      >>= fun (instr, env) ->
      Result (instr @ [ Li (T 6, ImmInt i); Sd (T 6, ImmInt offset, Sp) ], env)
    | ABool b ->
      res
      |> free_reg f (T 6)
      >>= fun (instr, env) ->
      Result
        (instr @ [ Li (T 6, ImmInt (int_of_bool b)); Sd (T 6, ImmInt offset, Sp) ], env)
    | AUnit -> res |> free_reg f (T 6) >>= fun (instr, env) -> Result (instr, env)
  in
  let left, right = split_at_n 7 args in
  let r_len = length right in
  res
  >>= fun c_env ->
  res
  >>= (fun env ->
    fold_right
      (fun a r ->
        r
        >>= fun (offset, lst, e) ->
        Result e
        |> s_right_arg offset a
        >>= fun (instr, e) -> Result (offset - 8, instr @ lst, e))
      right
      (Result ((r_len - 1) * 8, [], env)))
  >>= fun (_, right, env) ->
  fold_left
    (fun r a ->
      r
      >>= fun (cnt, lst, e) ->
      Result e |> s_left_arg cnt a >>= fun (instr, e) -> Result (cnt + 1, instr @ lst, e))
    (Result (1, [], env))
    left
  >>= fun (_, left, _) -> Result (right @ left, c_env)
;;

let save_args offset args res =
  let f o = o + 8 in
  res
  |> get_offset
  >>= fun env_offset ->
  res
  |> update_env (fun _ -> offset) f_id (fun _ x -> x) f_id
  >>= fun env ->
  let left, right = split_at_n 8 args in
  let _, r_offsets =
    List.fold_left (fun (offset, lst) r -> offset + 8, (r, offset) :: lst) (0, []) right
  in
  fold_right
    (fun l r ->
      r
      >>= fun (cnt, lst, e) ->
      Result e
      |> block_reg f (Some l) (A cnt)
      >>= fun (p_instr, e) ->
      Result e
      |> save_id f l (A cnt)
      >>= fun (instr, e) -> Result (cnt - 1, lst @ p_instr @ instr, e))
    left
    (Result (min (length args - 1) 7, [], env))
  >>= fun (_, instr, env) ->
  Result env
  |> update_env (fun _ -> env_offset) f_id (fun _ x -> r_offsets @ x) f_id
  >>= fun env -> Result (instr, env)
;;

let load_imm f a res =
  let load_const n_instr =
    res
    |> free_some_reg f
    >>= fun (instr, env) ->
    Result env
    |> get_free_reg f
    >>= fun (g_instr, reg, env) ->
    Result env
    |> block_reg f None reg
    >>= fun (b_instr, env) -> Result (instr @ g_instr @ b_instr @ n_instr reg, reg, env)
  in
  match a with
  | AId a -> res |> find_id f a None
  | AInt i -> load_const (fun reg -> [ Li (reg, ImmInt i) ])
  | ABool b -> load_const (fun reg -> [ Li (reg, ImmInt (int_of_bool b)) ])
  | AUnit -> load_const (fun _ -> [])
;;

let filter_tag tag =
  if tag = "main"
  then tag ^ "2"
  else String.map (fun c -> if c = '#' || c = '$' then '_' else c) tag
;;

let get_unique_tag id env =
  let rec find_unique base counter =
    let candidate = if counter = 0 then base else base ^ "_" ^ string_of_int counter in
    if List.exists (fun (f_name, _) -> f_name = candidate) env
    then find_unique base (counter + 1)
    else candidate
  in
  if id = "main" then "main" else find_unique id 0
;;

let rec build_aexpr tag a res =
  let f o = o - 8 in
  let get_tag id = ".tag_" ^ filter_tag id in
  let get_tag_addr id = Id (get_tag (filter_tag id)) in
  let get_true_tag id = ".tag_" ^ filter_tag id ^ "_t" in
  let get_true_tag_addr id = Id (get_true_tag (filter_tag id)) in
  let build_cexpr tag c res =
    let bin_op op i1 i2 =
      res
      |> load_imm f i1
      >>= fun (instr1, reg1, env) ->
      Result env
      |> load_imm f i2
      >>= fun (instr2, reg2, env) ->
      Result env
      |> free_some_reg f
      >>= fun (instrf, env) ->
      Result env
      |> get_free_reg f
      >>= fun (instrg, regd, env) ->
      Result (instr1 @ instr2 @ instrf @ instrg @ [ op regd reg1 reg2 ], Some regd, env)
    in
    let cond_op op i1 i2 =
      res
      |> load_imm f i1
      >>= fun (instr1, reg1, env) ->
      Result env
      |> load_imm f i2
      >>= fun (instr2, reg2, env) ->
      Result env
      |> update_cond (fun c -> (tag, op reg1 reg2) :: c)
      >>= fun env -> Result (instr1 @ instr2, None, env)
    in
    match c with
    | AOr (i1, i2) -> bin_op (fun rd r1 r2 -> Math (I Or, rd, r1, r2)) i1 i2
    | AAnd (i1, i2) -> bin_op (fun rd r1 r2 -> Math (I And, rd, r1, r2)) i1 i2
    | AAdd (i1, i2) -> bin_op (fun rd r1 r2 -> Math (I Add, rd, r1, r2)) i1 i2
    | ASub (i1, i2) -> bin_op (fun rd r1 r2 -> Math (Sub, rd, r1, r2)) i1 i2
    | AMul (i1, i2) -> bin_op (fun rd r1 r2 -> Math (Mul, rd, r1, r2)) i1 i2
    | ADiv (i1, i2) -> bin_op (fun rd r1 r2 -> Math (Div, rd, r1, r2)) i1 i2
    | AEq (i1, i2) -> cond_op (fun r1 r2 -> Bnch (Beq, r1, r2, tag)) i1 i2
    | AGte (i1, i2) -> cond_op (fun r1 r2 -> Bnch (Bge, r1, r2, tag)) i1 i2
    | ALte (i1, i2) -> cond_op (fun r1 r2 -> Bnch (Ble, r1, r2, tag)) i1 i2
    | AGt (i1, i2) -> cond_op (fun r1 r2 -> Bnch (Bgt, r1, r2, tag)) i1 i2
    | ALt (i1, i2) -> cond_op (fun r1 r2 -> Bnch (Blt, r1, r2, tag)) i1 i2
    | ANot i ->
      res
      |> load_imm f i
      >>= fun (instr, reg, env) ->
      Result (instr @ [ Mathi (Xor, reg, reg, ImmInt (-1)) ], Some reg, env)
    | CImmExpr i ->
      res |> load_imm f i >>= fun (instr, reg, env) -> Result (instr, Some reg, env)
    | AApp (i, args) ->
      (match i with
       | AId id ->
         res
         |> get_fun id
         >>= fun c_opt ->
         let load_ptr c_opt r =
           match c_opt with
           | None ->
             r
             |> find_id f id (Some (A 0))
             >>= fun (instr_fun, _, env) -> Result (instr_fun, 0, env)
           | Some (_, c) ->
             r
             |> block_reg f None (A 0)
             >>= fun (instr_fun, env) ->
             Result
               ( instr_fun
                 @ [ Lui (A 0, ConstAddr ("hi", filter_tag id))
                   ; Mathi (Add, A 0, A 0, ConstAddr ("lo", filter_tag id))
                   ]
               , c
               , env )
         in
         res
         |> free_regs f
         >>= fun (instr_free, env) ->
         Result env
         |> load_ptr c_opt
         >>= fun (instr_fun, c, env) ->
         Result env
         |> init_args ([ AInt c; AInt (List.length args) ] @ args)
         >>= fun (instr_args, env) ->
         Result
           ( instr_free @ instr_fun @ instr_args @ [ Call (Id "part_app") ]
           , Some (A 0)
           , env )
       | _ -> Error "Call some shit")
    | AIf (ec, e1, e2) ->
      let free_a0 = free_reg f (A 0) in
      let id = "if_bnch" in
      let dflt_bnch res =
        res
        >>= (fun (instr0, reg, env) ->
          Result ([], None, env)
          |> build_aexpr tag e1
          >>= fun (instr1, reg1, env) ->
          (match reg1 with
           | Some reg when reg <> A 0 ->
             Result env
             |> free_a0
             >>= fun (instr, env) -> Result (instr @ [ Mv (A 0, reg) ], env)
           | Some _ -> Result ([], env)
           | _ -> Error "Error in if")
          >>= fun (instr2, env) ->
          Result
            ( instr0
              @ (Beqz (reg, get_tag_addr id) :: instr1)
              @ instr2
              @ [ Jmp (get_true_tag_addr id); Tag (get_tag id) ]
            , Some (A 0)
            , env ))
        |> build_aexpr tag e2
        >>= fun (instr1, reg2, env) ->
        (match reg2 with
         | Some reg when reg <> A 0 ->
           Result env
           |> free_a0
           >>= fun (instr, env) -> Result (instr @ [ Mv (A 0, reg) ], env)
         | Some _ -> Result ([], env)
         | _ -> Error "Error in if")
        >>= fun (instr2, env) ->
        Result (instr1 @ instr2 @ [ Tag (get_true_tag id) ], Some (A 0), env)
      in
      (match ec with
       | AId id ->
         res
         |> get_cond
         >>= fun conds ->
         (match List.find_opt (fun (t, _) -> t = get_tag_addr id) conds with
          | Some (_, cond) ->
            res
            >>= (fun env ->
              Result ([], None, env)
              |> build_aexpr tag e2
              >>= fun (instr1, reg1, env) ->
              (match reg1 with
               | Some reg when reg <> A 0 ->
                 Result env
                 |> free_a0
                 >>= fun (instr, env) -> Result (instr @ [ Mv (A 0, reg) ], env)
               | Some _ -> Result ([], env)
               | _ -> Error "Error in if")
              >>= fun (instr2, env) ->
              Result
                ( (cond :: instr1)
                  @ instr2
                  @ [ Jmp (get_true_tag_addr id); Tag (get_tag id) ]
                , Some (A 0)
                , env ))
            |> build_aexpr tag e1
            >>= fun (instr1, reg2, env) ->
            (match reg2 with
             | Some reg when reg <> A 0 ->
               Result env
               |> free_a0
               >>= fun (instr, env) -> Result (instr @ [ Mv (A 0, reg) ], env)
             | Some _ -> Result ([], env)
             | _ -> Error "Error in if")
            >>= fun (instr2, env) ->
            Result (instr1 @ instr2 @ [ Tag (get_true_tag id) ], Some (A 0), env)
          | None -> res |> load_imm f ec |> dflt_bnch)
       | _ -> res |> load_imm f ec |> dflt_bnch)
  in
  res
  >>= fun (instr0, _, env) ->
  match a with
  | ACExpr c ->
    Result env
    |> build_cexpr tag c
    >>= fun (instr1, reg, env) -> Result (instr0 @ instr1, reg, env)
  | ALet (id, c, a) ->
    Result env
    |> build_cexpr (get_tag_addr id) c
    >>= fun (instr1, reg_opt, env) ->
    (match reg_opt with
     | None -> Result (instr0 @ instr1, None, env) |> build_aexpr tag a
     | Some reg ->
       Result env
       |> block_reg f (Some id) reg
       >>= fun (instr2, env) ->
       Result (instr0 @ instr1 @ instr2, Some reg, env) |> build_aexpr tag a)
;;

let init_fun anf res =
  match anf with
  | AFun (id, args, e) ->
    let offset_call = 0 in
    let offset_args = min 8 (length args) * 8 in
    let offset_expr = count_offset_aexpr e in
    let offset_reserved = 2 * 8 in
    let offset_full = offset_call + offset_args + offset_expr + offset_reserved in
    let offset_align = 16 * ((offset_full + 15) / 16) in
    res
    |> get_funs
    >>= fun funs ->
    let unique_id = get_unique_tag id funs in
    let args_cnt = List.length args in
    let res = res |> add_fun unique_id args_cnt in
    res
    |> save_args (-offset_align) args
    >>= fun (s_argsi, env) ->
    Result ([], None, env)
    |> build_aexpr (Id "") e
    >>= fun (e_instr, reg, _) ->
    (match reg with
     | None -> Error "Void?"
     | Some reg ->
       Result
         ( (Tag (filter_tag unique_id)
            :: [ Mathi (Add, Sp, Sp, ImmInt (-offset_align))
               ; Sd (Ra, ImmInt (offset_full - 8), Sp)
               ; Sd (S 0, ImmInt (offset_full - 16), Sp)
               ; Mathi (Add, S 0, Sp, ImmInt offset_align)
               ])
           @ s_argsi
           @ e_instr
           @ [ Mv (A 0, reg)
             ; Ld (Ra, ImmInt (offset_full - 8), Sp)
             ; Ld (S 0, ImmInt (offset_full - 16), Sp)
             ; Mathi (Add, Sp, Sp, ImmInt offset_align)
             ; Ret
             ]
         , [ unique_id, args_cnt ] ))
;;

let head =
  [ Attribute "unaligned_access, 0"
  ; Attribute "stack_align, 16"
  ; Global "main"
  ; Tag "main"
  ; Mathi (Add, Sp, Sp, ImmInt (-32))
  ; Sd (Ra, ImmInt 16, Sp)
  ; Sd (S 0, ImmInt 8, Sp)
  ; Sd (S 1, ImmInt 0, Sp)
  ; Mathi (Add, S 0, Sp, ImmInt 32)
  ; Call (Id "init_part_apps")
  ; Call (Id "main2")
  ; Sd (A 0, ImmInt 24, Sp)
  ; Call (Id "cleanup_part_apps")
  ; Ld (A 0, ImmInt 24, Sp)
  ; Ld (Ra, ImmInt 16, Sp)
  ; Ld (S 0, ImmInt 8, Sp)
  ; Ld (S 1, ImmInt 0, Sp)
  ; Mathi (Add, Sp, Sp, ImmInt 32)
  ; Li (A 7, ImmInt exit)
  ; Ecall
  ]
;;

let default_res =
  Result
    ( -32
    , []
    , []
    , [ T 0; T 1; T 2; T 3; T 4; T 5; T 6; A 0; A 1; A 2; A 3; A 4; A 5; A 6; A 7 ]
    , []
    , default_func )
;;

let asm anf_lst =
  List.fold_left
    (fun r anf ->
      r
      >>= fun (prog, p_f) ->
      default_res
      |> update_funs (fun d -> d @ p_f)
      |> init_fun anf
      >>= fun (instr, f) -> Result (prog @ instr, p_f @ f))
    (Result (head, []))
    anf_lst
  >>= fun (prog, _) -> Result prog
;;
