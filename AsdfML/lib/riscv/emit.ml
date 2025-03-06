(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base
open Machine

let stack_pos = ref 0
let code : (instr * string) Queue.t = Queue.create ()
let fn_code : (instr * string) Queue.t = Queue.create ()
let cur_code = ref code
let emit ?(comm = "") instr = instr (fun i -> Queue.enqueue !cur_code (i, comm))
let set_code () = cur_code := code
let set_fn_code () = cur_code := fn_code

let flush_fn () =
  Queue.drain
    fn_code
    ~f:(fun (i, comm) -> Queue.enqueue code (i, comm))
    ~while_:(fun _ -> true)
;;

let empty_fn () = Queue.clear fn_code

let emit_store ?(comm = "") reg =
  let stack_loc = fp, !stack_pos in
  emit sd reg stack_loc ~comm;
  stack_pos := !stack_pos - word;
  LMem stack_loc
;;

let emit_fn_decl name (args : Ast.id list) stack_size =
  (* if List.length args > 8
     then failwith "TODO: stack arguments"
     else *)
  emit str (Format.sprintf {|
    .globl %s
    .type %s, @function|} name name);
  emit label name;
  if not (List.is_empty args)
  then emit comment ("args: " ^ (args |> String.concat ~sep:", "));
  emit addi sp sp (-stack_size);
  emit sd ra (sp, stack_size);
  emit sd fp (sp, stack_size - word);
  emit addi fp sp (stack_size - 16) ~comm:"Prologue ends";
  stack_pos := 0
;;

let emit_fn_ret stack_size =
  emit ld fp (sp, stack_size - word) ~comm:"Epilogue starts";
  emit ld ra (sp, stack_size);
  emit addi sp sp stack_size;
  emit ret
;;

let emit_load ?(comm = "") (dst : loc) (src : rvalue) =
  let load_src dst_reg = function
    | RInt n -> emit li dst_reg n ~comm
    | RFn id -> emit la dst_reg id ~comm
    | RReg src_reg ->
      (match src_reg with
       | SP -> emit mv dst_reg sp ~comm
       | Reg _ | Temp _ ->
         if not (equal_reg src_reg dst_reg)
         then
           emit mv dst_reg src_reg ~comm
           (* | Offset (src_reg, off) -> emit ld dst_reg (src_reg, off) ~comm *))
    | ROffset (src, off) -> emit ld dst_reg (src, off) ~comm
  in
  let load_dst dst src =
    match dst, src with
    | LReg dst_reg, _ -> load_src dst_reg src
    | LMem dst_off, RReg ((Reg _ | Temp _) as src) -> emit sd src dst_off ~comm
    | LMem dst_off, _ ->
      load_src t1 src;
      emit ld t1 dst_off ~comm
  in
  load_dst dst src
;;

let emit_load_reg ?(comm = "") reg src = emit_load (LReg reg) src ~comm

let emit_fn_call name (args : rvalue list) =
  if List.length args > n_reg_args
  then failwith "TODO: stack arguments"
  else (
    List.zip_exn (List.take arg_regs (List.length args)) args
    |> List.iter ~f:(fun (reg, arg) -> emit_load (LReg reg) arg);
    emit call name;
    a0)
;;

let dump_reg_args_to_stack args =
  let regs, stack = List.split_n args (List.length arg_regs) in
  match stack with
  | [] ->
    List.take arg_regs (List.length regs)
    |> List.zip_exn args
    |> List.fold ~init:[] ~f:(fun acc (arg, reg) ->
      let loc = emit_store reg ~comm:arg in
      (arg, loc) :: acc)
  | _ ->
    let tuple = List.last_exn arg_regs in
    let arg_regs = List.drop_last_exn arg_regs in
    let dumped =
      List.take args (List.length arg_regs)
      |> List.zip_exn arg_regs
      |> List.fold ~init:[] ~f:(fun acc (reg, arg) ->
        let loc = emit_store reg ~comm:arg in
        (arg, loc) :: acc)
    in
    let tuple_loc = emit_store tuple ~comm:"Tuple for stack arguments" in
    let heap_loc =
      List.split_n args (List.length arg_regs)
      |> snd
      |> List.foldi ~init:[] ~f:(fun idx acc arg ->
        let loc =
          emit_store
            (emit_fn_call "ml_get_tuple_field" [ loc_to_rvalue tuple_loc; RInt idx ])
        in
        (* TODO: don't unpack tuple args *)
        (arg, loc) :: acc)
    in
    dumped @ heap_loc
;;

let direct_unops = [ "not"; "[ - ]" ]

let direct_binops =
  [ "( + )"
  ; "( - )"
  ; "( * )"
  ; "( / )"
  ; "( && )"
  ; "( || )"
  ; "( > )"
  ; "( < )"
  ; "( >= )"
  ; "( <= )"
  ; "( = )"
  ; "( <> )"
  ]
;;

let is_direct_unop = List.mem direct_unops ~equal:String.equal
let is_direct_binop = List.mem direct_binops ~equal:String.equal

let emit_direct_unop ?(comm = "") dest op a =
  match op with
  | "not" -> emit xori dest a 1 ~comm
  | "[ - ]" -> emit sub dest zero a ~comm
  | _ -> failwith "emit_direct_unop: invalid op"
;;

let emit_direct_binop ?(comm = "") dest op a0 a1 =
  (* TODO: addi case *)
  match op with
  | "( + )" -> emit add dest a0 a1 ~comm
  | "( - )" -> emit sub dest a0 a1 ~comm
  | "( * )" -> emit mul dest a0 a1 ~comm
  | "( / )" -> emit div dest a0 a1 ~comm
  | "( && )" -> emit and_ dest a0 a1 ~comm
  | "( || )" -> emit or_ dest a0 a1 ~comm
  | "( < )" -> emit slt dest a0 a1 ~comm
  | "( > )" -> emit slt dest a1 a0 ~comm
  | "( >= )" ->
    emit slt dest a0 a1;
    emit xori dest dest 1
  | "( <= )" ->
    emit slt dest a1 a0;
    emit xori dest dest 1
  | "( = )" ->
    emit xor dest a0 a1;
    emit seqz dest dest
  | "( <> )" ->
    emit xor dest a0 a1;
    emit snez dest dest
  | _ -> failwith "emit_direct_math: invalid op"
;;
