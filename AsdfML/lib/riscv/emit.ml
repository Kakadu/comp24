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
  let stack_loc = Offset (fp, !stack_pos) in
  emit sd reg stack_loc ~comm;
  stack_pos := !stack_pos - 8;
  stack_loc
;;

let emit_fn_decl name (args : Ast.id list) stack_size =
  if List.length args > 8
  then failwith "TODO: stack arguments"
  else (
    emit str (Format.sprintf {|
    .globl %s
    .type %s, @function|} name name);
    emit label name;
    if not (List.is_empty args)
    then emit comment ("args: " ^ (args |> String.concat ~sep:", "));
    emit addi sp sp (-stack_size);
    emit sd ra (Offset (sp, stack_size));
    emit sd fp (Offset (sp, stack_size - 8));
    emit addi fp sp (stack_size - 16) ~comm:"Prologue ends";
    stack_pos := 0)
;;

let dump_reg_args_to_stack args =
  assert (List.length args <= 8);
  List.take arg_regs (List.length args)
  |> List.zip_exn args
  |> List.filter ~f:(fun (arg, _) -> String.( <> ) "_" arg)
  |> List.fold ~init:[] ~f:(fun acc (arg, reg) ->
    let loc = emit_store reg ~comm:arg in
    (arg, loc) :: acc)
;;

let emit_fn_ret stack_size =
  emit ld fp (Offset (sp, stack_size - 8)) ~comm:"Epilogue starts";
  emit ld ra (Offset (sp, stack_size));
  emit addi sp sp stack_size;
  emit ret
;;

let emit_load ?(comm = "") dst = function
  | AsmInt n -> emit li dst n ~comm
  | AsmFn n -> emit la dst n ~comm
  | AsmReg r ->
    (match r with
     | SP -> emit mv dst sp ~comm
     | Reg _ | Temp _ -> if not (equal_reg r dst) then emit mv dst r ~comm
     | Offset _ -> emit ld dst r ~comm)
;;

(* TODO *)
let emit_load_2 ?(comm = "") = function
  | AsmInt _ | AsmFn _ -> failwith "emit_load: invalid dst"
  | AsmReg dst ->
    (match dst with
     | SP -> failwith "emit_load: invalid dst"
     | Reg _ | Temp _ ->
       (function
         | AsmInt n -> emit li dst n ~comm
         | AsmFn n -> emit la dst n ~comm
         | AsmReg src ->
           (match src with
            | SP -> emit mv dst sp ~comm
            | Reg _ | Temp _ -> if not (equal_reg src dst) then emit mv dst src ~comm
            | Offset _ -> emit ld dst src ~comm))
     | Offset (r1, o1) ->
       let load src = emit sd src dst ~comm in
       (function
         | AsmInt n ->
           emit li t0 n ~comm;
           load t0
         | AsmFn n ->
           emit la t0 n ~comm;
           load t0
         | AsmReg src ->
           (match src with
            | SP -> load sp
            | Reg _ | Temp _ -> if not (equal_reg src dst) then load src
            | Offset (r2, o2) -> if not (equal_reg r1 r2 && equal_int o1 o2) then load src)))
;;

let emit_fn_call name (args : asm_value list) =
  if List.length args > 8
  then failwith "TODO: stack arguments"
  else (
    (* Utils.dbg "emit_fn_call %s(%a)\n" name (Format.pp_print_list pp_asm_value) args; *)
    List.zip_exn (List.take arg_regs (List.length args)) args
    |> List.iter ~f:(fun (reg, arg) -> emit_load reg arg);
    emit call name;
    a0 (* emit_store a0 *))
;;

let direct_unops = [ "not"; "[ - ]" ]
let direct_binops = [ "( + )"; "( - )"; "( * )"; "( / )"; "( && )"; "( || )" ]
let is_direct_unop = List.mem direct_unops ~equal:String.equal
let is_direct_binop = List.mem direct_binops ~equal:String.equal

let emit_direct_unop ?(comm = "") dest op a =
  match op with
  | "not" -> emit xori dest a (-1) ~comm
  | "[ - ]" -> emit sub dest zero a ~comm
  | _ -> failwith "emit_direct_unop: invalid op"
;;

let emit_direct_binop ?(comm = "") dest op a0 a1 =
  (* TODO: addi case *)
  let op =
    match op with
    | "( + )" -> add
    | "( - )" -> sub
    | "( * )" -> mul
    | "( / )" -> div
    | "( && )" -> and_
    | "( || )" -> or_
    | _ -> failwith "emit_direct_math: invalid op"
  in
  emit op dest a0 a1 ~comm
;;
