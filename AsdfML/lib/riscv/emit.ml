open Base
open Machine

let stack_pos = ref 0
let code : (instr * string) Queue.t = Queue.create ()
let emit ?(comm = "") instr = instr (fun i -> Queue.enqueue code (i, comm))

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
    emit addi sp sp (-stack_size);
    emit sd ra (Offset (sp, stack_size));
    emit sd fp (Offset (sp, stack_size - 8));
    emit addi fp sp (stack_size - 16) ~comm:"Prologue ends";
    stack_pos := 0;
    List.take arg_regs (List.length args)
    |> List.zip_exn args
    |> List.fold ~init:[] ~f:(fun acc (arg, reg) ->
      let loc = emit_store reg ~comm:arg in
      (arg, loc) :: acc))
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
