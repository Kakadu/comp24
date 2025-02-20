open Riscv
open Flambda
open Linear_scan_allocation

type block = instruction list
type fun_body = block list
type generated_code = (string, fun_body) Hashtbl.t

let word_size = 8
let arg_regs_count = 8
let curr_block = ref []
let curr_assignment = ref (Base.Map.empty (module Base.String))

let codegen_binop op x y =
   match x, y with
   | Fl_var x, Fl_var y ->
   let xloc = Base.Map.find_exn !curr_assignment x in
   let yloc = Base.Map.find_exn !curr_assignment y in
   let xreg, yreg, restore =
   match xloc, yloc with
   | Reg xr, Reg yr -> xr, yr, []
   | StackLoc x_idx, Reg yr ->
   temp 0, yr, []
   | Reg xr, StackLoc y_idx ->
   xr, temp 0, []
   | StackLoc x_idx, StackLoc y_idx ->
   temp 0, temp 1, [] in
   let op = match op with
   | Anf.Plus -> ADD
   | Anf.Mul -> MUL
   | Anf.Div -> DIV
   | Anf.Minus -> SUB
   | Anf.Lt -> SLT
   | Anf.Leq

module FunUtils = struct
  (** evaluates cells count so that stack is aligned by 128-bit
   according to RISC-V calling convention*)
  let stack_size_aligned args_count =
    let byte_size = args_count * word_size in
    byte_size + (16 - byte_size mod 16) 

  let used_registers assignment =
    assignment
    |> Base.Map.filter_map  ~f:(function
      | Reg r -> Some r
      | StackLoc _ -> None) |> Base.Map.to_alist |> List.map snd
  ;;

  let resolve_args param_names arity =
    if arity <= arg_regs_count
    then List.mapi (fun i name -> name, Reg (arg i)) param_names
    else (
      let reg_args, stack_args = Base.List.split_n param_names arg_regs_count in
      let reg_args = List.mapi (fun i name -> name, Reg (arg i)) reg_args in
      let stack_args = List.mapi (fun i name -> name, StackLoc i) stack_args in
      reg_args @ stack_args)
  ;;

  let prologue_and_apilogue assignment =
    (* todo save frame pointer *)
    let used_registers = used_registers assignment in
    let saved_registers =  List.filter is_saved used_registers in
    let stack_size = stack_size_aligned (List.length saved_registers) in
    let extend_stack_insn = extend_stack_insn stack_size in
    let prologue = List.mapi (fun i reg -> sw ~v:reg ((i + 1) * word_size) ~src:Sp) saved_registers in
    let prologue = extend_stack_insn::prologue in
    let restore_stack_insn = shrink_stack_insn stack_size in
 
    let epilogue = List.mapi (fun i reg -> lw ~rd:reg ((i + 1) * word_size) ~src:Sp) saved_registers in
    let epilogue = epilogue @ [restore_stack_insn] in
    prologue, epilogue

    module Call = struct
      let save_and_restore_insns callee_arity =
        let callee_args = List.init callee_arity arg in
        let to_preserve = Ra::callee_args in
        let extra_stack_size = stack_size_aligned (List.length to_preserve) in
        let extend_stack_insn = extend_stack_insn extra_stack_size in
        let save_insns = List.mapi (fun i reg -> sw ~v:reg ((i + 1) * word_size) ~src:Sp) to_preserve in
        let save_insns = extend_stack_insn::save_insns in
        let restore_stack_insn = shrink_stack_insn extra_stack_size in
        let restore_insns = List.mapi (fun i reg -> lw ~rd:reg ((i + 1) * word_size) ~src:Sp) to_preserve in
        let restore_insns = restore_insns @ [restore_stack_insn] in
        save_insns, restore_insns
    end
end

let codegen program = "1"
