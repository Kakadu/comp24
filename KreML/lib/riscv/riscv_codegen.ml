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

module Arith = struct
  let const_to_int = function
    | Ast.Const_int i -> i
    | Ast.Const_bool true -> 1
    | Ast.Const_bool false -> 0
    | Ast.Const_unit -> 1
    | Ast.Const_nil -> 0
  ;;

  let imm_is_12_bytes x = -2048 <= x && x <= 2047

  let build_complex_instr save_insns insns restore_insns =
    save_insns @ insns @ restore_insns
  ;;

  let merge_with_save_and_restore save insns restore =
    match save, restore with
    | Some s, Some r -> s @ insns @ r
    | None, None -> insns
    | _ -> Utils.unreachable ()
  ;;

  (* handle case when src is on the stack*)
  let codegen_binop op x y rd =
    let find_reg forbidden =
      List.find (fun r -> List.for_all (( <> ) r) forbidden) available_regs
    in
    match x, y with
    | Fl_var x, Fl_var y ->
      let xloc = Base.Map.find_exn !curr_assignment x in
      let yloc = Base.Map.find_exn !curr_assignment y in
      let xreg, yreg, save, restore =
        match xloc, yloc with
        | Reg xr, Reg yr -> xr, yr, None, None
        | StackLoc x_idx, Reg yr ->
          let xreg = find_reg [ yr; rd ] in
          let save = sw ~v:xreg 0 ~src:Sp in
          let load = lw ~rd:xreg (x_idx * word_size) ~src:fp in
          let restore = lw ~rd:xreg 0 ~src:Sp in
          xreg, yr, Some [ save; load ], Some [ restore ]
        | Reg xr, StackLoc y_idx ->
          let yreg = find_reg [ xr; rd ] in
          let save = sw ~v:yreg 0 ~src:Sp in
          let load = lw ~rd:yreg (y_idx * word_size) ~src:fp in
          let restore = lw ~rd:yreg 0 ~src:Sp in
          xr, yreg, Some [ save; load ], Some [ restore ]
        | StackLoc x_idx, StackLoc y_idx ->
          let xreg = find_reg [ rd ] in
          let yreg = find_reg [ rd; xreg ] in
          let save0 = sw ~v:xreg 0 ~src:Sp in
          let save1 = sw ~v:yreg word_size ~src:Sp in
          let load0 = lw ~rd:xreg (word_size * x_idx) ~src:fp in
          let load1 = lw ~rd:yreg (word_size * y_idx) ~src:fp in
          let restore0 = lw ~rd:xreg 0 ~src:Sp in
          let restore1 = lw ~rd:yreg word_size ~src:Sp in
          xreg, yreg, Some [ save0; save1; load0; load1 ], Some [ restore0; restore1 ]
      in
      let insn =
        match op with
        | Anf.Add -> Rtype (rd, xreg, yreg, ADD)
        | Anf.Sub -> Rtype (rd, xreg, yreg, SUB)
        | Anf.Mul -> Rtype (rd, xreg, yreg, MUL)
        | Anf.Div -> Rtype (rd, xreg, yreg, DIV)
        | Anf.Lt -> Rtype (rd, xreg, yreg, SLT)
        | Anf.Gt -> Rtype (rd, yreg, xreg, SLT)
        | Anf.Eq -> Rtype (rd, xreg, yreg, SUB)
        | Anf.Neq -> Rtype (rd, xreg, yreg, SUB)
        | Anf.And -> Rtype (rd, xreg, yreg, AND)
        | Anf.Or -> Rtype (rd, xreg, yreg, OR)
      in
      merge_with_save_and_restore save [ insn ] restore
    | Fl_const x, Fl_var y ->
      let imm = const_to_int x in
      let yloc = Base.Map.find_exn !curr_assignment y in
      let yreg, save, restore =
        match yloc with
        | Reg r -> r, None, None
        | StackLoc idx ->
          let yreg = find_reg [ rd ] in
          let save = sw ~v:yreg 0 ~src:Sp in
          let load = lw ~rd:yreg (word_size * idx) ~src:fp in
          let restore = lw ~rd:yreg 0 ~src:Sp in
          yreg, Some [ save; load ], Some [ restore ]
      in
      if imm_is_12_bytes imm
      then (
        let insns =
          match op with
          | Anf.Add -> [ Itype (rd, yreg, imm, ADDI) ]
          | Anf.Sub ->
            let negate = Pseudo.neg yreg in
            [ negate; Itype (rd, yreg, -imm, ADDI); negate ]
          | Anf.Mul | Anf.Div ->
            let xreg = find_reg [ yreg; rd ] in
            let save = sw ~v:xreg 0 ~src:Sp in
            let load = Pseudo.li xreg imm in
            let insn =
              match op with
              | Anf.Mul -> Rtype (rd, xreg, yreg, MUL)
              | Anf.Div -> Rtype (rd, xreg, yreg, DIV)
              | _ -> Utils.unreachable ()
            in
            let restore = lw ~rd:xreg 0 ~src:Sp in
            [ save; load; insn; restore ]
          | Anf.Eq -> [ Itype (rd, yreg, -imm, ADDI); Pseudo.seqz ~rd ~src:rd ]
          | Anf.Neq -> [ Itype (rd, yreg, -imm, ADDI) ]
          | Anf.Gt -> [ Itype (rd, yreg, imm, SLTI) ]
          (* imm < reg  <=> 0 < reg - imm*)
          | Anf.Lt -> [ Itype (rd, yreg, -imm, ADDI); Rtype (rd, Zero, rd, SLT) ]
          | Anf.And -> [ Itype (rd, yreg, imm, ANDI) ]
          | Anf.Or -> [ Itype (rd, yreg, imm, ORI) ]
        in
        merge_with_save_and_restore save insns restore)
      else (
        let xreg = find_reg [ yreg; rd ] in
        let save_x = sw ~v:xreg 0 ~src:Sp in
        let load_x = Pseudo.li xreg imm in
        let restore_x = lw ~rd:xreg 0 ~src:Sp in
        let build_instr insns =
          build_complex_instr [ save_x; load_x ] insns [ restore_x ]
        in
        let insns =
          match op with
          | Anf.Add -> build_instr [ Rtype (rd, xreg, yreg, ADD) ]
          | Anf.Sub -> build_instr [ Rtype (rd, xreg, yreg, SUB) ]
          | Anf.Mul -> build_instr [ Rtype (rd, xreg, yreg, MUL) ]
          | Anf.Div -> build_instr [ Rtype (rd, xreg, yreg, DIV) ]
          | Anf.Lt -> build_instr [ Rtype (rd, xreg, yreg, SLT) ]
          | Anf.Gt -> build_instr [ Rtype (rd, yreg, xreg, SLT) ]
          | Anf.Eq -> build_instr [ Rtype (rd, xreg, yreg, SUB); Pseudo.seqz ~rd ~src:rd ]
          | Anf.Neq -> build_instr [ Rtype (rd, xreg, yreg, SUB) ]
          | Anf.And -> build_instr [ Rtype (rd, yreg, xreg, AND) ]
          | Anf.Or -> build_instr [ Rtype (rd, yreg, xreg, OR) ]
        in
        merge_with_save_and_restore save insns restore)
    | Fl_var x, Fl_const y ->
      let imm = const_to_int y in
      let xloc = Base.Map.find_exn !curr_assignment x in
      let xreg, save, restore =
        match xloc with
        | Reg r -> r, None, None
        | StackLoc idx ->
          let xreg = find_reg [ rd ] in
          let save = sw ~v:xreg 0 ~src:Sp in
          let load = lw ~rd:xreg (word_size * idx) ~src:fp in
          let restore = lw ~rd:xreg 0 ~src:Sp in
          xreg, Some [ save; load ], Some [ restore ]
      in
      if imm_is_12_bytes imm
      then (
        let insns =
          match op with
          | Anf.Add -> [ Itype (rd, xreg, imm, ADDI) ]
          | Anf.Sub -> [ Itype (rd, xreg, -imm, ADDI) ]
          | Anf.Mul | Anf.Div ->
            let yreg = find_reg [ rd; xreg ] in
            let save = sw ~v:yreg 0 ~src:Sp in
            let load = Pseudo.li yreg imm in
            let restore = lw ~rd:yreg 0 ~src:Sp in
            let insn =
              match op with
              | Anf.Mul -> Rtype (rd, xreg, yreg, MUL)
              | Anf.Div -> Rtype (rd, xreg, yreg, DIV)
              | _ -> Utils.unreachable ()
            in
            [ save; load; insn; restore ]
          | Anf.Eq -> [ Itype (rd, xreg, -imm, ADDI); Pseudo.seqz ~rd ~src:rd ]
          | Anf.Neq -> [ Itype (rd, xreg, -imm, ADDI) ]
          | Anf.Lt -> [ Itype (rd, xreg, imm, SLTI) ]
          (* reg > imm <=> reg - imm > 0*)
          | Anf.Gt -> [ Itype (rd, xreg, -imm, ADDI); Rtype (rd, Zero, rd, SLT) ]
          | Anf.Or -> [ Itype (rd, xreg, imm, ORI) ]
          | Anf.And -> [ Itype (rd, xreg, imm, ANDI) ]
        in
        merge_with_save_and_restore save insns restore)
      else (
        let yreg = find_reg [ rd; xreg ] in
        let save_y = sw ~v:yreg 0 ~src:Sp in
        let load_y = Pseudo.li yreg imm in
        let restore_y = lw ~rd:yreg 0 ~src:Sp in
        let build_instr insns =
          build_complex_instr [ save_y; load_y ] @ insns @ [ restore_y ]
        in
        let insns =
          match op with
          | Anf.Add -> build_instr [ Rtype (rd, xreg, yreg, ADD) ]
          | Anf.Sub -> build_instr [ Rtype (rd, xreg, yreg, SUB) ]
          | Anf.Mul -> build_instr [ Rtype (rd, xreg, yreg, MUL) ]
          | Anf.Div -> build_instr [ Rtype (rd, xreg, yreg, DIV) ]
          | Anf.Eq -> build_instr [ Rtype (rd, xreg, yreg, SUB); Pseudo.seqz ~rd ~src:rd ]
          | Anf.Neq -> build_instr [ Rtype (rd, xreg, yreg, SUB) ]
          | Anf.Gt -> build_instr [ Rtype (rd, yreg, xreg, SLT) ]
          | Anf.Lt -> build_instr [ Rtype (rd, xreg, yreg, SLT) ]
          | Anf.And -> build_instr [ Rtype (rd, xreg, yreg, AND) ]
          | Anf.Or -> build_instr [ Rtype (rd, xreg, yreg, OR) ]
        in
        merge_with_save_and_restore save insns restore)
    | Fl_const x, Fl_const y ->
      let x = const_to_int x
      and y = const_to_int y in
      let xreg = find_reg [ rd ] in
      let yreg = find_reg [ rd; xreg ] in
      let save_x = sw ~v:xreg 0 ~src:Sp in
      let save_y = sw ~v:yreg word_size ~src:Sp in
      let load_x = Pseudo.li xreg x in
      let load_y = Pseudo.li yreg y in
      let restore_x = lw ~rd:xreg 0 ~src:Sp in
      let restore_y = lw ~rd:yreg word_size ~src:Sp in
      let insns =
        match op with
        | Anf.Add -> [ Rtype (rd, xreg, yreg, ADD) ]
        | Anf.Sub -> [ Rtype (rd, xreg, yreg, SUB) ]
        | Anf.Mul -> [ Rtype (rd, xreg, yreg, MUL) ]
        | Anf.Div -> [ Rtype (rd, xreg, yreg, MUL) ]
        | Anf.Eq -> [ Rtype (rd, xreg, yreg, SUB); Pseudo.seqz ~rd ~src:rd ]
        | Anf.Neq -> [ Rtype (rd, xreg, yreg, SUB) ]
        | Anf.Lt -> [ Rtype (rd, xreg, yreg, SLT) ]
        | Anf.Gt -> [ Rtype (rd, yreg, xreg, SLT) ]
        | Anf.And -> [ Rtype (rd, xreg, yreg, AND) ]
        | Anf.Or -> [ Rtype (rd, xreg, yreg, OR) ]
      in
      [ save_x; save_y; load_x; load_y ] @ insns @ [ restore_x; restore_y ]
    | _ -> Utils.internalfail "unexpected opearnds"
  ;;
end

module Function = struct
  (** evaluates cells count so that stack is aligned by 128-bit
      according to RISC-V calling convention*)
  let stack_size_aligned args_count =
    let byte_size = args_count * word_size in
    byte_size + (16 - (byte_size mod 16))
  ;;

  let used_registers assignment =
    assignment
    |> Base.Map.filter_map ~f:(function
      | Reg r -> Some r
      | StackLoc _ -> None)
    |> Base.Map.to_alist
    |> List.map snd
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
    let saved_registers = List.filter is_saved used_registers in
    let stack_size = stack_size_aligned (List.length saved_registers) in
    let extend_stack_insn = extend_stack_insn stack_size in
    let prologue =
      List.mapi (fun i reg -> sw ~v:reg ((i + 1) * word_size) ~src:Sp) saved_registers
    in
    let prologue = extend_stack_insn :: prologue in
    let restore_stack_insn = shrink_stack_insn stack_size in
    let epilogue =
      List.mapi (fun i reg -> lw ~rd:reg ((i + 1) * word_size) ~src:Sp) saved_registers
    in
    let epilogue = epilogue @ [ restore_stack_insn ] in
    prologue, epilogue
  ;;

  module Call = struct
    let save_and_restore_insns callee_arity =
      let callee_args = List.init callee_arity arg in
      let to_preserve = Ra :: callee_args in
      let extra_stack_size = stack_size_aligned (List.length to_preserve) in
      let extend_stack_insn = extend_stack_insn extra_stack_size in
      let save_insns =
        List.mapi (fun i reg -> sw ~v:reg ((i + 1) * word_size) ~src:Sp) to_preserve
      in
      let save_insns = extend_stack_insn :: save_insns in
      let restore_stack_insn = shrink_stack_insn extra_stack_size in
      let restore_insns =
        List.mapi (fun i reg -> lw ~rd:reg ((i + 1) * word_size) ~src:Sp) to_preserve
      in
      let restore_insns = restore_insns @ [ restore_stack_insn ] in
      save_insns, restore_insns
    ;;
  end
end

let codegen program = "1"
