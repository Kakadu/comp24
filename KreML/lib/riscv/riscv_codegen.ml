open Riscv
open Flambda
open Linear_scan_allocation

type block = instruction list
type fun_body = block list
type generated_code = (string, fun_body) Hashtbl.t

let word_size = 8
let arg_regs_count = 8
let curr_block = ref []
let block_counter = ref 0
let current_fun_arity = ref 0

(* when we codegen ite we must preserve rd of binding*)
let curr_rd = ref (temp 0)
let curr_assignment = ref (Base.Map.empty (module Base.String))

let find_reg forbidden =
  List.find (fun r -> List.for_all (( <> ) r) forbidden) available_regs
;;

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
    match x, y with
    | Fl_var x, Fl_var y ->
      let xloc = Base.Map.find_exn !curr_assignment x in
      let yloc = Base.Map.find_exn !curr_assignment y in
      let xreg, yreg, save, restore =
        match xloc, yloc with
        | Reg xr, Reg yr -> xr, yr, None, None
        | StackLoc x_idx, Reg yr ->
          let xreg = find_reg [ yr; rd ] in
          let save = sw ~v:xreg 0 ~dst:Sp in
          let load = lw ~rd:xreg (x_idx * word_size) ~src:fp in
          let restore = lw ~rd:xreg 0 ~src:Sp in
          xreg, yr, Some [ save; load ], Some [ restore ]
        | Reg xr, StackLoc y_idx ->
          let yreg = find_reg [ xr; rd ] in
          let save = sw ~v:yreg 0 ~dst:Sp in
          let load = lw ~rd:yreg (y_idx * word_size) ~src:fp in
          let restore = lw ~rd:yreg 0 ~src:Sp in
          xr, yreg, Some [ save; load ], Some [ restore ]
        | StackLoc x_idx, StackLoc y_idx ->
          let xreg = find_reg [ rd ] in
          let yreg = find_reg [ rd; xreg ] in
          let save0 = sw ~v:xreg 0 ~dst:Sp in
          let save1 = sw ~v:yreg word_size ~dst:Sp in
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
          let save = sw ~v:yreg 0 ~dst:Sp in
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
            let save = sw ~v:xreg 0 ~dst:Sp in
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
        let save_x = sw ~v:xreg 0 ~dst:Sp in
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
          let save = sw ~v:xreg 0 ~dst:Sp in
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
            let save = sw ~v:yreg 0 ~dst:Sp in
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
        let save_y = sw ~v:yreg 0 ~dst:Sp in
        let load_y = Pseudo.li yreg imm in
        let restore_y = lw ~rd:yreg 0 ~src:Sp in
        let build_instr insns =
          build_complex_instr [ save_y; load_y ] insns [ restore_y ]
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
      let save_x = sw ~v:xreg 0 ~dst:Sp in
      let save_y = sw ~v:yreg word_size ~dst:Sp in
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

module Memory = struct
  let store_rvalue location rvalue =
    match location, rvalue with
    | Loc_reg rd, Rv_reg src when rd = src -> []
    | Loc_reg rd, Rv_reg src -> [ Pseudo.mv ~rd ~src ]
    | Loc_reg rd, Rv_imm imm -> [ Pseudo.li rd imm ]
    | Loc_reg rd, Rv_mem (src, offset) -> [ lw ~rd offset ~src ]
    | Loc_mem (dst, offset), Rv_reg v -> [ sw ~v offset ~dst ]
    | Loc_mem (dst, offset), Rv_imm imm ->
      let temp_reg = find_reg [ dst ] in
      let save = sw ~v:temp_reg 0 ~dst:Sp in
      let load = Pseudo.li temp_reg imm in
      let store = sw ~v:temp_reg offset ~dst in
      let restore = lw ~rd:temp_reg 0 ~src:Sp in
      [ save; load; store; restore ]
    | Loc_mem (dst_reg, dst_offset), Rv_mem (src_reg, src_offset) ->
      let temp_reg = find_reg [ dst_reg; src_reg ] in
      let save = sw ~v:temp_reg 0 ~dst:Sp in
      let load = lw ~rd:temp_reg src_offset ~src:src_reg in
      let store = sw ~v:temp_reg dst_offset ~dst:dst_reg in
      let restore = lw ~rd:temp_reg 0 ~src:Sp in
      [ save; load; store; restore ]
    | _ -> []
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

  let saved_used_registers assignment = used_registers assignment |> List.filter is_saved

  let temporary_used_registers assignment arity =
    let args = List.init (max 8 arity) arg in
    args @ used_registers assignment |> List.filter (fun r -> is_saved r |> not)
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
    let saved_registers = saved_used_registers assignment in
    let stack_size = stack_size_aligned (List.length saved_registers) in
    let extend_stack_insn = extend_stack_insn stack_size in
    let prologue =
      List.mapi (fun i reg -> sw ~v:reg ((i + 1) * word_size) ~dst:Sp) saved_registers
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
    let save_and_restore_insns caller_arity =
      let callee_args = List.init caller_arity arg in
      let callee_temps = temporary_used_registers !curr_assignment caller_arity in
      let to_preserve = (Ra :: callee_args) @ callee_temps in
      let extra_stack_size = stack_size_aligned (List.length to_preserve) in
      let extend_stack_insn = extend_stack_insn extra_stack_size in
      let save_insns =
        List.mapi (fun i reg -> sw ~v:reg ((i + 1) * word_size) ~dst:Sp) to_preserve
      in
      let save_insns = extend_stack_insn :: save_insns in
      let restore_stack_insn = shrink_stack_insn extra_stack_size in
      let restore_insns =
        List.mapi (fun i reg -> lw ~rd:reg ((i + 1) * word_size) ~src:Sp) to_preserve
      in
      let restore_insns = restore_insns @ [ restore_stack_insn ] in
      save_insns, restore_insns
    ;;

    let arg_locations args_count =
      List.init args_count (fun i ->
        if i < 8 then Loc_reg (arg i) else Loc_mem (Sp, word_size * (i - 7)))
    ;;
  end

  module Runtime = struct
    let alloc_tuple size action =
      let save, restore = Call.save_and_restore_insns !current_fun_arity in
      let a0 = arg 0 in
      let load_size = Pseudo.li a0 size in
      let call = Pseudo.call Runtime.alloc_tuple in
      let extra_insns = action a0 in
      save @ [ load_size; call ] @ extra_insns @ restore
    ;;

    (** args must be placed in correct locations from call sight of this method *)
    let call callee_name before_call_insns result_action =
      let save, restore = Call.save_and_restore_insns !current_fun_arity in
      let call = Pseudo.call callee_name in
      let extra_insns = result_action (arg 0) in
      save @ before_call_insns @ (call :: extra_insns) @ restore
    ;;
  end
end

(* let a =
   if flag then
   let b = 6 in
   a + 6
   else 5*)

let rec codegen_flambda location =
  let append_reversed insns = curr_block := List.rev insns @ !curr_block in
  function
  | Fl_var id ->
    (match Base.Map.find_exn !curr_assignment id with
     | Reg reg -> Rv_reg reg |> Memory.store_rvalue location
     | StackLoc idx -> Rv_mem (fp, idx * word_size) |> Memory.store_rvalue location)
  | Fl_const c ->
    let const = Arith.const_to_int c in
    Rv_imm const |> Memory.store_rvalue location
  | Fl_tuple elems ->
    let size = List.length elems in
    Function.Runtime.alloc_tuple size (fun addr ->
      let fill_insns =
        List.mapi
          (fun i e ->
            let loc = Loc_mem (addr, i * word_size) in
            codegen_flambda loc e)
          elems
        |> List.concat
      in
      let store = Memory.store_rvalue location (Rv_reg addr) in
      fill_insns @ store)
  | Fl_cons (x, xs) ->
    let a0 = Loc_reg (arg 0) in
    let a1 = Loc_reg (arg 1) in
    let before_call_insns = codegen_flambda a0 x @ codegen_flambda a1 xs in
    Function.Runtime.call Runtime.list_cons before_call_insns (fun addr ->
      Memory.store_rvalue (Loc_reg addr) (Rv_reg addr))
  | Fl_getfield (idx, obj) ->
    let some_reg = find_reg [] in
    let save = sw ~v:some_reg 0 ~dst:Sp in
    let extend_stack = extend_stack_insn word_size in
    let load_addr = codegen_flambda (Loc_reg some_reg) obj in
    let load_value = lw ~rd:some_reg (idx * word_size) ~src:some_reg in
    let store = Memory.store_rvalue location (Rv_reg some_reg) in
    let shrink_stack = shrink_stack_insn word_size in
    let restore = lw ~rd:some_reg 0 ~src:Sp in
    [ save; extend_stack ] @ load_addr @ (load_value :: store) @ [ shrink_stack; restore ]
  | Fl_binop (op, x, y) ->
    let rd, save, restore =
      match location with
      | Loc_reg reg -> reg, None, None
      | Loc_mem (_, _) ->
        let some_reg = find_reg [] in
        let save = sw ~v:some_reg 0 ~dst:Sp in
        let extend_stack = extend_stack_insn word_size in
        let shrink_stack = shrink_stack_insn word_size in
        let restore = lw ~rd:some_reg 0 ~src:Sp in
        some_reg, Some [ save; extend_stack ], Some [ shrink_stack; restore ]
    in
    let binop_insns = Arith.codegen_binop op x y rd in
    let store_to_loc = Memory.store_rvalue location (Rv_reg rd) in
    (match save, restore with
     | None, None -> binop_insns @ store_to_loc
     | Some s, Some r -> s @ binop_insns @ store_to_loc @ r
     | _ -> Utils.unreachable ())
  | Fl_unop (Anf.Not, x) ->
    let rd, save, restore =
      match location with
      | Loc_reg reg -> reg, None, None
      | Loc_mem (_, _) ->
        let some_reg = find_reg [] in
        let save = sw ~v:some_reg 0 ~dst:Sp in
        let extend_stack = extend_stack_insn word_size in
        let shrink_stack = shrink_stack_insn word_size in
        let restore = lw ~rd:some_reg 0 ~src:Sp in
        some_reg, Some [ save; extend_stack ], Some [ shrink_stack; restore ]
    in
    let insns = codegen_flambda (Loc_reg rd) x in
    let neg = Pseudo.neg rd in
    let store_to_loc = Memory.store_rvalue location (Rv_reg rd) in
    (match save, restore with
     | None, None -> insns @ (neg :: store_to_loc)
     | Some s, Some r -> s @ insns @ (neg :: store_to_loc) @ r
     | _ -> Utils.unreachable ())
  | Fl_let (name, v, scope) ->
    let loc =
      match name with
      | Some name -> Base.Map.find_exn !curr_assignment name
      | None -> Reg Zero
    in
    let loc =
      match loc with
      | Reg r -> Loc_reg r
      | StackLoc idx -> Loc_mem (fp, idx * word_size)
    in
    let insns = codegen_flambda loc v in
    append_reversed insns;
    let _ = codegen_flambda idc_loc scope in
    []
  | Fl_app (Fl_closure { name; env_size; arity; _ }, args)
    when arity = List.length args && env_size = 0 ->
    let save, restore = Function.Call.save_and_restore_insns !current_fun_arity in
    let arg_locations = Function.Call.arg_locations arity in
    let arrange = List.map2 codegen_flambda arg_locations args |> List.concat in
    (* save function result *)
    let call = Pseudo.call name in
    let save_result = Memory.store_rvalue location (Rv_reg (arg 0)) in
    save @ arrange @ (call :: save_result) @ restore
  | Fl_app (f, args) ->
    let arg_locations = Function.Call.arg_locations (1 + List.length args) in
    let arrange = List.map2 codegen_flambda arg_locations (f :: args) |> List.concat in
    Function.Runtime.call Runtime.call_closure arrange (fun addr ->
      Memory.store_rvalue location (Rv_reg addr))
  | Fl_closure { arity; env_size; arrange; _ } ->
    Function.Runtime.alloc_tuple (env_size + arity) (fun addr ->
      let arrange_insns =
        List.map
          (fun (idx, arg) ->
            let loc = Loc_mem (addr, idx * word_size) in
            codegen_flambda loc arg)
          arrange
        |> List.concat
      in
      let store = Memory.store_rvalue location (Rv_reg addr) in
      arrange_insns @ store)
  | Fl_ite (c, t, e) ->
    let some_reg = find_reg [] in
    let save = sw ~v:some_reg 0 ~dst:Sp in
    let extend_stack = extend_stack_insn word_size in
    let shrink_stack = shrink_stack_insn word_size in
    let restore = lw ~rd:some_reg 0 ~src:Sp in
    let cond = codegen_flambda (Loc_reg some_reg) c in
    let fresh_label l =
      let s = Format.sprintf ".L_%s_%i" l !block_counter in
      block_counter := !block_counter + 1;
      s
    in
    let else_label = fresh_label "else" in
    let join_label = fresh_label "join" in
    let branch = Btype (some_reg, Zero, else_label, BEQ) in
    curr_block := (branch :: cond) @ (extend_stack :: save :: !curr_block);
    let then_final_insns = codegen_flambda location t in
    let jump_join = Pseudo.jump join_label in
    curr_block := (Label else_label :: jump_join :: then_final_insns) @ !curr_block;
    let else_final_insns = codegen_flambda location e in
    curr_block := (restore :: shrink_stack :: Label join_label :: else_final_insns) @ !curr_block;
    []
;;

(* let a = f x in *)

let codegen program = "1"
