open Riscv
open Flambda
open Linear_scan_allocation

let word_size = 8
let arg_regs_count = 8
let curr_block = ref []
let block_counter = ref 0
let current_fun_arity = ref 0
let curr_assignment = ref (Base.Map.empty (module Base.String))

let find_reg forbidden =
  let used_temps =
    Base.Map.filter_map !curr_assignment ~f:(function
      | Loc_reg (Temp _ as r) -> Some r
      | _ -> None)
    |> Base.Map.to_alist
    |> List.map snd
  in
  let unused_temp =
    List.find_opt
      (fun t -> List.mem t used_temps |> not && List.mem t forbidden |> not)
      temporary_regs
  in
  match unused_temp with
  | Some v -> v, [], []
  | None ->
    let some_reg =
      List.find (fun r -> List.for_all (( <> ) r) forbidden) available_regs
    in
    let extend_stack_insn = extend_stack_insn (2 * word_size) in
    let save_reg = sw ~v:some_reg 0 ~dst:Sp in
    let restore_reg = lw ~rd:some_reg 0 ~src:Sp in
    let shrink_stack_insn = shrink_stack_insn (2 * word_size) in
    some_reg, [ extend_stack_insn; save_reg ], [ restore_reg; shrink_stack_insn ]
;;

(* let pp_loc fmt = function
   | Loc_reg r -> pp_reg fmt r
   | Loc_mem (b, o) -> Format.fprintf fmt "%i(%a)" o pp_reg b
   ;; *)

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

  (* handle case when src is on the stack*)
  let codegen_binop op x y rd =
    match x, y with
    | Fl_var x, Fl_var y ->
      let xloc = Base.Map.find_exn !curr_assignment x in
      let yloc = Base.Map.find_exn !curr_assignment y in
      let xreg, yreg, save, restore =
        match xloc, yloc with
        | Loc_reg xr, Loc_reg yr -> xr, yr, [], []
        | Loc_mem (xbase, xoffset), Loc_reg yr ->
          let xreg, save, restore = find_reg [ yr; rd ] in
          let load = lw ~rd:xreg xoffset ~src:xbase in
          xreg, yr, save @ [ load ], restore
        | Loc_reg xr, Loc_mem (ybase, yoffset) ->
          let yreg, save, restore = find_reg [ xr; rd ] in
          let load = lw ~rd:yreg yoffset ~src:ybase in
          xr, yreg, save @ [ load ], restore
        | Loc_mem (xbase, xoffset), Loc_mem (ybase, yoffset) ->
          let xreg, xsave, xrestore = find_reg [ rd ] in
          let yreg, ysave, yrestore = find_reg [ rd; xreg ] in
          let load0 = lw ~rd:xreg xoffset ~src:xbase in
          let load1 = lw ~rd:yreg yoffset ~src:ybase in
          xreg, yreg, xsave @ ysave @ [ load0; load1 ], xrestore @ yrestore
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
      save @ (insn :: restore)
    | Fl_const x, Fl_var y ->
      let imm = const_to_int x in
      let yloc = Base.Map.find_exn !curr_assignment y in
      let yreg, save, restore =
        match yloc with
        | Loc_reg r -> r, [], []
        | Loc_mem (base, offset) ->
          let yreg, save, restore = find_reg [ rd ] in
          let load = lw ~rd:yreg offset ~src:base in
          yreg, save @ [ load ], restore
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
            let xreg, xsave, xrestore = find_reg [ yreg; rd ] in
            let load = Pseudo.li xreg imm in
            let insn =
              match op with
              | Anf.Mul -> Rtype (rd, xreg, yreg, MUL)
              | Anf.Div -> Rtype (rd, xreg, yreg, DIV)
              | _ -> Utils.unreachable ()
            in
            xsave @ [ load; insn ] @ xrestore
          | Anf.Eq -> [ Itype (rd, yreg, -imm, ADDI); Pseudo.seqz ~rd ~src:rd ]
          | Anf.Neq -> [ Itype (rd, yreg, -imm, ADDI) ]
          | Anf.Gt -> [ Itype (rd, yreg, imm, SLTI) ]
          (* imm < reg  <=> 0 < reg - imm*)
          | Anf.Lt -> [ Itype (rd, yreg, -imm, ADDI); Rtype (rd, Zero, rd, SLT) ]
          | Anf.And -> [ Itype (rd, yreg, imm, ANDI) ]
          | Anf.Or -> [ Itype (rd, yreg, imm, ORI) ]
        in
        save @ insns @ restore)
      else (
        let xreg, xsave, xrestore = find_reg [ yreg; rd ] in
        let load_x = Pseudo.li xreg imm in
        let build_instr insns = build_complex_instr xsave (load_x :: insns) xrestore in
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
        save @ insns @ restore)
    | Fl_var x, Fl_const y ->
      let imm = const_to_int y in
      let xloc = Base.Map.find_exn !curr_assignment x in
      let xreg, save, restore =
        match xloc with
        | Loc_reg r -> r, [], []
        | Loc_mem (base, offset) ->
          let xreg, xsave, xrestore = find_reg [ rd ] in
          let load = lw ~rd:xreg offset ~src:base in
          xreg, xsave @ [ load ], xrestore
      in
      if imm_is_12_bytes imm
      then (
        let insns =
          match op with
          | Anf.Add -> [ Itype (rd, xreg, imm, ADDI) ]
          | Anf.Sub -> [ Itype (rd, xreg, -imm, ADDI) ]
          | Anf.Mul | Anf.Div ->
            let yreg, ysave, yrestore = find_reg [ rd; xreg ] in
            let load = Pseudo.li yreg imm in
            let insn =
              match op with
              | Anf.Mul -> Rtype (rd, xreg, yreg, MUL)
              | Anf.Div -> Rtype (rd, xreg, yreg, DIV)
              | _ -> Utils.unreachable ()
            in
            ysave @ [ load; insn ] @ yrestore
          | Anf.Eq -> [ Itype (rd, xreg, -imm, ADDI); Pseudo.seqz ~rd ~src:rd ]
          | Anf.Neq -> [ Itype (rd, xreg, -imm, ADDI) ]
          | Anf.Lt -> [ Itype (rd, xreg, imm, SLTI) ]
          (* reg > imm <=> reg - imm > 0*)
          | Anf.Gt -> [ Itype (rd, xreg, -imm, ADDI); Rtype (rd, Zero, rd, SLT) ]
          | Anf.Or -> [ Itype (rd, xreg, imm, ORI) ]
          | Anf.And -> [ Itype (rd, xreg, imm, ANDI) ]
        in
        save @ insns @ restore)
      else (
        let yreg, ysave, yrestore = find_reg [ rd; xreg ] in
        let load_y = Pseudo.li yreg imm in
        let build_instr insns = build_complex_instr ysave (load_y :: insns) yrestore in
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
        save @ insns @ restore)
    | Fl_const x, Fl_const y ->
      let x = const_to_int x
      and y = const_to_int y in
      let xreg, xsave, xrestore = find_reg [ rd ] in
      let yreg, ysave, yrestore = find_reg [ rd; xreg ] in
      let load_x = Pseudo.li xreg x in
      let load_y = Pseudo.li yreg y in
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
      xsave @ ysave @ (load_x :: load_y :: insns) @ xrestore @ yrestore
    | _ -> Utils.internalfail "unexpected opearnds"
  ;;
end

module Memory = struct
  (** evaluates cells count so that stack is kep 16-bytes aligned
      according to RISC-V calling convention*)
  let stack_size_aligned size = size + ((16 - (size mod 16)) mod 16)

  (* (16 - x mod 16 ) mod*)

  let extend_stack_aligned size =
    let aligned_size = stack_size_aligned size in
    extend_stack_insn aligned_size
  ;;

  let shrink_stack_aligned size =
    let aligned_size = stack_size_aligned size in
    shrink_stack_insn aligned_size
  ;;

  let store_rvalue location rvalue =
    match location, rvalue with
    | Loc_reg rd, Rv_reg src when rd = src -> []
    | Loc_reg rd, Rv_reg src -> [ Pseudo.mv ~rd ~src ]
    | Loc_reg rd, Rv_imm imm -> [ Pseudo.li rd imm ]
    | Loc_reg rd, Rv_mem (src, offset) -> [ lw ~rd offset ~src ]
    | Loc_mem (dst, offset), Rv_reg v -> [ sw ~v offset ~dst ]
    | Loc_mem (dst, offset), Rv_imm imm ->
      let temp_reg, save, restore = find_reg [ dst ] in
      let load = Pseudo.li temp_reg imm in
      let store = sw ~v:temp_reg offset ~dst in
      save @ (load :: store :: restore)
    | Loc_mem (dst_reg, dst_offset), Rv_mem (src_reg, src_offset) ->
      let temp_reg, save, restore = find_reg [ dst_reg; src_reg ] in
      let load = lw ~rd:temp_reg src_offset ~src:src_reg in
      let store = sw ~v:temp_reg dst_offset ~dst:dst_reg in
      save @ (load :: store :: restore)
    | _ -> []
  ;;
end

module Function = struct
  let used_registers assignment =
    assignment
    |> Base.Map.filter_map ~f:(function
      | Loc_reg r -> Some r
      | Loc_mem _ -> None)
    |> Base.Map.to_alist
    |> List.map snd
  ;;

  let saved_used_registers assignment =
    used_registers assignment |> List.filter is_saved |> Utils.ListUtils.distinct
  ;;

  let temporary_used_registers assignment arity =
    let args = List.init (min 8 arity) arg in
    args @ used_registers assignment
    |> List.filter (fun r -> is_saved r |> not)
    |> Utils.ListUtils.distinct
  ;;

  let resolve_param_locations param_names arity =
    if arity <= arg_regs_count
    then List.mapi (fun i name -> name, Loc_reg (arg i)) param_names
    else (
      let reg_args, stack_args = Base.List.split_n param_names arg_regs_count in
      let reg_args = List.mapi (fun i name -> name, Loc_reg (arg i)) reg_args in
      let stack_args =
        List.mapi (fun i name -> name, Loc_mem (fp, (i + 1) * word_size)) stack_args
      in
      reg_args @ stack_args)
  ;;

  let stack_locals_count regs_assignment =
    Base.Map.filter regs_assignment ~f:(function
      | Loc_reg _ -> false
      | Loc_mem _ -> true)
    |> Base.Map.length
  ;;

  (* la t0, fptr
     la t1, closure
     sw t0, 0(t1)
     lw t2, 0(t1)*)

  let prologue_and_epilogue regs_assignment =
    let saved_registers = fp :: saved_used_registers regs_assignment in
    let stack_locals_count = stack_locals_count regs_assignment in
    let save_regs =
      List.mapi (fun i reg -> sw ~v:reg ((i + 1) * word_size) ~dst:Sp) saved_registers
    in
    let stack_cells_count = stack_locals_count + List.length save_regs in
    let extra_stack_space_aligned =
      Memory.stack_size_aligned (word_size * stack_cells_count)
    in
    let extend_stack_insn = Memory.extend_stack_aligned extra_stack_space_aligned in
    let set_init_sp_to_fp = Itype (fp, Sp, extra_stack_space_aligned, ADDI) in
    let prologue = (extend_stack_insn :: save_regs) @ [ set_init_sp_to_fp ] in
    let restore_stack_insn = Memory.shrink_stack_aligned extra_stack_space_aligned in
    let restore_regs =
      List.mapi (fun i reg -> lw ~rd:reg ((i + 1) * word_size) ~src:Sp) saved_registers
    in
    let epilogue = restore_regs @ [ restore_stack_insn ] in
    prologue, epilogue
  ;;

  module Call = struct
    let save_and_restore_insns caller_arity retvalue_location =
      let callee_args = List.init caller_arity arg in
      let callee_temps = temporary_used_registers !curr_assignment caller_arity in
      let to_preserve =
        callee_args @ (Ra :: callee_temps)
        |> Utils.ListUtils.distinct
        |> List.filter (fun reg ->
          match retvalue_location with
          | Loc_reg r when r = reg -> false
          | _ -> true)
      in
      let extend_stack_insn =
        Memory.extend_stack_aligned (word_size * List.length to_preserve)
      in
      let save_insns =
        List.mapi (fun i reg -> sw ~v:reg ((i + 1) * word_size) ~dst:Sp) to_preserve
      in
      let save_insns = extend_stack_insn :: save_insns in
      let restore_stack_insn =
        Memory.shrink_stack_aligned (word_size * List.length to_preserve)
      in
      let restore_insns =
        List.mapi (fun i reg -> lw ~rd:reg ((i + 1) * word_size) ~src:Sp) to_preserve
      in
      save_insns, restore_insns @ [ restore_stack_insn ]
    ;;

    let arg_locations args_count =
      List.init args_count (fun i ->
        if i < 8 then Loc_reg (arg i) else Loc_mem (fp, word_size * (i - 7)))
    ;;

    let extend_stack_for_args stack_args_count =
      Memory.extend_stack_aligned (stack_args_count * word_size)
    ;;

    let shrink_stack_for_args stack_args_count =
      Memory.shrink_stack_aligned (stack_args_count * word_size)
    ;;
  end

  (* la t0, closure
     la t1, my_function
     sw t1, t0(0)*)
  module Runtime = struct
    let alloc_tuple size retvalue_location ~save_retvalue =
      let save, restore =
        Call.save_and_restore_insns !current_fun_arity retvalue_location
      in
      let a0 = arg 0 in
      let load_size = Pseudo.li a0 size in
      let call = Pseudo.call Runtime.alloc_tuple in
      save @ [ load_size; call ] @ save_retvalue @ restore
    ;;

    (** this function is supposed to use in [action] of [alloc_tuple], so
        callee-saved args are considered to be saved on call-site *)
    let alloc_closure f_label env_addr_reg arity fv_count =
      let store_env_addr = Pseudo.mv ~rd:(arg 1) ~src:env_addr_reg in
      let load_faddr = Pseudo.la (arg 0) f_label in
      let load_arity = Pseudo.li (arg 2) arity in
      let load_fvcount = Pseudo.li (arg 3) fv_count in
      let call = Pseudo.call Runtime.rv_alloc_closure in
      [ store_env_addr; load_faddr; load_arity; load_fvcount; call ]
    ;;

    (** args must be placed in [before_call_insns] from call sight of this method *)
    let call callee_name before_call_insns retvalue_location ~save_retvalue =
      let save, restore =
        Call.save_and_restore_insns !current_fun_arity retvalue_location
      in
      let call = Pseudo.call callee_name in
      save @ before_call_insns @ (call :: save_retvalue) @ restore
    ;;
  end
end

let rec codegen_flambda location =
  let append_reversed insns = curr_block := List.rev insns @ !curr_block in
  function
  | Fl_var id ->
    (match Base.Map.find_exn !curr_assignment id with
     | Loc_reg reg -> Rv_reg reg
     | Loc_mem (base, offset) -> Rv_mem (base, offset))
    |> Memory.store_rvalue location
  | Fl_const c ->
    let const = Arith.const_to_int c in
    Rv_imm const |> Memory.store_rvalue location
  | Fl_tuple elems ->
    let size = List.length elems in
    let a0 = arg 0 in
    let fill_tuple_insns =
      List.mapi
        (fun i e ->
          let loc = Loc_mem (a0, i * word_size) in
          codegen_flambda loc e)
        elems
      |> List.concat
    in
    let save_tuple = Memory.store_rvalue location a0value in
    Function.Runtime.alloc_tuple
      size
      location
      ~save_retvalue:(fill_tuple_insns @ save_tuple)
  | Fl_cons (x, xs) ->
    let a0 = Loc_reg (arg 0) in
    let a1 = Loc_reg (arg 1) in
    let before_call_insns = codegen_flambda a0 x @ codegen_flambda a1 xs in
    let save_retvalue = Memory.store_rvalue location a0value in
    Function.Runtime.call Runtime.list_cons before_call_insns location ~save_retvalue
  | Fl_getfield (idx, obj) ->
    let some_reg, save, restore = find_reg [] in
    let extend_stack = Memory.extend_stack_aligned word_size in
    let load_addr = codegen_flambda (Loc_reg some_reg) obj in
    let load_value = lw ~rd:some_reg (idx * word_size) ~src:some_reg in
    let store = Memory.store_rvalue location (Rv_reg some_reg) in
    let shrink_stack = Memory.shrink_stack_aligned word_size in
    save @ (extend_stack :: load_addr) @ (load_value :: store) @ (shrink_stack :: restore)
  | Fl_binop (op, x, y) ->
    let rd, save, restore =
      match location with
      | Loc_reg reg -> reg, [], []
      | Loc_mem (_, _) ->
        let some_reg, save, restore = find_reg [] in
        some_reg, save, restore
    in
    let binop_insns = Arith.codegen_binop op x y rd in
    let store_to_loc = Memory.store_rvalue location (Rv_reg rd) in
    save @ binop_insns @ store_to_loc @ restore
  | Fl_unop (Anf.Not, x) ->
    let rd, save, restore =
      match location with
      | Loc_reg reg -> reg, [], []
      | Loc_mem (_, _) ->
        let some_reg, save, restore = find_reg [] in
        let extend_stack = Memory.extend_stack_aligned word_size in
        let shrink_stack = Memory.shrink_stack_aligned word_size in
        some_reg, save @ [ extend_stack ], shrink_stack :: restore
    in
    let insns = codegen_flambda (Loc_reg rd) x in
    let neg = Pseudo.neg rd in
    let store_to_loc = Memory.store_rvalue location (Rv_reg rd) in
    save @ insns @ (neg :: store_to_loc) @ restore
  | Fl_let (name, v, scope) ->
    let loc =
      match name with
      | Some name -> Base.Map.find_exn !curr_assignment name
      | None -> Loc_reg Zero
    in
    let insns = codegen_flambda loc v in
    append_reversed insns;
    let insns = codegen_flambda location scope in
    insns
  | Fl_app (Fl_closure { name; env_size; arity; _ }, args)
    when arity = List.length args && env_size = 0 ->
    let save, restore =
      Function.Call.save_and_restore_insns !current_fun_arity location
    in
    let arg_locations = Function.Call.arg_locations arity in
    let arrange = List.map2 codegen_flambda arg_locations args |> List.concat in
    let stack_args_count = max 0 (arity - arg_regs_count) in
    let call = Pseudo.call name in
    let save_retvalue = Memory.store_rvalue location a0value in
    (* since [location] can be equal to one of callee saved registers, [restore] can
       overwrite saving function ret value. It can be fixed with more smart register allocation working with live intervals,
       but we figure it out with storing on stack and then moving to desired [location] after restore *)
    if stack_args_count = 0
    then save @ arrange @ (call :: save_retvalue @ restore)
    else (
      let extend_stack = Function.Call.extend_stack_for_args stack_args_count in
      let shrink_stack = Function.Call.shrink_stack_for_args stack_args_count in
      save @ (extend_stack :: arrange) @ (call :: save_retvalue @ shrink_stack :: restore))
  | Fl_app (f, args) ->
    let arg_locations = Function.Call.arg_locations (1 + List.length args) in
    let stack_args_count = max 0 (List.length args - arg_regs_count) in
    let arrange = List.map2 codegen_flambda arg_locations (f :: args) |> List.concat in
    let save_retvalue = Memory.store_rvalue location a0value in
    if stack_args_count = 0
    then Function.Runtime.call Runtime.call_closure arrange location ~save_retvalue
    else (
      let extend_stack = Function.Call.extend_stack_for_args stack_args_count in
      let shrink_stack = Function.Call.shrink_stack_for_args stack_args_count in
      Function.Runtime.call
        Runtime.call_closure
        (extend_stack :: arrange)
        location
        ~save_retvalue:(shrink_stack :: save_retvalue))
  | Fl_closure { arity; env_size; arrange; name } ->
    let a0 = arg 0 in
    let arrange_env_insns =
      List.map
        (fun (idx, arg) ->
          let loc = Loc_mem (a0, idx * word_size) in
          codegen_flambda loc arg)
        arrange
      |> List.concat
    in
    let alloc_closure_insns = Function.Runtime.alloc_closure name a0 arity env_size in
    let save_closure = Memory.store_rvalue location a0value in
    let save_retvalue = arrange_env_insns @ alloc_closure_insns @ save_closure in
    Function.Runtime.alloc_tuple (env_size + arity) location ~save_retvalue
  | Fl_ite (c, t, e) ->
    let some_reg, save, restore = find_reg [] in
    let cond = codegen_flambda (Loc_reg some_reg) c in
    let fresh_label l =
      let s = Format.sprintf ".L_%s_%i" l !block_counter in
      block_counter := !block_counter + 1;
      s
    in
    let else_label = fresh_label "else" in
    let join_label = fresh_label "join" in
    let branch = Btype (some_reg, Zero, else_label, BEQ) in
    curr_block := (branch :: cond) @ save @ !curr_block;
    let then_final_insns = codegen_flambda location t |> List.rev in
    let jump_join = Pseudo.jump join_label in
    curr_block := (Label else_label :: jump_join :: then_final_insns) @ !curr_block;
    let else_final_insns = codegen_flambda location e |> List.rev in
    curr_block := (restore @ (Label join_label :: else_final_insns)) @ !curr_block;
    []
;;

let convert_generic_assignment_to_rv generic =
  Base.Map.map generic ~f:(function
    | Reg r -> Loc_reg r
    | StackLoc idx -> Loc_mem (fp, idx * word_size))
;;

let codegen_fun (fun_name, { param_names; arity; body }) regs_assignment =
  let has_env = List.length param_names != arity in
  let arity, param_names =
    if has_env then arity + 1, "env" :: param_names else arity, param_names
  in
  let rv_locals_assignment = convert_generic_assignment_to_rv regs_assignment in
  let explicit_params, env_params = Base.List.split_n param_names arity in
  current_fun_arity := arity;
  let explicit_param_locations = Function.resolve_param_locations explicit_params arity in
  let env_param_locations =
    let a0 = arg 0 in
    List.mapi (fun i name -> name, Loc_mem (a0, i * word_size)) env_params
  in
  curr_assignment := rv_locals_assignment;
  List.iter
    (fun (arg, loc) ->
      curr_assignment := Base.Map.set !curr_assignment ~key:arg ~data:loc)
    explicit_param_locations;
  List.iter
    (fun (env_param, loc) ->
      curr_assignment := Base.Map.set !curr_assignment ~key:env_param ~data:loc)
    env_param_locations;
  let prologue, epilogue = Function.prologue_and_epilogue !curr_assignment in
  let prologue, epilogue = List.rev prologue, List.rev epilogue in
  let fun_name_with_directive = Format.sprintf ".global %s" fun_name in
  curr_block := prologue @ [ Label fun_name_with_directive ] @ !curr_block;
  let return_value_location = Loc_reg (arg 0) in
  let final_insn = codegen_flambda return_value_location body |> List.rev in
  let ret = Pseudo.ret in
  curr_block := (ret :: epilogue) @ final_insn @ !curr_block
;;

module RvAllocator = Linear_scan_allocation.Allocator (RegistersStorage)

let codegen_program flstructure =
  let available_regs = Riscv.available_regs in
  let regs_assignment = RvAllocator.scan_program available_regs flstructure in
  List.iter2
    (fun (fun_name, fun_decl) (fun_name', fun_regs_assignment) ->
      if fun_name <> fun_name' then Utils.internalfail "order";
      match fun_decl with
      | Fun_with_env decl | Fun_without_env decl ->
        codegen_fun (fun_name, decl) fun_regs_assignment)
    flstructure
    regs_assignment;
  !curr_block |> List.rev
;;

let dump instructions =
  let open Stdlib.Format in
  List.iter (fun insn -> fprintf std_formatter "%a" pp_insn insn) instructions
;;
