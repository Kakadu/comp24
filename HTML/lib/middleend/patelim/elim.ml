let p_elim_decls prog =
  let prog_res = PC_elim.pc_elim prog in
  Result.map (List.map IR_utils.transform_back_decl) prog_res
;;
