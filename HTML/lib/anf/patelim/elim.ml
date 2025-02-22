let p_elim_decls prog =
  let prog_res = PM_elim.pm_elim prog in
  Result.map (List.map IR_utils.transform_back_decl) prog_res
;;
