open Liveness_analysis

type 'a assignment =
  | Reg of 'a
  | StackLoc of int

type 'a fun_allocation = (string, 'a assignment, Base.String.comparator_witness) Base.Map.t
type 'a program_allocation = (string * 'a fun_allocation) list

module Allocator (Storage : Registers_storage_intf.S) = struct
  let free_live_if_possible available live time =
    Storage.fold
      (fun (acc_av, acc_live) (reg, range) ->
        if range.e < time
        then Storage.add reg acc_av, acc_live
        else acc_av, Storage.add (reg, range) acc_live)
      (available, Storage.empty)
      live
  ;;

  let process_var available live range mapping =
    let time = range.s in
    let available, live = free_live_if_possible available live time in
    if Storage.size available > 0
    then (
      let reg, available = Storage.pop available in
      let res = Base.Map.set mapping ~key:range.var ~data:(Reg reg) in
      available, Storage.add (reg, range) live, res)
    else (
      let max = Storage.fold (fun acc (_, r) -> if r.e > acc then r.e else acc) 0 live in
      if max < range.e
      then (
        (* spill current var *)
        let res = Base.Map.set mapping ~key:range.var ~data:(StackLoc 0) in
        available, live, res)
      else (
        (* spill oldest *)
        let ((reg_with_max_range, max_range) as max) =
          Storage.find (fun (_, r) -> r.e = max) live
        in
        let live = Storage.remove max live |> Storage.add (reg_with_max_range, range) in
        let mapping = Base.Map.set mapping ~key:max_range.var ~data:(StackLoc 0) in
        let mapping =
          Base.Map.set mapping ~key:range.var ~data:(Reg reg_with_max_range)
        in
        available, live, mapping))
  ;;

  let scan_fun available vars =
    let live = Storage.empty in
    let empty = Base.Map.empty (module Base.String) in
    let _, _, res =
      List.fold_left
        (fun (av, live, mapping) range -> process_var av live range mapping)
        (available, live, empty)
        vars
    in
    res
  ;;

  let scan_program available liveness_analysis_result : 'a program_allocation=
    List.map (fun (id, vars) -> id, scan_fun available vars) liveness_analysis_result
end

let pp fmt pp_reg program_liveness_info =
  let open Format in
  let pp_value fmt = function
    | StackLoc i -> fprintf fmt "stacklock %i" i
    | Reg r -> pp_reg fmt r in
  let print_allocation fmt a =
    Base.Map.iteri a ~f:(fun ~key ~data ->
      fprintf fmt "@[%s: %a @,@]" key pp_value data ) in
  List.iter (fun (id, a) -> fprintf fmt "@[%s:@, %a@]@." id print_allocation a) program_liveness_info


