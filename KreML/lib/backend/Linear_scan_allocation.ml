open Liveness_analysis

type 'a location =
  | Reg of 'a
  | StackLoc of int

type 'a regs_assignment =
  (string, 'a location, Base.String.comparator_witness) Base.Map.t

type 'a program_allocation = (string * 'a regs_assignment) list

module Allocator (Storage : Registers_storage_intf.S) = struct
  let free_live_if_possible available live time =
    Storage.fold
      (fun (acc_av, acc_live) (reg, range) ->
        if range.e <= time
        then Storage.add reg acc_av, acc_live
        else acc_av, Storage.add (reg, range) acc_live)
      (available, Storage.empty)
      live
  ;;

  let process_var available live range spilled_count mapping =
    let time = range.s in
    let available, live = free_live_if_possible available live time in
    (* let open Format in
       fprintf std_formatter "available size: %i" (Storage.size available); *)
    if Storage.size available > 0
    then (
      let reg, available = Storage.pop available in
      let res = Base.Map.set mapping ~key:range.var ~data:(Reg reg) in
      available, Storage.add (reg, range) live, spilled_count, res)
    else (
      let max = Storage.fold (fun acc (_, r) -> if r.e > acc then r.e else acc) 0 live in
      if max <= range.e
      then (
        (* spill current var *)
        let res = Base.Map.set mapping ~key:range.var ~data:(StackLoc spilled_count) in
        available, live, spilled_count + 1, res)
      else (
        (* spill oldest *)
        let ((reg_with_max_range, max_range) as max) =
          Storage.find (fun (_, r) -> r.e = max) live
        in
        let live = Storage.remove max live |> Storage.add (reg_with_max_range, range) in
        let mapping =
          Base.Map.set mapping ~key:max_range.var ~data:(StackLoc (spilled_count + 1))
        in
        let mapping =
          Base.Map.set mapping ~key:range.var ~data:(Reg reg_with_max_range)
        in
        available, live, spilled_count + 1, mapping))
  ;;

  let scan_fun available vars =
    let live = Storage.empty in
    let empty = Base.Map.empty (module Base.String) in
    let _, _, _, res =
      List.fold_left
        (fun (av, live, spilled_count, mapping) range ->
          process_var av live range spilled_count mapping)
        (available, live, 0, empty)
        vars
    in
    res
  ;;

  let scan_program available flstructure : 'a program_allocation =
    let liveness = Liveness_analysis.analyse_program flstructure in
    List.map (fun (id, vars) -> id, scan_fun available vars) liveness
  ;;
end

let pp fmt pp_reg assignment =
  let open Format in
  let pp_value fmt = function
    | StackLoc i -> fprintf fmt "stacklock %i" i
    | Reg r -> pp_reg fmt r
  in
  let print_allocation fmt a =
    Base.Map.iteri a ~f:(fun ~key ~data -> fprintf fmt "@[%s: %a@]@." key pp_value data)
  in
  List.iter
    (fun (id, a) -> fprintf fmt "@[%s: @. %a@]@." id print_allocation a)
    assignment
;;
