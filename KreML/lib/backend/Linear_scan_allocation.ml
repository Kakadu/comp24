open Liveness_analysis
open Active_registers_intf

type 'a assignment =
  | Reg of 'a
  | StackLoc of int

type 'a lsra_result = (string, 'a assignment, Base.String.comparator_witness) Base.Map.t

module Allocator (Storage : Active_registers_intf.S) = struct
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
    if Storage.size available = 0
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

  let scan available la_result =
    let live = Storage.empty in
    let empty = Base.Map.empty (module Base.String) in
    let _, _, res =
      List.fold_left
        (fun (av, live, mapping) range -> process_var av live range mapping)
        (available, live, empty)
        la_result
    in
    res
  ;;
end
