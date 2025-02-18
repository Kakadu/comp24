open Riscv


type 'a t = 'a list
let remove reg regs = List.filter (( <> ) reg) regs
let add reg regs = reg :: regs
let size regs = List.length regs
let pop regs =
  match regs with
  | x::xs -> x, xs
  | _ -> Utils.internalfail "Pop from empty registers storage"
let remove_at regs idx = List.filteri (fun i _ -> i <> idx) regs
let element_at regs idx = List.filteri (fun i _ -> i = idx) regs |> List.hd
let with_ranges regs ranges = List.combine regs ranges
let fold = List.fold_left
let empty = []
let available = List.init 7 (fun i -> Temp i)

