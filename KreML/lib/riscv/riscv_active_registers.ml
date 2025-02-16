open Riscv


type 'a t = 'a list
let remove regs reg = List.filter ((<>) reg) regs
let add regs reg = reg::regs
let size regs = List.length regs
let from_temps count =
  List.init count (fun i -> Temp i) 