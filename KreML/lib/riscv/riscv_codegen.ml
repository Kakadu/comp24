open Riscv
open Flambda
open Linear_scan_allocation

type block = instruction list
type fun_body = block list
type generated_code = (string, fun_body) Hashtbl.t

let curr_block = ref []
let curr_assignment = ref (Base.Map.empty (module Base.String))

let normalize_binop op x y =
  match op with
  | Anf.Geq
let codegen_binop op x y =
  match x, y with
  | Fl_var x, Fl_var y ->
    let xloc = Base.Map.find_exn !curr_assignment x in
    let yloc = Base.Map.find_exn !curr_assignment y in
    let xreg, yreg, restore =
     match xloc, yloc with
    | Reg xr, Reg yr -> xr, yr, []
    | StackLoc x_idx, Reg yr ->
      temp 0, yr, []
    | Reg xr, StackLoc y_idx ->
      xr, temp 0, []
     | StackLoc x_idx, StackLoc y_idx ->
      temp 0, temp 1, [] in
     let op = match op with
     | Anf.Plus -> ADD
     | Anf.Mul -> MUL
     | Anf.Div -> DIV
     | Anf.Minus -> SUB
     | Anf.Lt -> SLT
     | Anf.Leq

     

let codegen program =
  "1"