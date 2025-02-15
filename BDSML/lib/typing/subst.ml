open Utils
open Types
module VarMap = Map.Make (VarId)

type t = type_val VarMap.t

let empty = VarMap.empty
