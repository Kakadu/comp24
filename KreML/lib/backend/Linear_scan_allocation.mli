open Liveness_analysis

type 'a assignment =
  | Reg of 'a
  | StackLoc of int

type 'a lsra_result = (string, 'a assignment, Base.String.comparator_witness) Base.Map.t

module Allocator(Storage: Active_registers_intf.S) : sig
  val scan : range list -> 'a lsra_result
end