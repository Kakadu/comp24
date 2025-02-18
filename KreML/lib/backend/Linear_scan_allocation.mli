open Liveness_analysis

type 'a assignment =
  | Reg of 'a
  | StackLoc of int

type 'a fun_allocation = (string, 'a assignment, Base.String.comparator_witness) Base.Map.t
type 'a program_allocation = (string * 'a fun_allocation) list

module Allocator(S : Registers_storage_intf.S) : sig
  val scan_fun : 'a S.t -> range list -> 'a fun_allocation
  val scan_program : 'a S.t -> Liveness_analysis.analysis_result -> 'a program_allocation
end

val pp : Format.formatter -> (Format.formatter -> 'a -> unit) -> 'a program_allocation -> unit