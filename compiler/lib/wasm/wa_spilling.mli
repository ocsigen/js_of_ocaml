(*
type stack = Code.Var.t option list

type spilling_info =
  { reloads : (Code.Var.t * int) list
  ; depth_change : int
  ; spills : (Code.Var.t * int) list
  ; stack : stack
  }

type block_info =
  { initial_depth : int
  ; loaded_variables : Code.Var.Set.t
  ; spilling : spilling_info
  }

type info =
  { max_depth : int
  ; subcalls : bool
  ; initial_spilling : spilling_info
  ; block : block_info Code.Addr.Map.t
  ; instr : spilling_info Code.Var.Map.t
  }
*)

type stack = Code.Var.t option list

type info

val generate_spilling_information :
     Code.program
  -> context:Wa_code_generation.context
  -> closures:Wa_closure_conversion.closure Code.Var.Map.t
  -> pc:Code.Addr.t
  -> env:Code.Var.t
  -> params:Code.Var.t list
  -> info

val make_info : unit -> info

val add_spilling :
     info
  -> location:Code.Var.t
  -> stack:stack
  -> live_vars:Code.Var.Set.t
  -> spilled_vars:Code.Var.Set.t
  -> info * stack

type ctx

val start_function : context:Wa_code_generation.context -> info -> ctx

val start_block : context:Wa_code_generation.context -> info -> Code.Addr.t -> ctx

val perform_reloads :
     ctx
  -> [ `Branch of Code.last | `Instr of Code.instr | `Vars of Code.Var.Set.t ]
  -> unit Wa_code_generation.t

val perform_spilling :
     ctx
  -> [ `Function | `Instr of Code.Var.t | `Block of Code.Addr.t ]
  -> unit Wa_code_generation.t

val kill_variables : ctx -> unit

val assign : ctx -> Code.Var.t -> unit Wa_code_generation.t

val adjust_stack : ctx -> src:Code.Addr.t -> dst:Code.Addr.t -> unit Wa_code_generation.t

val stack_adjustment_needed : ctx -> src:Code.Addr.t -> dst:Code.Addr.t -> bool
