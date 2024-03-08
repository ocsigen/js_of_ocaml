val init : unit -> unit

val f :
     out_channel
  -> Code.program
  -> live_vars:int array
  -> in_cps:Effects.in_cps
  -> debug:Parse_bytecode.Debug.t
  -> string list * (string * Javascript.expression) list
