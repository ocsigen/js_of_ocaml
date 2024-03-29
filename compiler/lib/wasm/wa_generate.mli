val init : unit -> unit

val start : unit -> Wa_code_generation.context

val f :
     context:Wa_code_generation.context
  -> unit_name:string option
  -> Code.program
  -> live_vars:int array
  -> in_cps:Effects.in_cps
  -> Wa_ast.var * (string list * (string * Javascript.expression) list)

val add_start_function : context:Wa_code_generation.context -> Wa_ast.var -> unit

val add_init_function : context:Wa_code_generation.context -> to_link:string list -> unit

val output :
     out_channel
  -> context:Wa_code_generation.context
  -> debug:Parse_bytecode.Debug.t
  -> unit
