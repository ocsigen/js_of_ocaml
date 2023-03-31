module Make (_ : Wa_target_sig.S) : sig
  val f : context:Wa_code_generation.context -> unit
end
