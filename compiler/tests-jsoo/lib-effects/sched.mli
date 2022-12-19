(* Control operations on threads *)
val fork : (unit -> unit) -> unit

val yield : unit -> unit

(* Runs the scheduler. *)
val run : (unit -> unit) -> unit
