(** This is the javascript equivalent of Async_unix.Scheduler, ie a reimplementation of
    the async scheduler for javascript (at least the part of it that makes sense). *)

open Async_kernel.Std

(** [sleep d] is a deferred which becomes determined in [d] seconds. *)
val sleep : float -> unit Deferred.t

(** [yield ()] returns a deferred that becomes determined after the current cycle
    completes. *)
val yield : unit -> unit Deferred.t

(** Initialize the async scheduler *)
val init : unit -> unit
