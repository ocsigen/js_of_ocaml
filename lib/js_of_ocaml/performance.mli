(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** Performance API

    A code example:
    {[
      let perf = Performance.performance in
      perf##mark (Js.string "start");
      do_something ();
      perf##mark (Js.string "end");
      let _ = perf##measure (Js.string "elapsed") (Js.string "start") (Js.string "end") in
      let entries = perf##getEntriesByType (Js.string "measure") in
      Js.array_get entries 0 |> ignore
    ]}

    @see <https://developer.mozilla.org/en-US/docs/Web/API/Performance> for API documentation.
*)

class type performanceEntry = object
  inherit PerformanceObserver.performanceEntry
end

class type ['detail] performanceMark = object
  inherit performanceEntry

  method detail : 'detail Js.opt Js.readonly_prop
end

class type ['detail] performanceMeasure = object
  inherit performanceEntry

  method detail : 'detail Js.opt Js.readonly_prop
end

class type ['detail] performanceMarkOptions = object
  method detail : 'detail Js.writeonly_prop

  method startTime : Js.number_t Js.writeonly_prop
end

class type ['detail] performanceMeasureOptions = object
  method detail : 'detail Js.writeonly_prop

  method start : Js.js_string Js.t Js.writeonly_prop
  (** Start as a mark name. Maps to the JavaScript [start] field. *)

  method start_time : Js.number_t Js.writeonly_prop
  (** Start as a timestamp. Maps to the JavaScript [start] field. *)

  method _end : Js.js_string Js.t Js.writeonly_prop
  (** End as a mark name. Maps to the JavaScript [end] field. *)

  method end_time : Js.number_t Js.writeonly_prop
  (** End as a timestamp. Maps to the JavaScript [end] field. *)

  method duration : Js.number_t Js.writeonly_prop
end

class type performance = object
  method timeOrigin : Js.number_t Js.readonly_prop

  method now : Js.number_t Js.meth

  method mark : 'a. Js.js_string Js.t -> 'a performanceMark Js.t Js.meth

  method mark_options :
    'a.
    Js.js_string Js.t -> 'a performanceMarkOptions Js.t -> 'a performanceMark Js.t Js.meth

  method measure :
    'a.
       Js.js_string Js.t
    -> Js.js_string Js.t Js.optdef
    -> Js.js_string Js.t Js.optdef
    -> 'a performanceMeasure Js.t Js.meth

  method measure_options :
    'a.
       Js.js_string Js.t
    -> 'a performanceMeasureOptions Js.t
    -> 'a performanceMeasure Js.t Js.meth

  method clearMarks : unit Js.meth

  method clearMarks_named : Js.js_string Js.t -> unit Js.meth

  method clearMeasures : unit Js.meth

  method clearMeasures_named : Js.js_string Js.t -> unit Js.meth

  method clearResourceTimings : unit Js.meth

  method getEntries : performanceEntry Js.t Js.js_array Js.t Js.meth

  method getEntriesByName :
    Js.js_string Js.t -> performanceEntry Js.t Js.js_array Js.t Js.meth

  method getEntriesByName_type :
       Js.js_string Js.t
    -> Js.js_string Js.t
    -> performanceEntry Js.t Js.js_array Js.t Js.meth

  method getEntriesByType :
    Js.js_string Js.t -> performanceEntry Js.t Js.js_array Js.t Js.meth

  method setResourceTimingBufferSize : int -> unit Js.meth
end

val performance : performance Js.t
(** [Window.performance], the global [Performance] object. *)

val is_supported : unit -> bool
(** Whether the global [performance] object is defined. *)

val mark :
     ?detail:'a
  -> ?startTime:Js.number_t
  -> performance Js.t
  -> Js.js_string Js.t
  -> 'a performanceMark Js.t
(** Wrapper for [Performance.mark(name, options?)] taking labeled arguments. *)

val makeMeasureOptions :
     ?detail:'a
  -> ?start:Js.js_string Js.t
  -> ?start_time:Js.number_t
  -> ?_end:Js.js_string Js.t
  -> ?end_time:Js.number_t
  -> ?duration:Js.number_t
  -> unit
  -> 'a performanceMeasureOptions Js.t
(** Smart constructor for {!performanceMeasureOptions}.

    Raises [Invalid_argument] if both [?start] and [?start_time], or
    both [?_end] and [?end_time], are provided — they map to the same
    JavaScript field. *)
