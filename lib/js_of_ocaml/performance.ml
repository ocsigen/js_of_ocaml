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

open! Import

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

  method start_time : Js.number_t Js.writeonly_prop

  method _end : Js.js_string Js.t Js.writeonly_prop

  method end_time : Js.number_t Js.writeonly_prop

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

let performance : performance Js.t = Js.Unsafe.global##.performance

let is_supported () = Js.Optdef.test Js.Unsafe.global##.performance

let mark ?detail ?startTime (perf : performance Js.t) name =
  let opts : _ performanceMarkOptions Js.t = Js.Unsafe.obj [||] in
  Option.iter (fun v -> opts##.detail := v) detail;
  Option.iter (fun v -> opts##.startTime := v) startTime;
  perf##mark_options name opts

let makeMeasureOptions ?detail ?start ?start_time ?_end ?end_time ?duration () =
  if Option.is_some start && Option.is_some start_time
  then invalid_arg "Performance.makeMeasureOptions: both ?start and ?start_time given";
  if Option.is_some _end && Option.is_some end_time
  then invalid_arg "Performance.makeMeasureOptions: both ?_end and ?end_time given";
  let opts : _ performanceMeasureOptions Js.t = Js.Unsafe.obj [||] in
  Option.iter (fun v -> opts##.detail := v) detail;
  Option.iter (fun v -> opts##.start := v) start;
  Option.iter (fun v -> opts##.start_time := v) start_time;
  Option.iter (fun v -> opts##._end := v) _end;
  Option.iter (fun v -> opts##.end_time := v) end_time;
  Option.iter (fun v -> opts##.duration := v) duration;
  opts
