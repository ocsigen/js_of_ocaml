open! Import

class type performanceObserverInit =
  object
    method entryTypes : Js.js_string Js.t Js.js_array Js.t Js.writeonly_prop
  end

class type performanceEntry =
  object
    method name : Js.js_string Js.t Js.readonly_prop

    method entryType : Js.js_string Js.t Js.readonly_prop

    method startTime : float Js.readonly_prop

    method duration : float Js.readonly_prop
  end

class type performanceObserverEntryList =
  object
    method getEntries : performanceEntry Js.t Js.js_array Js.t Js.meth
  end

class type performanceObserver =
  object
    method observe : performanceObserverInit Js.t -> unit Js.meth

    method disconnect : unit Js.meth

    method takeRecords : performanceEntry Js.t Js.js_array Js.t Js.meth
  end

let performanceObserver = Js.Unsafe.global##._PerformanceObserver

let is_supported () = Js.Optdef.test performanceObserver

let performanceObserver :
    (   (performanceObserverEntryList Js.t -> performanceObserver Js.t -> unit) Js.callback
     -> performanceObserver Js.t)
    Js.constr =
  performanceObserver

let observe ~entry_types ~f =
  let entry_types = entry_types |> List.map Js.string |> Array.of_list |> Js.array in
  let performance_observer_init : performanceObserverInit Js.t = Js.Unsafe.obj [||] in
  let () = performance_observer_init##.entryTypes := entry_types in
  let obs = new%js performanceObserver (Js.wrap_callback f) in
  let () = obs##observe performance_observer_init in
  obs
