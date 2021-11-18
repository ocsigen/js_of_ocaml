(** PerformanceObserver API

    A code example:
    {[
      if (PerformanceObserver.is_supported()) then
        let entry_types = [ "measure" ] in
        let f entries observer =
          let entries = entries##getEntries in
          Firebug.console##debug entries ;
          Firebug.console##debug observer
        in
        PerformanceObserver.observe ~entry_types ~f
        ()
   ]}

   @see <https://developer.mozilla.org/en-US/docs/Web/API/PerformanceObserver> for API documentation.
*)

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

val performanceObserver :
  (   (performanceObserverEntryList Js.t -> performanceObserver Js.t -> unit) Js.callback
   -> performanceObserver Js.t)
  Js.constr

val is_supported : unit -> bool

val observe :
     entry_types:string list
  -> f:(performanceObserverEntryList Js.t -> performanceObserver Js.t -> unit)
  -> performanceObserver Js.t
