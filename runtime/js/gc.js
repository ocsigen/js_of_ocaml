//Provides: caml_gc_minor
export function caml_gc_minor(_unit) {
  //available with [node --expose-gc]
  if (typeof globalThis.gc === "function") globalThis.gc(true);
  return 0;
}
//Provides: caml_gc_major
export function caml_gc_major(_unit) {
  //available with [node --expose-gc]
  if (typeof globalThis.gc === "function") globalThis.gc();
  return 0;
}
//Provides: caml_gc_full_major
export function caml_gc_full_major(_unit) {
  //available with [node --expose-gc]
  if (typeof globalThis.gc === "function") globalThis.gc();
  return 0;
}
//Provides: caml_gc_compaction
export function caml_gc_compaction(_unit) {
  return 0;
}
//Provides: caml_gc_counters
export function caml_gc_counters(_unit) {
  return [254, 0, 0, 0];
}
//Provides: caml_gc_quick_stat
export function caml_gc_quick_stat(_unit) {
  return [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
}

//Provides: caml_gc_stat
export function caml_gc_stat(unit) {
  return caml_gc_quick_stat(unit);
}

//Provides: caml_gc_set
export function caml_gc_set(_control) {
  return 0;
}

//Provides: caml_gc_get
export function caml_gc_get(_unit) {
  return [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
}

//Provides: caml_final_register const
export function caml_final_register(_f, _x) {
  return 0;
}

//Provides: caml_final_register_called_without_value
var all_finalizers = new globalThis.Set();
export function caml_final_register_called_without_value(cb, a) {
  if (globalThis.FinalizationRegistry && a instanceof Object) {
    var x = new globalThis.FinalizationRegistry(function (x) {
      all_finalizers.delete(x);
      cb(0);
      return;
    });
    x.register(a, x);
    all_finalizers.add(x);
  }
  return 0;
}

//Provides: caml_final_release const
export function caml_final_release(_unit) {
  return 0;
}

//Provides: caml_memprof_start
export function caml_memprof_start(_rate, _stack_size, _tracker) {
  return 0;
}

//Provides: caml_memprof_stop
export function caml_memprof_stop(_unit) {
  return 0;
}

//Provides: caml_memprof_discard
//Version: >= 5.2
export function caml_memprof_discard(_t) {
  return 0;
}

//Provides: caml_eventlog_resume
//Version: < 5.0
export function caml_eventlog_resume(_unit) {
  return 0;
}

//Provides: caml_eventlog_pause
//Version: < 5.0
export function caml_eventlog_pause(_unit) {
  return 0;
}

//Provides: caml_gc_huge_fallback_count
//Version: < 5.0
export function caml_gc_huge_fallback_count(_unit) {
  return 0;
}

//Provides: caml_gc_major_slice
export function caml_gc_major_slice(_work) {
  return 0;
}

//Provides: caml_gc_minor_words
export function caml_gc_minor_words(_unit) {
  return 0;
}

//Provides: caml_get_minor_free
export function caml_get_minor_free(_unit) {
  return 0;
}

//Provides: caml_get_major_bucket
//Version: < 5.0
export function caml_get_major_bucket(_n) {
  return 0;
}

//Provides: caml_get_major_credit
//Version: < 5.0
export function caml_get_major_credit(_n) {
  return 0;
}
