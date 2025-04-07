//Provides: caml_gc_minor
function caml_gc_minor(unit) {
  //available with [node --expose-gc]
  if (typeof globalThis.gc === "function") globalThis.gc(true);
  return 0;
}
//Provides: caml_gc_major
function caml_gc_major(unit) {
  //available with [node --expose-gc]
  if (typeof globalThis.gc === "function") globalThis.gc();
  return 0;
}
//Provides: caml_gc_full_major
function caml_gc_full_major(unit) {
  //available with [node --expose-gc]
  if (typeof globalThis.gc === "function") globalThis.gc();
  return 0;
}
//Provides: caml_gc_compaction
function caml_gc_compaction() {
  return 0;
}
//Provides: caml_gc_counters
function caml_gc_counters() {
  return [254, 0, 0, 0];
}
//Provides: caml_gc_quick_stat
function caml_gc_quick_stat() {
  return [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
}

//Provides: caml_gc_stat
//Requires: caml_gc_quick_stat
function caml_gc_stat() {
  return caml_gc_quick_stat();
}

//Provides: caml_gc_set
function caml_gc_set(_control) {
  return 0;
}

//Provides: caml_gc_get
function caml_gc_get() {
  return [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
}

//Provides: caml_final_register const
function caml_final_register() {
  return 0;
}

//Provides: caml_final_register_called_without_value
var all_finalizers = new globalThis.Set();
function caml_final_register_called_without_value(cb, a) {
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
function caml_final_release() {
  return 0;
}

//Provides: caml_memprof_start
function caml_memprof_start(rate, stack_size, tracker) {
  return 0;
}

//Provides: caml_memprof_stop
function caml_memprof_stop(unit) {
  return 0;
}

//Provides: caml_memprof_discard
//Version: >= 5.2
function caml_memprof_discard(t) {
  return 0;
}

//Provides: caml_eventlog_resume
//Version: < 5.0
function caml_eventlog_resume(unit) {
  return 0;
}

//Provides: caml_eventlog_pause
//Version: < 5.0
function caml_eventlog_pause(unit) {
  return 0;
}

//Provides: caml_gc_huge_fallback_count
//Version: < 5.0
function caml_gc_huge_fallback_count(unit) {
  return 0;
}

//Provides: caml_gc_major_slice
function caml_gc_major_slice(work) {
  return 0;
}

//Provides: caml_gc_minor_words
function caml_gc_minor_words(unit) {
  return 0;
}

//Provides: caml_get_minor_free
function caml_get_minor_free(unit) {
  return 0;
}

//Provides: caml_get_major_bucket
//Version: < 5.0
function caml_get_major_bucket(n) {
  return 0;
}

//Provides: caml_get_major_credit
//Version: < 5.0
function caml_get_major_credit(n) {
  return 0;
}
