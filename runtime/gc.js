

//Provides: caml_gc_minor
function caml_gc_minor(){ return 0}
//Provides: caml_gc_major
function caml_gc_major(){ return 0}
//Provides: caml_gc_full_major
function caml_gc_full_major(){ return 0}
//Provides: caml_gc_compaction
function caml_gc_compaction(){ return 0}
//Provides: caml_gc_counters
function caml_gc_counters() { return [254,0,0,0] }
//Provides: caml_gc_quick_stat
function caml_gc_quick_stat(){
  return [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
}
//Provides: caml_gc_stat
function caml_gc_stat() {
  return [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
}

//Provides: caml_gc_set
function caml_gc_set(_control) {
  return 0;
}

//Provides: caml_gc_get
function caml_gc_get(){
  return [0,0,0,0,0,0,0,0,0]
}

//Provides: caml_memprof_set
function caml_memprof_set(_control) {
  return 0;
}

//Provides: caml_final_register const
function caml_final_register () { return 0; }
//Provides: caml_final_register_called_without_value const
function caml_final_register_called_without_value () { return 0; }
//Provides: caml_final_release const
function caml_final_release () { return 0; }

//Provides: caml_memprof_start
function caml_memprof_start(rate,stack_size,tracker){
  return 0;
}

//Provides: caml_memprof_stop
function caml_memprof_stop(unit) {
  return 0;
}

//Provides: caml_eventlog_resume
function caml_eventlog_resume(unit) { return 0; }

//Provides: caml_eventlog_pause
function caml_eventlog_pause(unit) { return 0; }

//Provides: caml_gc_huge_fallback_count
function caml_gc_huge_fallback_count(unit) { return 0; }

//Provides: caml_gc_major_slice
function caml_gc_major_slice(work) { return 0; }

//Provides: caml_gc_minor_words
function caml_gc_minor_words(unit) { return 0; }

//Provides: caml_get_minor_free
function caml_get_minor_free(unit) { return 0; }

//Provides: caml_get_major_bucket
function caml_get_major_bucket(n) { return 0; }

//Provides: caml_get_major_credit
function caml_get_major_credit(n) { return 0; }
