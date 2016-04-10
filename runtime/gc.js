

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
