//Provides: caml_domain_dls
var caml_domain_dls = [0];

//Provides: caml_domain_dls_set
//Requires: caml_domain_dls
function caml_domain_dls_set(a) {
  caml_domain_dls = a;
}

//Provides: caml_domain_dls_get
//Requires: caml_domain_dls
function caml_domain_dls_get(unit) {
  return caml_domain_dls;
}


//Provides: caml_atomic_load
function caml_atomic_load(ref){
  return ref[1];
}

//Provides: caml_atomic_cas
function caml_atomic_cas(ref,o,n) {
  if(ref[1] === o){
    ref[1] = n;
    return 1;
  }
  return 0;
}

//Provides: caml_atomic_fetch_add
function caml_atomic_fetch_add(ref, i) {
  var old = ref[1];
  ref[1] += i;
  return old;
}

//Provides: caml_atomic_exchange
function caml_atomic_exchange(ref, v) {
  var r = ref[1];
  ref[1] = v;
  return r;
}

//Provides: caml_atomic_make_contended
function caml_atomic_make_contended(a) {
  return [0, a]
}

//Provides: caml_ml_domain_unique_token
var caml_ml_domain_unique_token_ = [0]
function caml_ml_domain_unique_token(unit) {
  return caml_ml_domain_unique_token_
}


//Provides: caml_ml_domain_set_name
function caml_ml_domain_set_name(_name) {
  return 0;
}

//Provides: caml_recommended_domain_count
function caml_recommended_domain_count(unit) { return 1 }


//Provides: caml_domain_id
var caml_domain_id = 0;

//Provides: caml_domain_spawn
//Requires: caml_ml_mutex_unlock
//Requires: caml_domain_id
//Requires: caml_callback
//Version: >= 5.2
var caml_domain_latest_idx = 1
function caml_domain_spawn(f,term_sync){
    var id = caml_domain_latest_idx++;
    var old = caml_domain_id;
    caml_domain_id = id;
    var res = caml_callback(f,[0]);
    caml_domain_id = old;
    caml_ml_mutex_unlock(term_sync[2]);
    //TODO: fix exn case
    term_sync[1] = [0, [0, res]];
    return id;
}

//Provides: caml_domain_spawn
//Requires: caml_ml_mutex_unlock
//Requires: caml_domain_id
//Requires: caml_callback
//Version: < 5.2
var caml_domain_latest_idx = 1
function caml_domain_spawn(f,mutex){
    var id = caml_domain_latest_idx++;
    var old = caml_domain_id;
    caml_domain_id = id;
    var res = caml_callback(f,[0]);
    caml_domain_id = old;
    caml_ml_mutex_unlock(mutex);
    return id;
}


//Provides: caml_ml_domain_id
//Requires: caml_domain_id
function caml_ml_domain_id(unit){
    return caml_domain_id;
}


//Provides: caml_ml_domain_cpu_relax
function caml_ml_domain_cpu_relax(unit){
    return 0;
}
