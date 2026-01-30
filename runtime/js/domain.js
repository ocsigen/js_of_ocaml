import { caml_callback } from './jslib.js';
import { caml_ml_mutex_unlock } from './sync.js';

//Provides: caml_domain_dls
//Version: >= 5
export let caml_domain_dls = [0];

//Provides: caml_domain_dls_set
//Version: >= 5
export function caml_domain_dls_set(a) {
  caml_domain_dls = a;
}

//Provides: caml_domain_dls_compare_and_set
//Version: >= 5.2
export function caml_domain_dls_compare_and_set(old, n) {
  if (caml_domain_dls !== old) return 0;
  caml_domain_dls = n;
  return 1;
}

//Provides: caml_domain_dls_get
//Version: >= 5
export function caml_domain_dls_get(_unit) {
  return caml_domain_dls;
}

//Provides: caml_atomic_load
//Version: >= 5
export function caml_atomic_load(ref) {
  return ref[1];
}

//Provides: caml_atomic_load_field
//Version: >= 5.4
export function caml_atomic_load_field(b, i) {
  return b[i + 1];
}

//Provides: caml_atomic_cas
//Version: >= 5
export function caml_atomic_cas(ref, o, n) {
  if (ref[1] === o) {
    ref[1] = n;
    return 1;
  }
  return 0;
}

//Provides: caml_atomic_cas_field
//Version: >= 5.4
export function caml_atomic_cas_field(b, i, o, n) {
  if (b[i + 1] === o) {
    b[i + 1] = n;
    return 1;
  }
  return 0;
}

//Provides: caml_atomic_fetch_add
//Version: >= 5
export function caml_atomic_fetch_add(ref, i) {
  var old = ref[1];
  ref[1] += i;
  return old;
}

//Provides: caml_atomic_fetch_add_field
//Version: >= 5.4
export function caml_atomic_fetch_add_field(b, i, n) {
  var old = b[i + 1];
  b[i + 1] += n;
  return old;
}

//Provides: caml_atomic_exchange
//Version: >= 5
export function caml_atomic_exchange(ref, v) {
  var r = ref[1];
  ref[1] = v;
  return r;
}

//Provides: caml_atomic_exchange_field
//Version: >= 5.4
export function caml_atomic_exchange_field(b, i, v) {
  var r = b[i + 1];
  b[i + 1] = v;
  return r;
}

//Provides: caml_atomic_make_contended
//Version: >= 5.2
export function caml_atomic_make_contended(a) {
  return [0, a];
}

//Provides: caml_ml_domain_unique_token
//Version: >= 5.0, < 5.2
var caml_ml_domain_unique_token_ = [0];
export function caml_ml_domain_unique_token(_unit) {
  return caml_ml_domain_unique_token_;
}

//Provides: caml_recommended_domain_count
//Version: >= 5
export function caml_recommended_domain_count(_unit) {
  return 1;
}

//Provides: caml_ml_domain_index
//Version: >= 5.03
export function caml_ml_domain_index(_unit) {
  return caml_domain_id;
}

//Provides: caml_domain_id
//Version: >= 5
export let caml_domain_id = 0;

//Provides: caml_domain_spawn
//Version: >= 5.2
var caml_domain_latest_idx = 1;
export function caml_domain_spawn(f, term_sync) {
  var id = caml_domain_latest_idx++;
  var old = caml_domain_id;
  caml_domain_id = id;
  var res = caml_callback(f, [0]);
  caml_domain_id = old;
  caml_ml_mutex_unlock(term_sync[2]);
  //TODO: fix exn case
  term_sync[1] = [0, [0, res]];
  return id;
}

//Provides: caml_domain_spawn
//Version: >= 5.0, < 5.2
var caml_domain_latest_idx = 1;
export function caml_domain_spawn(f, mutex) {
  var id = caml_domain_latest_idx++;
  var old = caml_domain_id;
  caml_domain_id = id;
  var _res = caml_callback(f, [0]);
  caml_domain_id = old;
  caml_ml_mutex_unlock(mutex);
  return id;
}

//Provides: caml_ml_domain_id
//Version: >= 5.0
export function caml_ml_domain_id(_unit) {
  return caml_domain_id;
}

//Provides: caml_ml_domain_cpu_relax
//Version: >= 5
export function caml_ml_domain_cpu_relax(_unit) {
  return 0;
}
