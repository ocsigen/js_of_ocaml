//Provides: caml_domain_dls
//Version: >= 5
var caml_domain_dls = [0];

//Provides: caml_domain_dls_set
//Requires: caml_domain_dls
//Version: >= 5
function caml_domain_dls_set(a) {
  caml_domain_dls = a;
  return 0;
}

//Provides: caml_domain_dls_compare_and_set
//Requires: caml_domain_dls
//Version: >= 5.2
function caml_domain_dls_compare_and_set(old, n) {
  if (caml_domain_dls !== old) return 0;
  caml_domain_dls = n;
  return 1;
}

//Provides: caml_domain_dls_get
//Requires: caml_domain_dls
//Version: >= 5
function caml_domain_dls_get(_unit) {
  return caml_domain_dls;
}

//Provides: caml_domain_tls
//Version: >= 5.2, < 5.3
//OxCaml
var caml_domain_tls = [0];

//Provides: caml_domain_tls_set
//Requires: caml_domain_tls
//Version: >= 5.2, < 5.3
//OxCaml
function caml_domain_tls_set(a) {
  caml_domain_tls = a;
  return 0;
}

//Provides: caml_domain_tls_get
//Requires: caml_domain_tls
//Version: >= 5.2, < 5.3
//OxCaml
function caml_domain_tls_get(_unit) {
  return caml_domain_tls;
}

//Provides: caml_atomic_load
//Version: >= 5
function caml_atomic_load(ref) {
  return ref[1];
}

//Provides: caml_atomic_load_field
//Version: >= 5.4
function caml_atomic_load_field(b, i) {
  return b[i + 1];
}

//Provides: caml_atomic_cas
//Version: >= 5
function caml_atomic_cas(ref, o, n) {
  if (ref[1] === o) {
    ref[1] = n;
    return 1;
  }
  return 0;
}

//Provides: caml_atomic_cas_field
//Version: >= 5.4
function caml_atomic_cas_field(b, i, o, n) {
  if (b[i + 1] === o) {
    b[i + 1] = n;
    return 1;
  }
  return 0;
}

//Provides: caml_atomic_compare_exchange
//Version: >= 5.2, < 5.3
//OxCaml
function caml_atomic_compare_exchange(ref, o, n) {
  var old = ref[1];
  if (old === o) ref[1] = n;
  return old;
}

//Provides: caml_atomic_fetch_add
//Version: >= 5
function caml_atomic_fetch_add(ref, i) {
  var old = ref[1];
  ref[1] += i;
  return old;
}

//Provides: caml_atomic_fetch_add_field
//Version: >= 5.4
function caml_atomic_fetch_add_field(b, i, n) {
  var old = b[i + 1];
  b[i + 1] += n;
  return old;
}

//Provides: caml_atomic_add
//Version: >= 5.2, < 5.3
//OxCaml
function caml_atomic_add(ref, i) {
  ref[1] += i;
  return 0;
}

//Provides: caml_atomic_sub
//Version: >= 5.2, < 5.3
//OxCaml
function caml_atomic_sub(ref, i) {
  ref[1] -= i;
  return 0;
}

//Provides: caml_atomic_land
//Version: >= 5.2, < 5.3
//OxCaml
function caml_atomic_land(ref, i) {
  ref[1] &= i;
  return 0;
}

//Provides: caml_atomic_lor
//Version: >= 5.2, < 5.3
//OxCaml
function caml_atomic_lor(ref, i) {
  ref[1] |= i;
  return 0;
}

//Provides: caml_atomic_lxor
//Version: >= 5.2, < 5.3
//OxCaml
function caml_atomic_lxor(ref, i) {
  ref[1] ^= i;
  return 0;
}

//Provides: caml_atomic_exchange
//Version: >= 5
function caml_atomic_exchange(ref, v) {
  var r = ref[1];
  ref[1] = v;
  return r;
}

//Provides: caml_atomic_exchange_field
//Version: >= 5.4
function caml_atomic_exchange_field(b, i, v) {
  var r = b[i + 1];
  b[i + 1] = v;
  return r;
}

//Provides: caml_atomic_set
//Version: >= 5.2, < 5.3
//OxCaml
function caml_atomic_set(ref, v) {
  ref[1] = v;
  return 0;
}

//Provides: caml_atomic_make_contended
//Version: >= 5.2
function caml_atomic_make_contended(a) {
  return [0, a];
}

//Provides: caml_ml_domain_unique_token
//Version: >= 5.0, < 5.2
var caml_ml_domain_unique_token_ = [0];
function caml_ml_domain_unique_token(_unit) {
  return caml_ml_domain_unique_token_;
}

//Provides: caml_recommended_domain_count
//Version: >= 5
function caml_recommended_domain_count(_unit) {
  return 1;
}

//Provides: caml_ml_domain_index
//Requires: caml_domain_id
//Version: >= 5.03
function caml_ml_domain_index(_unit) {
  return caml_domain_id;
}

//Provides: caml_ml_domain_index
//Requires: caml_domain_id
//Version: >= 5.2, < 5.3
//OxCaml
function caml_ml_domain_index(_unit) {
  return caml_domain_id;
}

//Provides: caml_domain_id
//Version: >= 5
var caml_domain_id = 0;

//Provides: caml_domain_spawn
//Requires: caml_ml_mutex_unlock
//Requires: caml_domain_id
//Requires: caml_callback
//Version: >= 5.2
var caml_domain_latest_idx = 1;
function caml_domain_spawn(f, term_sync) {
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
//Requires: caml_ml_mutex_unlock
//Requires: caml_domain_id
//Requires: caml_callback
//Version: >= 5.0, < 5.2
var caml_domain_latest_idx = 1;
function caml_domain_spawn(f, mutex) {
  var id = caml_domain_latest_idx++;
  var old = caml_domain_id;
  caml_domain_id = id;
  var _res = caml_callback(f, [0]);
  caml_domain_id = old;
  caml_ml_mutex_unlock(mutex);
  return id;
}

//Provides: caml_ml_domain_id
//Requires: caml_domain_id
//Version: >= 5.0
function caml_ml_domain_id(_unit) {
  return caml_domain_id;
}

//Provides: caml_ml_domain_cpu_relax
//Version: >= 5
function caml_ml_domain_cpu_relax(_unit) {
  return 0;
}

//Provides: caml_atomic_load_field
//Version: >= 5.2, < 5.3
//OxCaml
function caml_atomic_load_field(ref, field) {
  return ref[field + 1];
}

//Provides: caml_atomic_add_field
//Version: >= 5.2, < 5.3
//OxCaml
function caml_atomic_add_field(ref, field, i) {
  ref[field + 1] += i;
  return 0;
}

//Provides: caml_atomic_fetch_add_field
//Version: >= 5.2, < 5.3
//OxCaml
function caml_atomic_fetch_add_field(ref, field, i) {
  var old = ref[field + 1];
  ref[field + 1] += i;
  return old;
}

//Provides: caml_atomic_cas_field
//Version: >= 5.2, < 5.3
//OxCaml
function caml_atomic_cas_field(ref, field, o, n) {
  if (ref[field + 1] === o) {
    ref[field + 1] = n;
    return 1;
  }
  return 0;
}

//Provides: caml_atomic_compare_exchange_field
//Version: >= 5.2, < 5.3
//OxCaml
function caml_atomic_compare_exchange_field(ref, field, o, n) {
  var old = ref[field + 1];
  if (old === o) {
    ref[field + 1] = n;
  }
  return old;
}

//Provides: caml_atomic_set_field
//Version: >= 5.2, < 5.3
//OxCaml
function caml_atomic_set_field(ref, field, v) {
  ref[field + 1] = v;
  return 0;
}

//Provides: caml_atomic_exchange_field
//Version: >= 5.2, < 5.3
//OxCaml
function caml_atomic_exchange_field(ref, field, v) {
  var old = ref[field + 1];
  ref[field + 1] = v;
  return old;
}

//Provides: caml_atomic_sub_field
//Version: >= 5.2, < 5.3
//OxCaml
function caml_atomic_sub_field(ref, field, i) {
  ref[field + 1] -= i;
  return 0;
}

//Provides: caml_atomic_land_field
//Version: >= 5.2, < 5.3
//OxCaml
function caml_atomic_land_field(ref, field, i) {
  ref[field + 1] &= i;
  return 0;
}

//Provides: caml_atomic_lor_field
//Version: >= 5.2, < 5.3
//OxCaml
function caml_atomic_lor_field(ref, field, i) {
  ref[field + 1] |= i;
  return 0;
}

//Provides: caml_atomic_lxor_field
//Version: >= 5.2, < 5.3
//OxCaml
function caml_atomic_lxor_field(ref, field, i) {
  ref[field + 1] ^= i;
  return 0;
}
