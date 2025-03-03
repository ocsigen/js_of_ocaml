//Provides: MlMutex
class MlMutex {
  constructor() {
    this.locked = false;
  }
}

//Provides: caml_ml_mutex_new
//Requires: MlMutex
function caml_ml_mutex_new(unit) {
  return new MlMutex();
}

//Provides: caml_ml_mutex_lock
//Requires: caml_failwith
function caml_ml_mutex_lock(t) {
  if (t.locked) caml_failwith("Mutex.lock: mutex already locked. Cannot wait.");
  else t.locked = true;
  return 0;
}

//Provides: caml_ml_mutex_try_lock
function caml_ml_mutex_try_lock(t) {
  if (!t.locked) {
    t.locked = true;
    return 1;
  }
  return 0;
}

//Provides: caml_ml_mutex_unlock
function caml_ml_mutex_unlock(t) {
  t.locked = false;
  return 0;
}
