
//Provides: MlMutex
function MlMutex() {
  this.locked = false
}

//Provides: caml_ml_mutex_new
//Requires: MlMutex
function caml_ml_mutex_new(unit) {
  return new MlMutex();
}

//Provides: caml_ml_mutex_lock
//Requires: caml_failwith
function caml_ml_mutex_lock(t) {
  caml_failwith("Mutex.lock is not implemented");
}

//Provides: caml_ml_mutex_try_lock
function caml_ml_mutex_try_lock(t) {
  if(!t.locked) {
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
