import { caml_failwith } from './fail.js';

//Provides: MlMutex
export class MlMutex {
  constructor() {
    this.locked = false;
  }
}

//Provides: caml_ml_mutex_new
export function caml_ml_mutex_new(_unit) {
  return new MlMutex();
}

//Provides: caml_ml_mutex_lock
export function caml_ml_mutex_lock(t) {
  if (t.locked) caml_failwith("Mutex.lock: mutex already locked. Cannot wait.");
  else t.locked = true;
  return 0;
}

//Provides: caml_ml_mutex_try_lock
export function caml_ml_mutex_try_lock(t) {
  if (!t.locked) {
    t.locked = true;
    return 1;
  }
  return 0;
}

//Provides: caml_ml_mutex_unlock
export function caml_ml_mutex_unlock(t) {
  t.locked = false;
  return 0;
}
