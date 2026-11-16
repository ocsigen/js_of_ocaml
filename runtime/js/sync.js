// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2022 Hugo Heuzard
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, with linking exception;
// either version 2.1 of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

//Provides: MlMutex
class MlMutex {
  constructor() {
    this.locked = false;
  }
}

//Provides: caml_ml_mutex_new
//Requires: MlMutex
function caml_ml_mutex_new(_unit) {
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
