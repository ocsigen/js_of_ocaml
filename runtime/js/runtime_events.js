// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2023 Lucas Pluvinage
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

//Provides: caml_custom_event_index
//Version: >= 5.1
var caml_custom_event_index = 0;

//Provides: caml_runtime_events_user_register
//Requires: caml_custom_event_index
//Version: >= 5.1
function caml_runtime_events_user_register(event_name, event_tag, event_type) {
  caml_custom_event_index += 1;
  return [0, caml_custom_event_index, event_name, event_type, event_tag];
}

//Provides: caml_runtime_events_user_write
//Version: >= 5.1
function caml_runtime_events_user_write(_event, _event_content) {
  return 0;
}

//Provides: caml_runtime_events_user_resolve
//Version: >= 5.0
function caml_runtime_events_user_resolve() {
  return 0;
}

//Provides: caml_ml_runtime_events_start
//Version: >= 5.2
function caml_ml_runtime_events_start() {
  return 0;
}

//Provides: caml_runtime_events_start
//Version: >= 5.0, < 5.2
function caml_runtime_events_start() {
  return 0;
}

//Provides: caml_ml_runtime_events_pause
//Version: >= 5.2
function caml_ml_runtime_events_pause() {
  return 0;
}

//Provides: caml_runtime_events_pause
//Version: >= 5.0, < 5.2
function caml_runtime_events_pause() {
  return 0;
}

//Provides: caml_ml_runtime_events_are_active
//Version: >= 5.2
function caml_ml_runtime_events_are_active() {
  return 0;
}

//Provides: caml_runtime_events_resume
//Version: >=5.0, < 5.2
function caml_runtime_events_resume() {
  return 0;
}

//Provides: caml_ml_runtime_events_resume
//Version: >= 5.2
function caml_ml_runtime_events_resume() {
  return 0;
}

//Provides: caml_ml_runtime_events_create_cursor
//Version: >= 5.0
function caml_ml_runtime_events_create_cursor(_target) {
  return {};
}

//Provides: caml_ml_runtime_events_free_cursor
//Version: >= 5.0
function caml_ml_runtime_events_free_cursor(_cursor) {
  return 0;
}

//Provides: caml_ml_runtime_events_read_poll
//Version: >= 5.0
function caml_ml_runtime_events_read_poll(_cursor, _callbacks, _num) {
  return 0;
}

//Provides: caml_ml_runtime_events_path const
//Version: >= 5.2
//Since 5.3 in OCaml mainstream, but also available in OxCaml
function caml_ml_runtime_events_path(_unit) {
  return 0;
}
