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
function caml_runtime_events_user_write(event, event_content) {
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

//Provides: caml_runtime_events_create_cursor
//Version: >= 5.0
function caml_runtime_events_create_cursor(target) {
  return {};
}

//Provides: caml_runtime_events_free_cursor
//Version: >= 5.0
function caml_runtime_events_free_cursor(cursor) {
  return 0;
}

//Provides: caml_runtime_events_read_poll
//Version: >= 5.0
function caml_runtime_events_read_poll(cursor, callbacks, num) {
  return 0;
}

//Provides: caml_ml_runtime_events_path const
//Version: >= 5.3
function caml_ml_runtime_events_path(_unit) {
  return 0;
}
