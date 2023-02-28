
//Provides: caml_custom_event_index
var caml_custom_event_index = 0;

//Provides: caml_runtime_events_user_register
//Requires: caml_custom_event_index
function caml_runtime_events_user_register(event_name, event_tag, event_type) {
  caml_custom_event_index += 1;
  return [0, caml_custom_event_index, event_name, event_type, event_tag];
}

//Provides: caml_runtime_events_user_write
function caml_runtime_events_user_write(event, event_content) {
  return 0;
}

//Provides: caml_runtime_events_user_resolve
function caml_runtime_events_user_resolve() {
  return 0;
}

//Provides: caml_runtime_events_start
function caml_runtime_events_start() {
  return 0;
}

//Provides: caml_runtime_events_pause
function caml_runtime_events_pause() {
  return 0;
}

//Provides: caml_runtime_events_resume
function caml_runtime_events_resume() {
  return 0;
}

//Provides: caml_runtime_events_create_cursor
function caml_runtime_events_create_cursor(target) {
  return {};
}

//Provides: caml_runtime_events_free_cursor
function caml_runtime_events_free_cursor(cursor) {
  return 0;
}

//Provides: caml_runtime_events_read_poll
function caml_runtime_events_read_poll(cursor, callbacks, num) {
  return 0;
}
