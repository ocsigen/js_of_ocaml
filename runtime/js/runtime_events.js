//Provides: caml_custom_event_index
//Version: >= 5.1
export var caml_custom_event_index$v5_1_plus = 0;

//Provides: caml_runtime_events_user_register
//Version: >= 5.1
export function caml_runtime_events_user_register$v5_1_plus(event_name, event_tag, event_type) {
  caml_custom_event_index += 1;
  return [0, caml_custom_event_index, event_name, event_type, event_tag];
}

//Provides: caml_runtime_events_user_write
//Version: >= 5.1
export function caml_runtime_events_user_write$v5_1_plus(_event, _event_content) {
  return 0;
}

//Provides: caml_runtime_events_user_resolve
//Version: >= 5.0
export function caml_runtime_events_user_resolve$v5_0_plus() {
  return 0;
}

//Provides: caml_ml_runtime_events_start
//Version: >= 5.2
export function caml_ml_runtime_events_start$v5_2_plus() {
  return 0;
}

//Provides: caml_runtime_events_start
//Version: >= 5.0, < 5.2
export function caml_runtime_events_start$v5_0_plus() {
  return 0;
}

//Provides: caml_ml_runtime_events_pause
//Version: >= 5.2
export function caml_ml_runtime_events_pause$v5_2_plus() {
  return 0;
}

//Provides: caml_runtime_events_pause
//Version: >= 5.0, < 5.2
export function caml_runtime_events_pause$v5_0_plus() {
  return 0;
}

//Provides: caml_ml_runtime_events_are_active
//Version: >= 5.2
export function caml_ml_runtime_events_are_active$v5_2_plus() {
  return 0;
}

//Provides: caml_runtime_events_resume
//Version: >=5.0, < 5.2
export function caml_runtime_events_resume$v5_0_plus() {
  return 0;
}

//Provides: caml_ml_runtime_events_resume
//Version: >= 5.2
export function caml_ml_runtime_events_resume$v5_2_plus() {
  return 0;
}

//Provides: caml_runtime_events_create_cursor
//Version: >= 5.0
export function caml_runtime_events_create_cursor$v5_0_plus(_target) {
  return {};
}

//Provides: caml_runtime_events_free_cursor
//Version: >= 5.0
export function caml_runtime_events_free_cursor$v5_0_plus(_cursor) {
  return 0;
}

//Provides: caml_runtime_events_read_poll
//Version: >= 5.0
export function caml_runtime_events_read_poll$v5_0_plus(_cursor, _callbacks, _num) {
  return 0;
}

//Provides: caml_ml_runtime_events_path const
//Version: >= 5.3
export function caml_ml_runtime_events_path$v5_3_plus(_unit) {
  return 0;
}
