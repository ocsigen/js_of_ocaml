let caml_custom_event_index = 0;

export function caml_runtime_events_user_register(event_name, event_tag, event_type) {
  caml_custom_event_index += 1;
  return [0, caml_custom_event_index, event_name, event_type, event_tag];
}

export function caml_runtime_events_user_write(event, event_content) {
  return 0;
}

export function caml_runtime_events_user_resolve() {
  return 0;
}

export function caml_runtime_events_start() {
  return 0;
}

export function caml_runtime_events_pause() {
  return 0;
}

export function caml_runtime_events_resume() {
  return 0;
}

export function caml_runtime_events_create_cursor(target) {
  return {};
}

export function caml_runtime_events_free_cursor(cursor) {
  return 0;
}

export function caml_runtime_events_read_poll(cursor, callbacks, num) {
  return 0;
}
