#include <stdlib.h>
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
void wasm_toplevel_init_compile () {
  fprintf(stderr, "Unimplemented Wasm primitive wasm_toplevel_init_compile!\n");
  exit(1);
}
void wasm_dynlink_init_sections () {
  fprintf(stderr, "Unimplemented Wasm primitive wasm_dynlink_init_sections!\n");
  exit(1);
}
CAMLprim value wasm_get_named_global(value name) {
  caml_failwith("wasm_get_named_global: not available in bytecode mode");
}
CAMLprim value wasm_get_ocaml_unit_list(value unit) {
  caml_failwith("wasm_get_ocaml_unit_list: not available in bytecode mode");
}
CAMLprim value wasm_get_prim_list(value unit) {
  caml_failwith("wasm_get_prim_list: not available in bytecode mode");
}
void caml_register_global(value idx, value data, value name) {
  /* no-op in bytecode mode; only meaningful in Wasm */
}
CAMLprim value caml_wasm_load_module(value bytes) {
  caml_failwith("caml_wasm_load_module: only available in wasm_of_ocaml");
}
CAMLprim value caml_wasm_load_wasmo(value bytes) {
  caml_failwith("caml_wasm_load_wasmo: only available in wasm_of_ocaml");
}
CAMLprim value caml_wasm_register_fragments(value unit_name, value source) {
  caml_failwith("caml_wasm_register_fragments: only available in wasm_of_ocaml");
}
