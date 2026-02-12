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
