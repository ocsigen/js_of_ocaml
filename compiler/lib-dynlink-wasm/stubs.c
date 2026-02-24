#include <caml/mlvalues.h>
#include <caml/fail.h>

CAMLprim value wasm_toplevel_init_compile(value f) {
  caml_failwith("wasm_toplevel_init_compile: only available in wasm_of_ocaml");
}
CAMLprim value wasm_dynlink_init_sections(value sections) {
  caml_failwith("wasm_dynlink_init_sections: only available in wasm_of_ocaml");
}
CAMLprim value wasm_get_named_global(value name) {
  caml_failwith("wasm_get_named_global: only available in wasm_of_ocaml");
}
CAMLprim value wasm_get_ocaml_unit_list(value unit) {
  caml_failwith("wasm_get_ocaml_unit_list: only available in wasm_of_ocaml");
}
CAMLprim value wasm_get_prim_list(value unit) {
  caml_failwith("wasm_get_prim_list: only available in wasm_of_ocaml");
}
CAMLprim value wasm_get_crcs(value unit) {
  caml_failwith("wasm_get_crcs: only available in wasm_of_ocaml");
}
CAMLprim value caml_register_global(value idx, value data, value name) {
  caml_failwith("caml_register_global: only available in wasm_of_ocaml");
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
