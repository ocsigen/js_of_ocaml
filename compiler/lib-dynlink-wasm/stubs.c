#include <caml/mlvalues.h>
#include <caml/fail.h>

CAMLprim value wasm_toplevel_init_compile(value f) {
  caml_failwith("wasm_toplevel_init_compile: only available in wasm_of_ocaml");
}
CAMLprim value wasm_get_bytecode_sections(value unit) {
  caml_failwith("wasm_get_bytecode_sections: only available in wasm_of_ocaml");
}
CAMLprim value wasm_toplevel_init_reloc(value f) {
  caml_failwith("wasm_toplevel_init_reloc: only available in wasm_of_ocaml");
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
CAMLprim value wasm_get_runtime_aliases(value unit) {
  caml_failwith("wasm_get_runtime_aliases: only available in wasm_of_ocaml");
}
