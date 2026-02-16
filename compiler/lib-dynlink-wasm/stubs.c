#include <caml/mlvalues.h>
#include <caml/fail.h>

CAMLprim value caml_wasm_load_wasmo(value bytes) {
  caml_failwith("caml_wasm_load_wasmo: only available in wasm_of_ocaml");
}
