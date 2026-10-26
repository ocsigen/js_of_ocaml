#include <caml/mlvalues.h>
#include <caml/alloc.h>

/* Only called under js_of_ocaml / wasm_of_ocaml, which provide the
   primitive. */
CAMLprim value caml_jsoo_flags_effects(value unit) {
  return caml_copy_string("native");
}
