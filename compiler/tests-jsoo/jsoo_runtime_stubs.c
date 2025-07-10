
#include <caml/mlvalues.h>

#define UNUSED(x) (void)(x)

CAMLprim value caml_jsoo_flags_use_js_string(value v_unit)
{
  UNUSED(v_unit);
  return Val_false;
}
