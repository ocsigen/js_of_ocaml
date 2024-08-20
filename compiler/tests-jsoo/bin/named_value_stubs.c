#include "caml/mlvalues.h"

CAMLprim value caml_unregister_named_value(value nm) {
   return Val_unit;
}
