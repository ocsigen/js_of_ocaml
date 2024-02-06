
#include <stdio.h>
#include "caml/mlvalues.h"
#include "caml/memory.h"

CAMLprim value flush_stdout_stderr (value unit) {
  (void)unit;
  CAMLparam0 ();   /* v is ignored */
  fflush(stderr);
  fflush(stdout);
  CAMLreturn (Val_unit);
}
