#include <stdlib.h>
#define D(f) void f () { exit(1); }

D(setTimeout)

D(caml_js_get)
D(caml_js_set)
D(caml_js_call)
D(caml_js_fun_call)
D(caml_js_meth_call)
D(caml_js_obj)
D(caml_js_array)
D(caml_js_var)

D(caml_string_from_js)
D(caml_string_to_js)
D(caml_js_null_value)
D(caml_js_http_get_with_status)
