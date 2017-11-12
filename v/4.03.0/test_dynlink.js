(function(joo_global_object)
   {"use strict";
    var
     runtime=joo_global_object.jsoo_runtime,
     caml_new_string=runtime.caml_new_string;
    function caml_call1(f,a0)
     {return f.length == 1?f(a0):runtime.caml_call_gen(f,[a0])}
    var
     global_data=runtime.caml_get_global_data(),
     cst_Dynlink_OK=caml_new_string("Dynlink OK"),
     Pervasives=global_data.Pervasives;
    caml_call1(Pervasives[31],cst_Dynlink_OK);
    var Test_dynlink=[0];
    runtime.caml_register_global(2,Test_dynlink,"Test_dynlink");
    return}
  (function(){return this}()));
