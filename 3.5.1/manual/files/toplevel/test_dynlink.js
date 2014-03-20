(function(joo_global_object)
   {"use strict";
    var
     runtime=joo_global_object.jsoo_runtime,
     caml_new_string=runtime.caml_new_string;
    function caml_call1(f,a0)
     {return f.length == 1?f(a0):runtime.caml_call_gen(f,[a0])}
    runtime.caml_create_file
     ("/static/cmis/test_dynlink.cmi",
      "Caml1999I026\x84\x95\xa6\xbe\0\0\0l\0\0\0\x19\0\0\0\\\0\0\0V\xa0,Test_dynlink\xa0\xb0\xa0!f\0R\xc0\xc0\xc1@\xc0\xb3\x90\xa3$unitF@\x90@\x02\x05\xf5\xe1\0@\0\xfc\xc0\xb3\x90\xa3$unitF@\x90@\x02\x05\xf5\xe1\0@\0\xfd@\x02\x05\xf5\xe1\0@\0\xfe@\xb0\xc0/test_dynlink.mlCdh\xc0\x04\x02Cdi@@@@\x84\x95\xa6\xbe\0\0\0j\0\0\0\x0f\0\0\0:\0\0\0.\xa0\xa0,Test_dynlink\x900^gV\xcd\x92J\x1eM\xcd\x80\xa4\xe9\x9bz\x06\x9d\xa0\xa0&Stdlib\x900M\x1e#\xb7\xe2\x1b\xea\xfc~5\xae\x8d\x03\x17Sc\xa0\xa08CamlinternalFormatBasics\x900\x96\xd1j\xaebU\xeb\x02\xa7\x88\xee\x96\xa8\xad\xb8\x16@\x84\x95\xa6\xbe\0\0\0\x04\0\0\0\x02\0\0\0\x05\0\0\0\x05\xa0\x90@@");
    var
     global_data=runtime.caml_get_global_data(),
     cst_Test_dynlink_f_Ok=caml_new_string("Test_dynlink.f Ok"),
     cst_Dynlink_OK=caml_new_string("Dynlink OK"),
     Stdlib=global_data.Stdlib;
    caml_call1(Stdlib[46],cst_Dynlink_OK);
    var
     Test_dynlink=
      [0,function(_a_){return caml_call1(Stdlib[46],cst_Test_dynlink_f_Ok)}];
    runtime.caml_register_global(3,Test_dynlink,"Test_dynlink");
    return}
  (function(){return this}()));
