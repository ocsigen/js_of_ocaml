(function(globalThis)
   {"use strict";
    var
     runtime=globalThis.jsoo_runtime,
     caml_string_of_jsbytes=runtime.caml_string_of_jsbytes;
    function caml_call1(f,a0)
     {return f.length == 1?f(a0):runtime.caml_call_gen(f,[a0])}
    runtime.jsoo_create_file
     ("/static/cmis/test_dynlink.cmi",
      "Caml1999I031\x84\x95\xa6\xbe\0\0\0q\0\0\0\x1a\0\0\0`\0\0\0Z\xa0,Test_dynlink\xa0\xb0\xa0!f\x01\x01\x0e\xd0\xc0\xc1@\xc0\xb3\x90\xa3$unitF@\x90@\x02\x05\xf5\xe1\0@\0\xfc\xc0\xb3\x90\xa3$unitF@\x90@\x02\x05\xf5\xe1\0@\0\xfd@\x02\x05\xf5\xe1\0@\0\xfe@\xb0\xc0/test_dynlink.mlCdh\xc0\x04\x02Cdi@@\xa1\x04\x19@@@\x84\x95\xa6\xbe\0\0\0j\0\0\0\x0f\0\0\0:\0\0\0.\xa0\xa0,Test_dynlink\x900\xe4\xc7)\xb8T\x8b\xce\xdc\x95\xadUn\x94\xd1'\xf8\xa0\xa0&Stdlib\x900m{\xf1\x1a\xf1N\xa6\x83T\x92_:78y0\xa0\xa08CamlinternalFormatBasics\x900\x8f\x8fcEXy\x8e\xe4\b\xdf<P\xa5S\x9b\x15@\x84\x95\xa6\xbe\0\0\0\x04\0\0\0\x02\0\0\0\x05\0\0\0\x05\xa0\x90@@");
    var
     global_data=runtime.caml_get_global_data(),
     cst_Test_dynlink_f_Ok=caml_string_of_jsbytes("Test_dynlink.f Ok"),
     cst_Dynlink_OK=caml_string_of_jsbytes("Dynlink OK"),
     Stdlib=global_data.Stdlib;
    caml_call1(Stdlib[46],cst_Dynlink_OK);
    var
     Test_dynlink=
      [0,function(_a_){return caml_call1(Stdlib[46],cst_Test_dynlink_f_Ok)}];
    runtime.caml_register_global(3,Test_dynlink,"Test_dynlink");
    return}
  (globalThis));
