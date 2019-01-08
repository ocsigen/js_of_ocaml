(function(joo_global_object)
   {"use strict";
    var
     runtime=joo_global_object.jsoo_runtime,
     caml_string_of_jsbytes=runtime.caml_string_of_jsbytes;
    function caml_call1(f,a0)
     {return f.length == 1?f(a0):runtime.caml_call_gen(f,[a0])}
    runtime.caml_create_file
     ("/static/cmis/test_dynlink.cmi",
      "Caml1999I028\x84\x95\xa6\xbe\0\0\0p\0\0\0\x1a\0\0\0`\0\0\0Z\xa0,Test_dynlink\xa0\xb0\xa0!f\0S\xd0\xc0\xc1@\xc0\xb3\x90\xa3$unitF@\x90@\x02\x05\xf5\xe1\0@\0\xfc\xc0\xb3\x90\xa3$unitF@\x90@\x02\x05\xf5\xe1\0@\0\xfd@\x02\x05\xf5\xe1\0@\0\xfe@\xb0\xc0/test_dynlink.mlCdh\xc0\x04\x02Cdi@@\xa1\x04\x19@@@\x84\x95\xa6\xbe\0\0\0j\0\0\0\x0f\0\0\0:\0\0\0.\xa0\xa0,Test_dynlink\x900Z\0N\x94\xba,\xce\xe9\xe4/\x84)u\xa3c\xb4\xa0\xa0&Stdlib\x900\xc2\x1c]&Ada\xb5C2\x18r\xa5Q\xea\r\xa0\xa08CamlinternalFormatBasics\x900:<\xa1\x83\x86'\xf7v/Ig\x9c\xe0'\x8a\xd1@\x84\x95\xa6\xbe\0\0\0\x04\0\0\0\x02\0\0\0\x05\0\0\0\x05\xa0\x90@@");
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
  (function(){return this}()));
