//Phantom
if(joo_global_object.phantom && joo_global_object.phantom.exit)
  joo_global_object.quit = joo_global_object.phantom.exit;

if(joo_global_object.onerror)
  joo_global_object.onerror = function() { caml_sys_exit(1);}
