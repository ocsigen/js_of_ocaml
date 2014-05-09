//Phantom
if(joo_global_object.phantom && joo_global_object.phantom.exit)
  joo_global_object.quit = joo_global_object.phantom.exit;
//Node
else if(joo_global_object.process && joo_global_object.process.exit)
  joo_global_object.quit = joo_global_object.process.exit;

if(joo_global_object.onerror)
  joo_global_object.onerror = function() { caml_sys_exit(1);}
