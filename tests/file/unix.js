//Provides: caml_ml_open_descriptor_in
function  caml_ml_open_descriptor_in() {
  return 0;
}

//Provides: caml_sys_get_argv
function  caml_sys_get_argv() {
  return [];
}

//Provides: caml_sys_getenv
function  caml_sys_getenv() {
  return new MlString("");
}

//Provides: unix_inet_addr_of_string
function  unix_inet_addr_of_string() {
  return 0;
}

//Provides: caml_sys_exit
function   caml_sys_exit(n) {
    if(this.phantom && this.phantom.exit)
        phantom.exit(n);
    else
        return 0;
}
