//Provides: caml_sys_exit
function caml_sys_exit(n) {
  if(this.phantom && this.phantom.exit)
    phantom.exit(n);
  else
    return 0;
}

window.onerror = function() { caml_sys_exit(1);}
