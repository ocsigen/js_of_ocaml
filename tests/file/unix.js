//Provides: caml_sys_exit
function caml_sys_exit(n) {
    if(joo_global_object.phantom && joo_global_object.phantom.exit)
        phantom.exit(n);
    else
        return 0;
}

window.onerror = function() { caml_sys_exit(1);}
