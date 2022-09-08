//Provides: caml_alloc_stack
function caml_alloc_stack(ret, exn, h) {
    return {ret:ret, exn:exn, h:h};
}

//Provides: caml_ml_condition_new
function caml_ml_condition_new(unit){
    return {condition:1};
}

//Provides: caml_ml_condition_wait
function caml_ml_condition_wait(t,mutext){
    return 0;
}

//Provides: caml_ml_condition_broadcast
function caml_ml_condition_broadcast(t){
    return 0;
}

//Provides: caml_ml_condition_signal
function caml_ml_condition_signal(t){
    return 0;
}

