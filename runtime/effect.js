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

//Provides: caml_continuation_use_and_update_handler_noexc
//Requires: caml_failwith
function caml_continuation_use_and_update_handler_noexc(){
  caml_failwith("caml_continuation_use_and_update_handler_noexc not implemented");
}

//Provides: caml_continuation_use_noexc
//Requires: caml_failwith
function caml_continuation_use_noexc(){
 caml_failwith("caml_continuation_use_noexc not implemented");
}
