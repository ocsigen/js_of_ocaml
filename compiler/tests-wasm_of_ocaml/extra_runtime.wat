(module
   ;; Not provided by the runtime, but only used by dead code
   (import "env" "caml_not_in_the_runtime"
      (func $not_in_the_runtime (param (ref eq)) (result (ref eq))))

   (func $unused (param $x (ref eq)) (result (ref eq))
      (call $not_in_the_runtime (local.get $x)))

   (func (export "caml_extra_identity") (param $x (ref eq)) (result (ref eq))
      (local.get $x)))
