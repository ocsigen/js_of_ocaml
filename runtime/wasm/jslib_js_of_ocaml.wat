(module
   (import "jslib" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "jslib" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "jslib" "caml_js_global"
      (func $caml_js_global (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_js_get"
      (func $caml_js_get (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_js_new"
      (func $caml_js_new (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_js_from_array"
      (func $caml_js_from_array (param (ref eq)) (result (ref eq))))
   (import "js" "caml_js_on_ie" (func $caml_js_on_ie (result i32)))
   (import "js" "caml_js_html_escape"
      (func $caml_js_html_escape (param anyref) (result anyref)))
   (import "js" "caml_js_html_entities"
      (func $caml_js_html_entities (param anyref) (result anyref)))

   (type $block (array (mut (ref eq))))

   (func (export "caml_js_on_ie") (param (ref eq)) (result (ref eq))
      (ref.i31 (call $caml_js_on_ie)))

   (func (export "caml_js_html_escape") (param (ref eq)) (result (ref eq))
      (return_call $wrap
         (call $caml_js_html_escape (call $unwrap (local.get 0)))))

   (func (export "caml_js_html_entities") (param (ref eq)) (result (ref eq))
      (return_call $wrap
         (call $caml_js_html_entities (call $unwrap (local.get 0)))))

   (func (export "caml_js_get_console") (param (ref eq)) (result (ref eq))
      (return_call $caml_js_get (call $caml_js_global (ref.i31 (i32.const 0)))
         (call $wrap (string.const "console"))))

   (func (export "caml_xmlhttprequest_create") (param (ref eq)) (result (ref eq))
      (return_call $caml_js_new
         (call $caml_js_get
            (call $caml_js_global (ref.i31 (i32.const 0)))
            (call $wrap (string.const "XMLHttpRequest")))
         (call $caml_js_from_array
            (array.new_fixed $block 1 (ref.i31 (i32.const 0))))))
)
