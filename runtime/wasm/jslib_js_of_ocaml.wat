(module
   (import "bindings" "log" (func $log_js (param anyref)))

   (func (export "caml_js_get_console")
     (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_js_get_console"))
      (i31.new (i32.const 0)))

  (func (export "caml_js_html_entities")
     (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_js_html_entities"))
      (i31.new (i32.const 0)))

  (func (export "caml_js_html_escape")
     (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_js_html_escape"))
      (i31.new (i32.const 0)))

  (func (export "caml_xmlhttprequest_create") (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_xmlhttprequest_create"))
      (i31.new (i32.const 0)))

  (func (export "caml_js_on_ie")
     (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_js_on_ie"))
      (i31.new (i32.const 0)))
)
