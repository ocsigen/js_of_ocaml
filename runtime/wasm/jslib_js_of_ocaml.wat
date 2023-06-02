(module
   (import "bindings" "log" (func $log_js (param anyref)))
   (import "bindings" "eval" (func $eval (param anyref) (result anyref)))
   (import "jslib" "wrap" (func $wrap (param anyref) (result (ref eq))))

   (func (export "caml_js_get_console") (param (ref eq)) (result (ref eq))
      (return_call $wrap (call $eval (string.const "console"))))

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
      (return_call $wrap
         (call $eval (string.const "new XMLHttpRequest"))))

   (func (export "caml_js_on_ie") (param (ref eq)) (result (ref eq))
      (return_call $wrap
         (call $eval
            (string.const
               "var ua = navigator?navigator.userAgent:\"\"; ua.indexOf(\"MSIE\") != -1 && ua.indexOf(\"Opera\") != 0"))))
)
