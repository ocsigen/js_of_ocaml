;; This ensures that the referenced JavaScript values are linked in the
;; runtime with separate compilation and is optimized away in case of
;; whole program compilation.

(global (export "_caml_js_delete") (import "js" "caml_js_delete") anyref)
(global (export "_process") (import "js" "process") anyref)
(global (export "_obj") (import "js" "obj") anyref)
