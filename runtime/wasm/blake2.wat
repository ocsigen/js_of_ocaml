(module
(@if (>= ocaml_version (5 2 0))
(@then
   (import "jslib" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "jslib" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "jslib" "caml_string_of_jsbytes"
      (func $caml_string_of_jsbytes (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_jsbytes_of_string"
      (func $caml_jsbytes_of_string (param (ref eq)) (result (ref eq))))

   (import "js" "blake2_js_for_wasm_create"
      (func $blake2_js_for_wasm_create (param (ref eq) anyref) (result anyref)))

   (import "js" "blake2_js_for_wasm_update"
      (func $blake2_js_for_wasm_update (param anyref anyref i32 i32)))

   (import "js" "blake2_js_for_wasm_string"
      (func $blake2_js_for_wasm_string
         (param (ref eq) anyref anyref i32 i32) (result anyref)))

   (import "js" "blake2_js_for_wasm_final"
      (func $blake2_js_for_wasm_final (param anyref (ref eq)) (result anyref)))

   (type $bytes (array (mut i8)))

   (func (export "caml_blake2_create")
      (param $hashlen (ref eq)) (param $key (ref eq)) (result (ref eq))
      (return_call $wrap
         (call $blake2_js_for_wasm_create
            (local.get $hashlen)
            (call $unwrap (call $caml_jsbytes_of_string (local.get $key))))))

   (func $jsbytes_of_substring
      (param $vbuf (ref eq)) (param $vofs (ref eq)) (param $vlen (ref eq))
      (result anyref i32 i32)
      (local $buf (ref $bytes)) (local $buf' (ref $bytes))
      (local $ofs i32) (local $len i32)
      (local.set $buf (ref.cast (ref $bytes) (local.get $vbuf)))
      (local.set $ofs (i31.get_u (ref.cast (ref i31) (local.get $vofs))))
      (local.set $len (i31.get_u (ref.cast (ref i31) (local.get $vlen))))
      ;; We copy the relevant part of the buffer if we only use a small portion
      (if (i32.le_u (i32.shl (local.get $len) (i32.const 1))
             (array.len (local.get $buf)))
         (then
            (local.set $buf' (array.new $bytes (i32.const 0) (local.get $len)))
            (array.copy $bytes $bytes
               (local.get $buf') (i32.const 0)
               (local.get $buf) (local.get $ofs) (local.get $len))
            (local.set $buf (local.get $buf'))
            (local.set $ofs (i32.const 0))))
      (tuple.make 3
         (call $unwrap (call $caml_jsbytes_of_string (local.get $buf)))
         (local.get $ofs)
         (local.get $len)))

   (func (export "caml_blake2_update")
      (param $ctx (ref eq)) (param $buf (ref eq)) (param $ofs (ref eq))
      (param $len (ref eq)) (result (ref eq))
      (call $blake2_js_for_wasm_update
         (call $unwrap (local.get $ctx))
         (call $jsbytes_of_substring
            (local.get $buf) (local.get $ofs) (local.get $len)))
      (ref.i31 (i32.const 0)))

   (func (export "caml_blake2_final")
      (param $ctx (ref eq)) (param $hashlen (ref eq)) (result (ref eq))
      (return_call $caml_string_of_jsbytes
         (call $wrap
            (call $blake2_js_for_wasm_final
               (call $unwrap (local.get $ctx))
               (local.get $hashlen)))))

   (func (export "caml_blake2_string") (export "caml_blake2_bytes")
      (param $hashlen (ref eq)) (param $key (ref eq)) (param $buf (ref eq))
      (param $ofs (ref eq)) (param $len (ref eq)) (result (ref eq))
      (local $ctx anyref)
      (local.set $ctx
         (call $blake2_js_for_wasm_create
            (local.get $hashlen)
            (call $unwrap (call $caml_jsbytes_of_string (local.get $key)))))
      (call $blake2_js_for_wasm_update
         (local.get $ctx)
         (call $jsbytes_of_substring
            (local.get $buf) (local.get $ofs) (local.get $len)))
      (return_call $caml_string_of_jsbytes
         (call $wrap
            (call $blake2_js_for_wasm_final
               (local.get $ctx) (local.get $hashlen)))))
))
)
