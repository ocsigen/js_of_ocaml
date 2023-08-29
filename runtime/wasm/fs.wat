(module
   (import "bindings" "log" (func $log_js (param anyref)))
   (import "bindings" "getcwd" (func $getcwd (result anyref)))
   (import "bindings" "chdir" (func $chdir (param anyref)))
   (import "bindings" "unlink" (func $unlink (param anyref)))
   (import "bindings" "readdir"
      (func $readdir (param anyref) (result (ref extern))))
   (import "bindings" "file_exists"
      (func $file_exists (param anyref) (result (ref eq))))
   (import "jslib" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "jslib" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "jslib" "caml_string_of_jsstring"
      (func $caml_string_of_jsstring (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_jsstring_of_string"
      (func $caml_jsstring_of_string (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_js_to_string_array"
      (func $caml_js_to_string_array (param $a (ref extern)) (result (ref eq))))
   (import "fail" "caml_raise_sys_error"
      (func $caml_raise_sys_error (param (ref eq))))

   (type $string (array (mut i8)))

   (func (export "caml_sys_getcwd")
      (param (ref eq)) (result (ref eq))
      (return_call $caml_string_of_jsstring (call $wrap (call $getcwd))))

   (func (export "caml_sys_chdir")
      (param (ref eq)) (result (ref eq))
      (call $chdir (call $unwrap (call $caml_jsstring_of_string (local.get 0))))
      (i31.new (i32.const 0)))

   (func (export "caml_sys_mkdir")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_sys_mkdir"))
      (i31.new (i32.const 0)))

   (func (export "caml_sys_read_directory")
      (param (ref eq)) (result (ref eq))
      (return_call $caml_js_to_string_array
         (call $readdir
            (call $unwrap (call $caml_jsstring_of_string (local.get 0))))))

   (func (export "caml_sys_remove")
      (param (ref eq)) (result (ref eq))
      (call $unlink (call $unwrap (call $caml_jsstring_of_string (local.get 0))))
      (i31.new (i32.const 0)))

   (func (export "caml_sys_rename")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_sys_rename"))
      (i31.new (i32.const 0)))

   (func (export "caml_sys_file_exists")
      (param (ref eq)) (result (ref eq))
      (return_call $file_exists
         (call $unwrap (call $caml_jsstring_of_string (local.get 0)))))

   (data $no_such_file ": No such file or directory")

   (func $caml_raise_no_such_file (param $vname (ref eq))
      (local $name (ref $string)) (local $msg (ref $string))
      (local $len i32)
      (local.set $name (ref.cast (ref $string) (local.get $vname)))
      (local.set $len (array.len (local.get $name)))
      (local.set $msg
         (array.new $string (i32.const 0)
            (i32.add (local.get $len) (i32.const 27))))
      (array.copy $string $string
         (local.get $msg) (i32.const 0)
         (local.get $name) (i32.const 0)
         (local.get $len))
      (array.init_data $string $no_such_file
         (local.get $msg) (local.get $len) (i32.const 0) (i32.const 27))
      (call $caml_raise_sys_error (local.get $msg)))

   (func (export "caml_read_file_content")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_read_file_content"))
      (call $caml_raise_no_such_file (local.get 0))
      (i31.new (i32.const 0)))

   (func (export "caml_fs_init") (result (ref eq))
      (i31.new (i32.const 0)))
)
