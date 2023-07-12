(module
   (import "bindings" "gettimeofday" (func $gettimeofday (result f64)))
   (import "bindings" "gmtime" (func $gmtime (param f64) (result (ref eq))))
   (import "bindings" "localtime"
      (func $localtime (param f64) (result (ref eq))))
   (import "bindings" "mktime"
      (func $mktime
         (param i32) (param i32) (param i32) (param i32) (param i32) (param i32)
         (result f64)))

   (type $block (array (mut (ref eq))))
   (type $float (struct (field f64)))

   (export "caml_unix_gettimeofday" (func $unix_gettimeofday))
   (func $unix_gettimeofday (export "unix_gettimeofday")
      (param (ref eq)) (result (ref eq))
      (struct.new $float (call $gettimeofday)))

   (func (export "caml_alloc_tm")
      (param $sec i32) (param $min i32) (param $hour i32) (param $mday i32)
      (param $mon i32) (param $year i32) (param $wday i32) (param $yday i32)
      (param $isdst i32) (result (ref eq))
      (array.new_fixed $block (i31.new (i32.const 0))
         (i31.new (local.get $sec))
         (i31.new (local.get $min))
         (i31.new (local.get $hour))
         (i31.new (local.get $mday))
         (i31.new (local.get $mon))
         (i31.new (local.get $year))
         (i31.new (local.get $wday))
         (i31.new (local.get $yday))
         (i31.new (local.get $isdst))))

   (export "caml_unix_gmtime" (func $unix_gmtime))
   (func $unix_gmtime (export "unix_gmtime") (param (ref eq)) (result (ref eq))
      (call $gmtime (struct.get $float 0 (ref.cast $float (local.get 0)))))

   (export "caml_unix_localtime" (func $unix_localtime))
   (func $unix_localtime (export "unix_localtime")
      (param (ref eq)) (result (ref eq))
      (call $localtime (struct.get $float 0 (ref.cast $float (local.get 0)))))

   (export "caml_unix_time" (func $unix_time))
   (func $unix_time (export "unix_time") (param (ref eq)) (result (ref eq))
      (struct.new $float (f64.floor (call $gettimeofday))))

   (export "caml_unix_mktime" (func $unix_mktime))
   (func $unix_mktime (export "unix_mktime") (param (ref eq)) (result (ref eq))
      (local $tm (ref $block)) (local $t f64)
      (local.set $tm (ref.cast $block (local.get 0)))
      (local.set $t
         (f64.div
            (call $mktime
               (i32.add
                  (i31.get_s
                     (ref.cast i31
                       (array.get $block (local.get $tm) (i32.const 6))))
                  (i32.const 1900))
               (i31.get_s
                  (ref.cast i31
                     (array.get $block (local.get $tm) (i32.const 5))))
               (i31.get_s
                  (ref.cast i31
                     (array.get $block (local.get $tm) (i32.const 4))))
               (i31.get_s
                  (ref.cast i31
                     (array.get $block (local.get $tm) (i32.const 3))))
               (i31.get_s
                  (ref.cast i31
                     (array.get $block (local.get $tm) (i32.const 2))))
               (i31.get_s
                  (ref.cast i31
                     (array.get $block (local.get $tm) (i32.const 1)))))
            (f64.const 1000)))
      (array.new_fixed $block (i31.new (i32.const 0))
         (struct.new $float (local.get $t))
         (call $localtime (local.get $t))))

   (export "caml_unix_inet_addr_of_string" (func $unix_inet_addr_of_string))
   (func $unix_inet_addr_of_string (export "unix_inet_addr_of_string")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))
)
