(module
   (import "bindings" "gettimeofday" (func $gettimeofday (result f64)))
   (import "bindings" "gmtime" (func $gmtime (result (ref eq))))
   (import "bindings" "localtime" (func $localtime (result (ref eq))))

   (type $block (array (mut (ref eq))))
   (type $float (struct (field f64)))

   (func (export "unix_gettimeofday")
      (param (ref eq)) (result (ref eq))
      (struct.new $float (call $gettimeofday)))

   (func (export "caml_alloc_tm")
      (param $sec i32) (param $min i32) (param $hour i32) (param $mday i32)
      (param $mon i32) (param $year i32) (param $wday i32) (param $yday $i32)
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

   (func (export "unix_gmtime") (param (ref eq)) (result (ref eq))
      (call $gmtime))

   (func (export "unix_localtime") (param (ref eq)) (result (ref eq))
      (call $localtime))

   (func (export "unix_time") (param (ref eq)) (result (ref eq))
      (struct.new $float (f64.floor (call $gettimeofday))))

   (func (export "unix_inet_addr_of_string")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))
)
