(module
   (type $float (struct (field f64)))
   (type $block (array (mut (ref eq))))

   (func (export "caml_gc_minor") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gc_major") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gc_full_major") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gc_compaction") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gc_counters") (param (ref eq)) (result (ref eq))
      (local $f (ref eq))
      (local.set $f (struct.new $float (f64.const 0)))
      (array.new_fixed $block 4 (ref.i31 (i32.const 0))
         (local.get $f) (local.get $f) (local.get $f)))

   (export "caml_gc_quick_stat" (func $caml_gc_stat))
   (func $caml_gc_stat (export "caml_gc_stat")
      (param (ref eq)) (result (ref eq))
      (local $f (ref eq))
      (local.set $f (struct.new $float (f64.const 0)))
      (array.new_fixed $block 18 (ref.i31 (i32.const 0))
         (local.get $f) (local.get $f) (local.get $f)
         (ref.i31 (i32.const 0)) (ref.i31 (i32.const 0))
         (ref.i31 (i32.const 0)) (ref.i31 (i32.const 0))
         (ref.i31 (i32.const 0)) (ref.i31 (i32.const 0))
         (ref.i31 (i32.const 0)) (ref.i31 (i32.const 0))
         (ref.i31 (i32.const 0)) (ref.i31 (i32.const 0))
         (ref.i31 (i32.const 0)) (ref.i31 (i32.const 0))
         (ref.i31 (i32.const 0)) (ref.i31 (i32.const 0))))

   (func (export "caml_gc_set") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gc_get") (param (ref eq)) (result (ref eq))
      (array.new_fixed $block 12
         (ref.i31 (i32.const 0)) (ref.i31 (i32.const 0))
         (ref.i31 (i32.const 0)) (ref.i31 (i32.const 0))
         (ref.i31 (i32.const 0)) (ref.i31 (i32.const 0))
         (ref.i31 (i32.const 0)) (ref.i31 (i32.const 0))
         (ref.i31 (i32.const 0)) (ref.i31 (i32.const 0))
         (ref.i31 (i32.const 0)) (ref.i31 (i32.const 0))))

   (func (export "caml_gc_huge_fallback_count")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gc_major_slice")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gc_major_bucket")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gc_major_credit")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gc_minor_free")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gc_minor_words")
      (param (ref eq)) (result (ref eq))
      (struct.new $float (f64.const 0)))

   (func (export "caml_final_register")
      (param (ref eq) (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_final_register_called_without_value")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ Use FinalizationRegistry?
      (ref.i31 (i32.const 0)))

   (func (export "caml_final_release") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_memprof_start")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_memprof_set") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_memprof_stop") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_eventlog_pause") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_eventlog_resume") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))
)
