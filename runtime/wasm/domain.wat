(module
   (type $block (array (mut (ref eq))))

   (func (export "caml_atomic_cas")
      (param $ref (ref eq)) (param $o (ref eq)) (param $n (ref eq))
      (result (ref eq))
      (local $b (ref $block))
      (local.set $b (ref.cast $block (local.get $ref)))
      (if (result (ref eq))
         (ref.eq (array.get $block (local.get $b) (i32.const 1))
                 (local.get $o))
         (then
            (array.set $block (local.get $b) (i32.const 1) (local.get $n))
            (i31.new (i32.const 1)))
         (else
            (i31.new (i32.const 0)))))

   (func (export "caml_atomic_load") (param (ref eq)) (result (ref eq))
      (array.get $block (ref.cast $block (local.get 0)) (i32.const 1)))

   (func (export "caml_atomic_fetch_add")
     (param $ref (ref eq)) (param $i (ref eq)) (result (ref eq))
     (local $b (ref $block))
     (local $old (ref eq))
     (local.set $b (ref.cast $block (local.get $ref)))
     (local.set $old (array.get $block (local.get $b) (i32.const 1)))
     (array.set $block (local.get $b) (i32.const 1)
        (i31.new (i32.add (i31.get_s (ref.cast i31 (local.get $old)))
                          (i31.get_s (ref.cast i31 (local.get $i))))))
     (local.get $old))

   (global $caml_domain_dls (mut (ref eq))
      (array.new_fixed $block (i31.new (i32.const 0))))

   (func (export "caml_domain_dls_set") (param $a (ref eq)) (result (ref eq))
      (global.set $caml_domain_dls (local.get $a))
      (i31.new (i32.const 0)))

   (func (export "caml_domain_dls_get") (param (ref eq)) (result (ref eq))
      (global.get $caml_domain_dls))
)
