(module
   (@if $portable-int
   (@then
      ;; Only defined by the OxCaml fork of wasm_of_ocaml
      (import "env" "portable_int_val"
         (func $int_val (param (ref eq)) (result i64)))))

   (func (export "test_conditionals") (param (ref eq)) (result (ref eq))
      (local $n i32)
      (local.set $n
         (@if $portable-int
         (@then (i32.const 1000000))
         (@else (i32.const 1))))
      (local.set $n
         (i32.add (local.get $n)
            (@if (>= $ocaml_version (4 14 0))
            (@then (i32.const 20))
            (@else (i32.const 200000)))))
      (local.set $n
         (i32.add (local.get $n)
            (@if (not $portable-int)
            (@then
               (@if (and (not $portable-int) (<> "a" "b"))
               (@then (i32.const 300))
               (@else (i32.const 3000))))
            (@else (i32.const 30000)))))
      (ref.i31
         (i32.add (local.get $n)
            (select (result i32) (i32.const 4000) (i32.const 40000)
               (@if $portable-int
               (@then (i32.const 0))
               (@else (i32.const 1))))))))
