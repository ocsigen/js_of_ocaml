At -O2 and -O3, higher-order functions are cloned for each combination of
known functions passed for the parameters they call, so that these calls
become direct calls. This is only done when one of these functions may
raise.

  $ cat > prog.ml << EOF
  > exception Stop
  > let rec iterate step x =
  >   let y = step x in
  >   try iterate step y with Stop -> y
  > let[@inline never] step_a x = if x >= 10 then raise Stop else x + 1
  > let[@inline never] step_b x = if x >= 100 then raise Stop else x * 2
  > let unknown = ref step_a
  > let () = if Array.length Sys.argv > 999 then unknown := step_b
  > let rec apply_n f n x = if n = 0 then x else apply_n f (n - 1) (f x)
  > let[@inline never] double x = 2 * x
  > let[@inline never] succ x = x + 1
  > let () =
  >   Printf.printf "%d %d %d %d %d\n"
  >     (iterate step_a 0) (iterate step_b 1) (iterate !unknown 5)
  >     (apply_n double 3 1) (apply_n succ 3 1)
  > EOF
  $ ocamlc -g prog.ml -o prog.bc

One copy of [iterate] per known step function. The call with an unknown
function still uses the original function. The function [apply_n] is not
cloned, as the functions passed to it do not raise.

  $ wasm_of_ocaml --opt 2 --pretty --debug clone prog.bc -o prog.js 2>&1 \
  >   | grep '{iterate}\|{apply_n}' | sed 's/^clone v[0-9]*/clone /'
  clone {iterate} (size 14): 2 copies (1 other uses)
  $ node prog.js
  10 128 10 8 4

No cloning at -O1, where the flow analysis does not track function
parameters.

  $ wasm_of_ocaml --opt 1 --pretty --debug clone prog.bc -o prog.js 2>&1 \
  >   | grep -c '{iterate}'
  0
  [1]
  $ node prog.js
  10 128 10 8 4

The pass can be disabled.

  $ wasm_of_ocaml --opt 2 --pretty --disable clone-higher-order --debug clone \
  >   prog.bc -o prog.js 2>&1 | grep -c '{iterate}'
  0
  [1]
