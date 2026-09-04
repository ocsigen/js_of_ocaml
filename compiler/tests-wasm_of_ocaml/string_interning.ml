(* Regression test for string constant interning in the Wasm backend: equal
   string constants share a single value, including long strings (>= 64
   bytes), which are built from a data segment by initialization code. *)

let long1 = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

let long2 = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

let short1 = "short string"

let short2 = "short string"

(* A constant block containing a long string. It is read through a reference
   cell mutated at runtime so that reading the block is not constant-folded
   away: this exercises the initialization code patching the interned string
   into the block, which must run after the global holding the string has
   been set. *)
let cell = ref ("", 0)

let () =
  cell := "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef", 17;
  if Array.length Sys.argv > 999 then cell := "unreachable", 0

let () =
  let s, n = !cell in
  Printf.printf "long length: %d\n" (String.length long1);
  Printf.printf "long1 == long2: %b\n" (long1 == long2);
  Printf.printf "short1 == short2: %b\n" (short1 == short2);
  Printf.printf "nested contents ok: %b\n" (String.equal s long1);
  Printf.printf "nested shared: %b\n" (s == long1);
  Printf.printf "n: %d\n" n
