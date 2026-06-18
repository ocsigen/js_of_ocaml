(* Hashing a JS string must agree on the js and wasm runtimes. A string whose
   code units all fit in a byte (every ASCII string, and Latin-1) is mixed
   directly, four code units per word; a string with larger code points is
   mixed by its UTF-8 bytes, since packing the raw code units would overlap
   their high bits and lose information.

   We pin the hash of [Js.string s] for a range of inputs; the js and wasm
   runtimes share this [%expect] block, so any divergence between them — or from
   the recorded reference values — is caught. For an ASCII string the JS string
   and the equivalent OCaml string share their bytes, so their hashes must also
   agree; we assert that. *)

open Js_of_ocaml

let is_ascii s = String.for_all (fun c -> Char.code c < 128) s

let check s =
  let js = Hashtbl.hash (Js.string s) in
  Printf.printf "%S -> %d\n" s js;
  if is_ascii s
  then begin
    let ocaml = Hashtbl.hash s in
    if js <> ocaml then Printf.printf "  MISMATCH vs OCaml string: %d\n" ocaml
  end

let%expect_test "JS string hashing matches the JS runtime (ASCII and UTF-8)" =
  List.iter
    check
    [ (* ASCII *)
      ""
    ; "a"
    ; "ab"
    ; "abc"
    ; "abcd"
    ; "abcde"
    ; "hello world"
    ; "a longer ascii string of some length"
    ; (* UTF-8 encoded non-ASCII *)
      "é"
    ; "héllo"
    ; "naïve café"
    ; "Σ"
    ; "日本語"
    ; "🎉"
    ; "café — résumé"
    ];
  [%expect
    {|
    "" -> 0
    "a" -> 721651713
    "ab" -> 856662637
    "abc" -> 767105082
    "abcd" -> 65890154
    "abcde" -> 335633756
    "hello world" -> 1053319576
    "a longer ascii string of some length" -> 423036158
    "\195\169" -> 145260179
    "h\195\169llo" -> 46355517
    "na\195\175ve caf\195\169" -> 359129460
    "\206\163" -> 642168663
    "\230\151\165\230\156\172\232\170\158" -> 802525496
    "\240\159\142\137" -> 313031078
    "caf\195\169 \226\128\148 r\195\169sum\195\169" -> 782065221
    |}]
