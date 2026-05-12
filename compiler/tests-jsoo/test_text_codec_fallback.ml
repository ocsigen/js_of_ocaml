(* Cross-check the in-tree TextEncoder / TextDecoder fallbacks (used when
   the host lacks the Web Encoding API, e.g. QuickJS) against the host's
   TextEncoder / TextDecoder. In Node both are available, so each case
   prints the fallback's byte/codepoint output as a snapshot and flags
   a divergence from the host inline. *)

open Js_of_ocaml

class type encoder = object
  method encode : Js.js_string Js.t -> Typed_array.uint8Array Js.t Js.meth
end

class type decoder = object
  method decode : Typed_array.uint8Array Js.t -> Js.js_string Js.t Js.meth
end

let enc_fallback : encoder Js.t =
  Jsoo_runtime.Js.runtime_value "jsoo_text_encoder_fallback"

let dec_fallback : decoder Js.t =
  Jsoo_runtime.Js.runtime_value "jsoo_text_decoder_fallback"

let enc_host : encoder Js.t = Js.Unsafe.eval_string "new TextEncoder()"

let dec_host : decoder Js.t = Js.Unsafe.eval_string "new TextDecoder()"

let hex_of_bytes (a : Typed_array.uint8Array Js.t) =
  String.concat
    " "
    (List.init a##.length (fun i ->
         Printf.sprintf "%02x" (Typed_array.unsafe_get a i)))

let bytes_eq (a : Typed_array.uint8Array Js.t) (b : Typed_array.uint8Array Js.t) =
  let n = a##.length in
  n = b##.length
  &&
  let rec loop i =
    i = n
    || (Typed_array.unsafe_get a i = Typed_array.unsafe_get b i && loop (i + 1))
  in
  loop 0

let codepoints_of_jsstring (s : Js.js_string Js.t) =
  String.concat
    " "
    (List.init s##.length (fun i ->
         Printf.sprintf
           "U+%04X"
           (int_of_float (Js.float_of_number (s##charCodeAt i)))))

let uint8_of_list xs =
  let arr = new%js Typed_array.uint8Array (List.length xs) in
  List.iteri (fun i v -> Typed_array.set arr i v) xs;
  arr

let hex_of_list xs = String.concat " " (List.map (Printf.sprintf "%02x") xs)

(* Build a [Js.js_string] from raw UTF-16 code units, including lone
   surrogates that OCaml literals can't express. *)
let jsstring_of_codes codes : Js.js_string Js.t =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "String.fromCharCode")
    (Array.of_list (List.map Js.Unsafe.inject codes))

let check_encode label (s : Js.js_string Js.t) =
  let fb = enc_fallback##encode s in
  let host = enc_host##encode s in
  Printf.printf "%s -> [%s]" label (hex_of_bytes fb);
  if not (bytes_eq fb host)
  then Printf.printf "  ! host: [%s]" (hex_of_bytes host);
  print_newline ()

let check_decode label bytes =
  let arr = uint8_of_list bytes in
  let fb = dec_fallback##decode arr in
  let host = dec_host##decode arr in
  Printf.printf
    "%s [%s] -> [%s]"
    label
    (hex_of_list bytes)
    (codepoints_of_jsstring fb);
  if Js.to_string fb <> Js.to_string host
  then Printf.printf "  ! host: [%s]" (codepoints_of_jsstring host);
  print_newline ()

let%expect_test "encoder: well-formed UTF-16 input" =
  check_encode {|""|} (Js.string "");
  check_encode {|"Hello, world!"|} (Js.string "Hello, world!");
  (* boundaries of each UTF-8 byte-length range *)
  check_encode {|"\x00\x7f"|} (Js.string "\x00\x7f");
  check_encode {|U+0080..U+07FF|} (Js.string "\u{0080}\u{07ff}");
  check_encode {|U+0800..U+D7FF|} (Js.string "\u{0800}\u{d7ff}");
  check_encode {|U+E000..U+FFFF|} (Js.string "\u{e000}\u{ffff}");
  check_encode {|U+10000..U+10FFFF|} (Js.string "\u{10000}\u{10ffff}");
  (* a mix *)
  check_encode {|"a中🎉b"|} (Js.string "a中🎉b");
  [%expect
    {|
    "" -> []
    "Hello, world!" -> [48 65 6c 6c 6f 2c 20 77 6f 72 6c 64 21]
    "\x00\x7f" -> [00 7f]
    U+0080..U+07FF -> [c2 80 df bf]
    U+0800..U+D7FF -> [e0 a0 80 ed 9f bf]
    U+E000..U+FFFF -> [ee 80 80 ef bf bf]
    U+10000..U+10FFFF -> [f0 90 80 80 f4 8f bf bf]
    "a中🎉b" -> [61 e4 b8 ad f0 9f 8e 89 62]
    |}]

let%expect_test "encoder: lone surrogates become U+FFFD" =
  (* WHATWG: any UTF-16 code unit that isn't part of a valid surrogate
     pair is replaced by the UTF-8 encoding of U+FFFD (ef bf bd). *)
  check_encode {|high alone|} (jsstring_of_codes [ 0xd83c ]);
  check_encode {|high + ascii|} (jsstring_of_codes [ 0xd83c; 0x41 ]);
  check_encode {|low alone|} (jsstring_of_codes [ 0xdc00 ]);
  check_encode {|low + high (reversed pair)|} (jsstring_of_codes [ 0xdc00; 0xd83c ]);
  check_encode {|high + non-surrogate BMP|} (jsstring_of_codes [ 0xd83c; 0x00e9 ]);
  [%expect
    {|
    high alone -> [ef bf bd]
    high + ascii -> [ef bf bd 41]
    low alone -> [ef bf bd]
    low + high (reversed pair) -> [ef bf bd ef bf bd]
    high + non-surrogate BMP -> [ef bf bd c3 a9]
    |}]

let%expect_test "decoder: well-formed UTF-8 input" =
  check_decode {|empty|} [];
  check_decode {|ASCII "Hi"|} [ 0x48; 0x69 ];
  check_decode {|U+00E9 "é"|} [ 0xc3; 0xa9 ];
  check_decode {|U+4E2D "中"|} [ 0xe4; 0xb8; 0xad ];
  check_decode {|U+1F389 "🎉"|} [ 0xf0; 0x9f; 0x8e; 0x89 ];
  (* Smallest / largest valid codepoint per UTF-8 byte-length. *)
  check_decode {|U+0080 (smallest 2-byte)|} [ 0xc2; 0x80 ];
  check_decode {|U+07FF (largest 2-byte)|} [ 0xdf; 0xbf ];
  check_decode {|U+0800 (smallest 3-byte)|} [ 0xe0; 0xa0; 0x80 ];
  check_decode {|U+D7FF (last BMP before surrogates)|} [ 0xed; 0x9f; 0xbf ];
  check_decode {|U+E000 (first BMP after surrogates)|} [ 0xee; 0x80; 0x80 ];
  check_decode {|U+FFFF (largest BMP)|} [ 0xef; 0xbf; 0xbf ];
  check_decode {|U+10000 (smallest 4-byte)|} [ 0xf0; 0x90; 0x80; 0x80 ];
  check_decode {|U+10FFFF (largest 4-byte)|} [ 0xf4; 0x8f; 0xbf; 0xbf ];
  [%expect
    {|
    empty [] -> []
    ASCII "Hi" [48 69] -> [U+0048 U+0069]
    U+00E9 "é" [c3 a9] -> [U+00E9]
    U+4E2D "中" [e4 b8 ad] -> [U+4E2D]
    U+1F389 "🎉" [f0 9f 8e 89] -> [U+D83C U+DF89]
    U+0080 (smallest 2-byte) [c2 80] -> [U+0080]
    U+07FF (largest 2-byte) [df bf] -> [U+07FF]
    U+0800 (smallest 3-byte) [e0 a0 80] -> [U+0800]
    U+D7FF (last BMP before surrogates) [ed 9f bf] -> [U+D7FF]
    U+E000 (first BMP after surrogates) [ee 80 80] -> [U+E000]
    U+FFFF (largest BMP) [ef bf bf] -> [U+FFFF]
    U+10000 (smallest 4-byte) [f0 90 80 80] -> [U+D800 U+DC00]
    U+10FFFF (largest 4-byte) [f4 8f bf bf] -> [U+DBFF U+DFFF]
    |}]

let%expect_test "decoder: invalid leads emit single U+FFFD" =
  check_decode {|stray 0x80|} [ 0x80 ];
  check_decode {|stray 0xbf|} [ 0xbf ];
  check_decode {|overlong 0xc0 0x80|} [ 0xc0; 0x80 ];
  check_decode {|overlong 0xc1 0xbf|} [ 0xc1; 0xbf ];
  check_decode {|out-of-range 0xf5|} [ 0xf5; 0x80; 0x80; 0x80 ];
  check_decode {|out-of-range 0xff|} [ 0xff ];
  (* 0xf4 is a valid 4-byte lead, but 0x90 as b2 would encode U+110000
     (out of the Unicode range). Distinct code path from out-of-range
     leads above. *)
  check_decode {|U+110000 attempt|} [ 0xf4; 0x90; 0x80; 0x80 ];
  (* Multi-byte lead followed by another lead byte (not a continuation). *)
  check_decode {|2-byte lead + new lead|} [ 0xc2; 0xc0 ];
  [%expect
    {|
    stray 0x80 [80] -> [U+FFFD]
    stray 0xbf [bf] -> [U+FFFD]
    overlong 0xc0 0x80 [c0 80] -> [U+FFFD U+FFFD]
    overlong 0xc1 0xbf [c1 bf] -> [U+FFFD U+FFFD]
    out-of-range 0xf5 [f5 80 80 80] -> [U+FFFD U+FFFD U+FFFD U+FFFD]
    out-of-range 0xff [ff] -> [U+FFFD]
    U+110000 attempt [f4 90 80 80] -> [U+FFFD U+FFFD U+FFFD U+FFFD]
    2-byte lead + new lead [c2 c0] -> [U+FFFD U+FFFD]
    |}]

let%expect_test "decoder: truncated sequences (maximal-subpart rule)" =
  (* A single U+FFFD is emitted that consumes the longest prefix that
     could have started a valid sequence. *)
  check_decode {|truncated 2-byte|} [ 0xc2 ];
  check_decode {|truncated 3-byte (1/3)|} [ 0xe4 ];
  check_decode {|truncated 3-byte (2/3)|} [ 0xe4; 0xb8 ];
  check_decode {|truncated 4-byte (1/4)|} [ 0xf0 ];
  check_decode {|truncated 4-byte (2/4)|} [ 0xf0; 0x9f ];
  check_decode {|truncated 4-byte (3/4)|} [ 0xf0; 0x9f; 0x8e ];
  [%expect
    {|
    truncated 2-byte [c2] -> [U+FFFD]
    truncated 3-byte (1/3) [e4] -> [U+FFFD]
    truncated 3-byte (2/3) [e4 b8] -> [U+FFFD]
    truncated 4-byte (1/4) [f0] -> [U+FFFD]
    truncated 4-byte (2/4) [f0 9f] -> [U+FFFD]
    truncated 4-byte (3/4) [f0 9f 8e] -> [U+FFFD]
    |}]

let%expect_test "decoder: non-shortest forms and UTF-8-encoded surrogates" =
  check_decode {|non-shortest 3-byte for U+07FF|} [ 0xe0; 0x9f; 0xbf ];
  check_decode {|non-shortest 4-byte for U+FFFF|} [ 0xf0; 0x8f; 0xbf; 0xbf ];
  check_decode {|surrogate U+D800 in UTF-8|} [ 0xed; 0xa0; 0x80 ];
  check_decode {|surrogate U+DFFF in UTF-8|} [ 0xed; 0xbf; 0xbf ];
  [%expect
    {|
    non-shortest 3-byte for U+07FF [e0 9f bf] -> [U+FFFD U+FFFD U+FFFD]
    non-shortest 4-byte for U+FFFF [f0 8f bf bf] -> [U+FFFD U+FFFD U+FFFD U+FFFD]
    surrogate U+D800 in UTF-8 [ed a0 80] -> [U+FFFD U+FFFD U+FFFD]
    surrogate U+DFFF in UTF-8 [ed bf bf] -> [U+FFFD U+FFFD U+FFFD]
    |}]

let%expect_test "decoder: lead with bad continuation, mixed runs" =
  check_decode {|2-byte lead + ASCII|} [ 0xc2; 0x41 ];
  check_decode {|3-byte lead + ASCII|} [ 0xe4; 0x41 ];
  check_decode {|mixed valid/invalid|} [ 0x48; 0xff; 0x69 ];
  check_decode {|ASCII after truncated 3-byte|} [ 0xe4; 0xb8; 0x41 ];
  check_decode {|valid then truncated|} [ 0xc3; 0xa9; 0xe4; 0xb8 ];
  [%expect
    {|
    2-byte lead + ASCII [c2 41] -> [U+FFFD U+0041]
    3-byte lead + ASCII [e4 41] -> [U+FFFD U+0041]
    mixed valid/invalid [48 ff 69] -> [U+0048 U+FFFD U+0069]
    ASCII after truncated 3-byte [e4 b8 41] -> [U+FFFD U+0041]
    valid then truncated [c3 a9 e4 b8] -> [U+00E9 U+FFFD]
    |}]
