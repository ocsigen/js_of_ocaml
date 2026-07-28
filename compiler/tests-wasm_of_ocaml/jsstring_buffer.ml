(* The scratch buffer used to convert between OCaml strings and JavaScript
   strings (jsstring.wat's [caml_buffer]) is the first 64 KiB page of the
   linear memory owned by the C runtime module, which also holds the zstd
   decoder's data and heap.  This checks that string conversion and zstd
   decompression do not interfere:

   - a string longer than the scratch page must be converted a page at a time,
     rather than written past the page and over the C module's data;

   - the JavaScript views on the scratch page must be refreshed after the zstd
     allocator grows the memory, which detaches the underlying ArrayBuffer. *)

let payload_len = 120_000

(* [Compression.output_value] of [payload], i.e. a zstd-compressed marshalled
   value.  Decoding it allocates more than the memory initially provides, so
   the zstd allocator has to grow it. *)
let compressed =
  "\132\149\166\189\015\048\135\169\069\001\129\234\050\245\026\040\181\047\253\000\088\061\001\000\248\010\000\001\212\192\097\098\099\100\101\102\103\104\105\106\107\108\109\110\111\112\113\114\115\116\117\118\119\120\121\122\001\000\143\082\119\062\157"

let payload = String.init payload_len (fun i -> Char.chr (Char.code 'a' + (i mod 26)))

let decompress () =
  let s : string = Marshal.from_string compressed 0 in
  assert (String.equal s payload)

(* [payload] is both longer than the scratch page and longer than the
   small-string fast paths of jsstring.wat, so converting it goes through the
   scratch buffer, a page at a time. *)
let convert () =
  let s = Js_of_ocaml.Js.to_string (Js_of_ocaml.Js.string payload) in
  assert (String.equal s payload)

let () =
  assert Compression.compression_supported;
  (* Grows the linear memory, detaching the ArrayBuffer the JavaScript runtime
     holds a view on. *)
  decompress ();
  convert ();
  (* Would fail if the conversion above had written over the zstd data. *)
  decompress ()
