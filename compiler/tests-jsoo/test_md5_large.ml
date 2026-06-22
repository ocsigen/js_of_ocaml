(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Regression test for https://github.com/ocsigen/js_of_ocaml/issues/2300

   [caml_MD5Final] used to compute the high word of the 64-bit message bit
   length with [(ctx.len >> 29)], which coerces [ctx.len] to a 32-bit int
   first.  For inputs of at least 2 GiB the bit length was therefore
   truncated and the digest was wrong (reachable e.g. through [Digest.file]
   on a large file).

   The bug can only show up once the byte count reaches 2^31, so we have to
   digest a file that is at least that big.  To keep it cheap the file is
   created sparse: we seek to the last byte and write it, leaving a hole that
   reads back as zeros, so no 2 GiB ever hits memory or disk.

   Even so, hashing 2 GiB takes ~40s in JS, so this test is off by default.
   Enable it with [JSOO_TEST_MD5_LARGE=true] (see [make test-md5-large]). *)

(* The sparse-file trick is not portable to the quickjs / wasi fs shims. *)
let%expect_test ("md5 of a >2GiB input (issue #2300)" [@when (not wasi) && not quickjs]) =
  (* 2^31 + 8 bytes.  This does not fit in an OCaml [int] under js_of_ocaml,
     where [int] is 32 bits, so the size is kept as an [int64] and the actual
     length comes from the filesystem rather than from OCaml. *)
  let size = 0x80000008L in
  (* Digest of [size] zero bytes, computed independently with both
     [python3 hashlib.md5] and coreutils [md5sum]. *)
  let expected = "e1c92af7f054a2edb7cfb939eab3c688" in
  let tmp = Filename.temp_file "jsoo_md5_large" ".bin" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove tmp with Sys_error _ -> ())
    (fun () ->
      let oc = open_out_bin tmp in
      LargeFile.seek_out oc (Int64.pred size);
      output_char oc '\000';
      close_out oc;
      let got = Digest.to_hex (Digest.file tmp) in
      Printf.printf "match: %b\n" (String.equal got expected));
  [%expect {| match: true |}]
