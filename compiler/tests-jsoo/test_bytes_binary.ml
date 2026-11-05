(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Jérôme Vouillon
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

(* Multi-byte accesses to bytes in each of the JavaScript runtime
   representations: a fresh buffer (empty string prefix), a buffer
   partially filled by sequential writes, a buffer backed by a string,
   and a buffer converted to a byte array. *)

let hex b =
  Bytes.iter (fun c -> Printf.printf "%02x" (Char.code c)) b;
  print_newline ()

let write b =
  Bytes.set_int64_le b 0 0x0123456789abcdefL;
  Bytes.set_int32_le b 8 (-2l);
  Bytes.set_int16_le b 12 0x12345;
  Bytes.set_int16_le b 14 (-1);
  Bytes.set_int64_be b 16 Int64.min_int;
  Bytes.set_int32_be b 24 Int32.min_int;
  Bytes.set_int16_be b 28 0xfedc;
  Bytes.set_int16_be b 30 0x7f

let read b =
  Printf.printf
    "%Lx %Lx %lx %lx %d %d %d %d\n"
    (Bytes.get_int64_le b 0)
    (Bytes.get_int64_be b 16)
    (Bytes.get_int32_le b 8)
    (Bytes.get_int32_be b 24)
    (Bytes.get_int16_le b 12)
    (Bytes.get_uint16_le b 14)
    (Bytes.get_int16_be b 28)
    (Bytes.get_uint16_be b 30)

let%expect_test "fresh buffer" =
  let b = Bytes.create 32 in
  write b;
  hex b;
  read b;
  [%expect
    {|
    efcdab8967452301feffffff4523ffff800000000000000080000000fedc007f
    123456789abcdef 8000000000000000 fffffffe 80000000 9029 65535 -292 127
    |}]

let%expect_test "partially filled buffer" =
  let b = Bytes.create 40 in
  for i = 0 to 7 do
    Bytes.set b i (Char.chr (0xf0 + i))
  done;
  Printf.printf
    "%Lx %lx %d\n"
    (Bytes.get_int64_le b 0)
    (Bytes.get_int32_be b 4)
    (Bytes.get_uint16_le b 6);
  let b' = Bytes.sub b 8 32 in
  Bytes.set_int32_le b 8 0x01020304l;
  Bytes.set_int64_le b 12 (-1L);
  hex (Bytes.sub b 0 20);
  write b';
  hex b';
  read b';
  [%expect
    {|
    f7f6f5f4f3f2f1f0 f4f5f6f7 63478
    f0f1f2f3f4f5f6f704030201ffffffffffffffff
    efcdab8967452301feffffff4523ffff800000000000000080000000fedc007f
    123456789abcdef 8000000000000000 fffffffe 80000000 9029 65535 -292 127
    |}]

let%expect_test "string-backed buffer" =
  let b =
    Bytes.of_string
      "\xef\xcd\xab\x89\x67\x45\x23\x01\xfe\xff\xff\xff\x45\x23\xff\xff\x80\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\xfe\xdc\x00\x7f"
  in
  read b;
  Bytes.set_int32_le b 8 0x7fffffffl;
  Bytes.set_int16_le b 12 (-2);
  read b;
  [%expect
    {|
    123456789abcdef 8000000000000000 fffffffe 80000000 9029 65535 -292 127
    123456789abcdef 8000000000000000 7fffffff 80000000 -2 65535 -292 127
    |}]

let%expect_test "byte array buffer" =
  let b = Bytes.make 32 '\000' in
  Bytes.set b 5 'x';
  write b;
  hex b;
  read b;
  [%expect
    {|
    efcdab8967452301feffffff4523ffff800000000000000080000000fedc007f
    123456789abcdef 8000000000000000 fffffffe 80000000 9029 65535 -292 127
    |}]
