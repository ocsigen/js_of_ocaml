(* With JavaScript strings, [Bytes.unsafe_of_string] copies the string. The
   [String] functions which read a string through it are inlined and their
   reads performed on the string instead (Inline, Specialize_string_reads),
   or, with separate compilation, made to read only the bytes they need.
   Check that they still compute the right results, including the bounds
   checks. *)

let s =
  Sys.opaque_identity
    (String.concat "" [ "h\xc3\xa9llo, w\xe2\x82\xacrld"; "\x00\xd8\x3d\xde\x00!" ])

let p fmt = Printf.printf fmt

let catch f =
  try f () with Invalid_argument msg | Failure msg -> p "Invalid_argument %s\n" msg

exception E of bytes

let () =
  p "sub: %S %S\n" (String.sub s 1 3) (String.sub s 0 0);
  catch (fun () -> p "%S\n" (String.sub s 10 100));
  p "split: %s\n" (String.concat "|" (String.split_on_char 'l' s));
  p
    "exists: %b for_all: %b\n"
    (String.exists (( = ) ',') s)
    (String.for_all (( <> ) '!') s);
  p "fold: %d\n" (String.fold_left (fun a c -> a + Char.code c) 0 s);
  p "seq: %d\n" (Seq.length (String.to_seq s));
  p "utf8 valid: %b\n" (String.is_valid_utf_8 s);
  let d = String.get_utf_8_uchar s 1 in
  p "utf8: %x %d\n" (Uchar.to_int (Uchar.utf_decode_uchar d)) (Uchar.utf_decode_length d);
  catch (fun () -> ignore (String.get_utf_8_uchar s 100));
  let d = String.get_utf_16be_uchar s (String.length s - 6) in
  p
    "utf16be: %x %d\n"
    (Uchar.to_int (Uchar.utf_decode_uchar d))
    (Uchar.utf_decode_length d);
  let d = String.get_utf_16le_uchar s 0 in
  p "utf16le: %x\n" (Uchar.to_int (Uchar.utf_decode_uchar d));
  p
    "int8: %d uint16: %d %d int16: %d\n"
    (String.get_int8 s 2)
    (String.get_uint16_le s 1)
    (String.get_uint16_be s 1)
    (String.get_int16_be s 1);
  p "int32: %lx %lx\n" (String.get_int32_le s 3) (String.get_int32_be s 3);
  p "int64: %Lx\n" (String.get_int64_le s 4);
  catch (fun () -> ignore (String.get_int64_le s (String.length s - 4)));
  (* Marshal.from_string reads the value at the given offset *)
  let m1 = Marshal.to_string (1, "abc", [ 2.5 ]) [] in
  let m2 = Marshal.to_string ("second", Some 42) [] in
  let m = Sys.opaque_identity ("xyz" ^ m1 ^ m2 ^ String.make 10 'z') in
  let (a, b, c) : int * string * float list = Marshal.from_string m 3 in
  let (d, e) : string * int option = Marshal.from_string m (3 + String.length m1) in
  p "marshal: %d %s %g %s %d\n" a b (List.hd c) d (Option.get e);
  (* [total_size] rather than [data_size]: the header size depends on the
     OCaml version *)
  p "total_size: %d\n" (Marshal.total_size (Bytes.unsafe_of_string m) 3);
  catch (fun () ->
      ignore
        (Marshal.from_string (String.sub m1 0 (String.length m1 - 1)) 0
          : int * string * float list));
  (* An exception argument of type bytes (printed differently depending on
     the representation of strings) *)
  p "printexc: %b\n" (String.length (Printexc.to_string (E (Bytes.of_string "abc"))) > 0)
