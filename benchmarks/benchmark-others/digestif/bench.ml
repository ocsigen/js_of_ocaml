(* Hashes of a 4 MB buffer with the pure OCaml implementation of
   digestif: 32-bit arithmetic (SHA-1, SHA-256, MD5) and 64-bit
   arithmetic (SHA-512) *)

let data = String.init (4 lsl 20) (fun i -> Char.chr (((i * 7) + (i lsr 8)) land 255))

let () =
  for _ = 1 to 2 do
    print_endline Digestif.SHA256.(to_hex (digest_string data));
    print_endline Digestif.SHA1.(to_hex (digest_string data));
    print_endline Digestif.MD5.(to_hex (digest_string data));
    print_endline Digestif.SHA512.(to_hex (digest_string data))
  done
