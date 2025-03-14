(* TEST *)
open Printf

let rec build_string f n accu =
  if n <= 0
    then String.concat "" accu
    else build_string f (n-1) (f (n-1) :: accu)
;;

let char n = String.make 1 (Char.chr n);;

let reference n =
  if n = 8 then "\\b"
  else if n = 9 then "\\t"
  else if n = 10 then "\\n"
  else if n = 13 then "\\r"
  else if n = Char.code '\"' then "\\\""
  else if n = Char.code '\\' then "\\\\"
  else if n < 32 || n > 126 then Printf.sprintf "\\%03d" n
  else char n
;;

let raw_string = build_string char 256 [];;
let ref_string = build_string reference 256 [];;

if String.escaped raw_string <> ref_string then failwith "test:String.escaped";;


let check_split sep s =
  let l = String.split_on_char sep s in
  assert(List.length l > 0);
  assert(String.concat (String.make 1 sep) l = s);
  List.iter (String.iter (fun c -> assert (c <> sep))) l
;;

let () =
  let s = " abc def " in
  for i = 0 to String.length s do
    check_split ' ' (String.sub s 0 i)
  done
;;


let () =
  printf "-- Hashtbl.hash raw_string: %x\n%!" (Hashtbl.hash raw_string);
  printf "-- String.hash raw_string: %x\n%!" (String.hash raw_string);
  printf "-- Hashtbl.seeded_hash 16 raw_string: %x\n%!" (Hashtbl.seeded_hash 16 raw_string);
  printf "-- String.seeded_hash 16 raw_string: %x\n%!" (String.seeded_hash 16 raw_string);
;;

(* GPR#805/815/833 *)
let () =
  if Sys.word_size = 32 && Sys.backend_type = Native || Sys.backend_type = Bytecode
  then begin
    let big = String.make Sys.max_string_length 'x' in
    let push x l = l := x :: !l in
    let (+=) a b = a := !a + b in
    let sz, l = ref 0, ref [] in
    while !sz >= 0 do push big l; sz += Sys.max_string_length done;
    while !sz <= 0 do push big l; sz += Sys.max_string_length done;
    try ignore (String.concat "" !l); assert false
    with Invalid_argument _ -> ();
  end

let () =
    assert(String.starts_with ~prefix:"foob" "foobarbaz");
    assert(String.starts_with ~prefix:"" "foobarbaz");
    assert(String.starts_with ~prefix:"" "");
    assert(not (String.starts_with ~prefix:"foobar" "bar"));
    assert(not (String.starts_with ~prefix:"foo" ""));
    assert(not (String.starts_with ~prefix:"fool" "foobar"));
    assert(String.ends_with ~suffix:"baz" "foobarbaz");
    assert(String.ends_with ~suffix:"" "foobarbaz");
    assert(String.ends_with ~suffix:"" "");
    assert(not (String.ends_with ~suffix:"foobar" "bar"));
    assert(not (String.ends_with ~suffix:"foo" ""));
    assert(not (String.ends_with ~suffix:"obaz" "foobar"));
