open StdLabels

let split_on_char ~sep s =
  let r = ref [] in
  let j = ref (String.length s) in
  for i = String.length s - 1 downto 0 do
    if String.unsafe_get s i = sep
    then (
      r := String.sub s ~pos:(i + 1) ~len:(!j - i - 1) :: !r;
      j := i)
  done;
  String.sub s ~pos:0 ~len:!j :: !r

let () =
  let v = split_on_char ~sep:'.' Sys.ocaml_version in
  if v < ["4"; "08"]
  then
    print_endline
      {|
let reloc name =
  let buf = Bytes.create 4 in
  Symtable.patch_object buf [Reloc_setglobal (Ident.create_persistent name), 0];
  let get i = Char.code (Bytes.get buf i) in
  get 0 + (get 1 lsl 8) + (get 2 lsl 16) + (get 3 lsl 24)
|}
  else
    print_endline
      {|
let reloc name =
  let buf = Bytes.create 4 in
  Symtable.patch_object [|buf|] [Reloc_setglobal (Ident.create_persistent name), 0];
  let get i = Char.code (Bytes.get buf i) in
  get 0 + (get 1 lsl 8) + (get 2 lsl 16) + (get 3 lsl 24)
|}
