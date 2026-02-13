external caml_create_file : string -> string -> unit = "caml_create_file"

external caml_read_file_content : string -> string = "caml_read_file_content"

let () =
  (* Test caml_create_file + caml_read_file_content *)
  caml_create_file "/static/test.txt" "hello from virtual fs";
  let content = caml_read_file_content "/static/test.txt" in
  Printf.printf "read_file_content: %s\n" content;
  (* Test Sys.file_exists on virtual files *)
  Printf.printf "file_exists /static/test.txt: %b\n" (Sys.file_exists "/static/test.txt");
  Printf.printf "file_exists /static: %b\n" (Sys.file_exists "/static");
  Printf.printf "file_exists /nonexistent: %b\n" (Sys.file_exists "/nonexistent");
  (* Test Sys.is_directory on virtual dirs *)
  Printf.printf "is_directory /static: %b\n" (Sys.is_directory "/static");
  (* Test reading via standard I/O *)
  let ic = open_in "/static/test.txt" in
  let len = in_channel_length ic in
  let buf = really_input_string ic len in
  close_in ic;
  Printf.printf "open_in read: %s\n" buf;
  (* Test Sys.readdir on virtual directory *)
  let entries = Sys.readdir "/static" in
  Array.sort String.compare entries;
  Printf.printf "readdir /static:";
  Array.iter (fun e -> Printf.printf " %s" e) entries;
  print_newline ();
  (* Test reading a non-existent file raises *)
  (try
     ignore (caml_read_file_content "/nonexistent");
     print_endline "ERROR: should have raised"
   with Sys_error msg -> Printf.printf "expected error: %s\n" msg);
  print_endline "done"
