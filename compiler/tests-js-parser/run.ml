open Js_of_ocaml_compiler
open Stdlib

let exe =
  match Sys.os_type with
  | "Cygwin" | "Win32" -> fun x -> x ^ ".exe"
  | "Unix" | _ -> fun x -> x

let node = try Sys.getenv "NODE" with Not_found -> exe "node"

let failure_expected = ref false

let flags, files =
  Sys.argv
  |> Array.to_list
  |> List.tl
  |> List.partition ~f:(fun x -> Char.equal (String.get x 0) '-')

let rec ls_files path =
  match Unix.lstat path with
  | { st_kind = S_DIR; _ } ->
      Sys.readdir path
      |> Array.to_list
      |> List.concat_map ~f:(fun name -> ls_files (Filename.concat path name))
  | { st_kind = S_REG; _ } -> [ path ]
  | { st_kind = S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK; _ } -> []

let files =
  List.concat_map files ~f:(fun path ->
      let l = ls_files path in
      List.filter l ~f:(fun name -> Filename.check_suffix name ".js"))

let () = Printf.eprintf "Found %d files\n%!" (List.length files)

let () =
  List.iter flags ~f:(function
      | "-fail" -> failure_expected := true
      | f -> failwith ("unrecognised flag " ^ f))

let unsupported_syntax = ref []

let fail = ref []

let pass = ref []

class clean_loc =
  object
    inherit Js_traverse.map

    method! parse_info _ = Parse_info.zero

    method! loc _ = N
  end

let clean_loc = new clean_loc

let clean_loc p = clean_loc#program p

let p_to_string p =
  let buffer = Buffer.create 100 in
  let pp = Pretty_print.to_buffer buffer in
  let _ = Js_output.program pp p in
  Buffer.contents buffer

let patdiff = false

let vs_explicit = false

let accepted_by_node file =
  try
    let ((ic, oc, ec) as ic_oc) =
      Unix.open_process_full (Printf.sprintf "%s --check %s" node file) [||]
    in
    let pid = Unix.process_full_pid ic_oc in
    let _pid, status = Unix.waitpid [] pid in
    close_in ic;
    close_out oc;
    close_in ec;
    match status with
    | WEXITED 0 -> true
    | WEXITED _ -> false
    | WSIGNALED _ | WSTOPPED _ -> assert false
  with _ ->
    Printf.eprintf "Failed with node %s\n%!" file;
    false

let () =
  let total = List.length files in
  List.iteri files ~f:(fun i filename ->
      let () = Printf.eprintf "%d/%d\r%!" i total in

      let ic = open_in_bin filename in
      let content = In_channel.input_all ic in
      let errors = ref [] in
      let add r = r := (filename, content) :: !r in
      close_in ic;
      try
        let p1 =
          Parse_js.Lexer.of_string
            ~report_error:(fun e -> errors := e :: !errors)
            ~filename
            content
          |> Parse_js.parse
        in
        (match List.rev !errors with
        | [] -> ()
        | l -> if accepted_by_node filename then List.iter ~f:Parse_js.Lexer.print_error l);
        if patdiff
        then (
          let s = p_to_string (clean_loc p1) in
          let jsoo_name = filename ^ ".jsoo" in
          let oc = open_out_bin jsoo_name in
          output_string oc s;
          close_out oc;
          let _ret = Sys.command (Printf.sprintf "patdiff %s %s" filename jsoo_name) in
          ());
        (if vs_explicit
         then
           try
             let explicit =
               Filename.(
                 concat
                   (concat (dirname (dirname filename)) "pass-explicit")
                   (basename filename))
             in
             let ic = open_in_bin explicit in
             let content = In_channel.input_all ic in
             close_in ic;
             let p2 =
               Parse_js.Lexer.of_string ~filename:explicit content |> Parse_js.parse
             in
             let p1 = clean_loc p1 and p2 = clean_loc p2 in
             let p1s = p_to_string p1 and p2s = p_to_string p2 in
             if Poly.(p1 <> p2)
             then
               if String.equal p1s p2s
               then (
                 Printf.printf ">>>>>>> AST MISMATCH %s <<<<<<<<<<\n" filename;
                 Printf.printf "%s\n\n" p1s)
               else (
                 Printf.printf ">>>>>>> MISMATCH %s <<<<<<<<<<\n" filename;
                 Printf.printf "%s\n\n%s\n" p1s p2s)
           with _ -> ());
        add pass
      with
      | Parse_js.Parsing_error loc ->
          if not (accepted_by_node filename)
          then add unsupported_syntax
          else fail := (filename, loc, content) :: !fail
      | e -> Printf.eprintf "Unexpected error %s\n%s\n" filename (Printexc.to_string e));
  Printf.printf "Summary:\n";
  Printf.printf "  skip : %d\n" (List.length !unsupported_syntax);
  Printf.printf "  fail : %d\n" (List.length !fail);
  Printf.printf "  pass : %d\n" (List.length !pass);
  let l = !fail in
  if !failure_expected
  then (
    List.iter !pass ~f:(fun (f, c) ->
        Printf.printf "succeded to parse %s\n" f;
        Printf.printf "%s\n\n" c);
    match !pass with
    | [] -> exit 0
    | _ -> exit 1)
  else (
    List.iter l ~f:(fun (f, (pi : Parse_info.t), c) ->
        Printf.printf "failed to parse %s:%d:%d\n" f pi.line pi.col;
        List.iteri (String.split_on_char ~sep:'\n' c) ~f:(fun i c ->
            if i + 1 = pi.line
            then (
              let b = Buffer.create (String.length c) in
              String.fold_utf_8 c () ~f:(fun () i u ->
                  if i = pi.col then Buffer.add_utf_8_uchar b (Uchar.of_int 0x274C);
                  Buffer.add_utf_8_uchar b u);
              Printf.printf "%s\n" (Buffer.contents b))
            else Printf.printf "%s\n" c);
        Printf.printf "\n");
    match !fail with
    | [] -> exit 0
    | _ -> exit 1)
