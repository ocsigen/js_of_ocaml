open Js_of_ocaml_compiler
open Stdlib

let failure_expected = ref false

let flags, files =
  Sys.argv
  |> Array.to_list
  |> List.tl
  |> List.partition ~f:(fun x -> Char.equal (String.get x 0) '-')

let () =
  List.iter flags ~f:(function
      | "-fail" -> failure_expected := true
      | f -> failwith ("unrecognised flag " ^ f))

let unsupported_syntax = ref []

let fail = ref []

let pass = ref []

let rs =
  [ Str.regexp_string "class"
  ; Str.regexp_string "import"
  ; Str.regexp_string "export"
  ; Str.regexp_string "new.target"
  ; Str.regexp_string "with"
  ]

let has_unsupported_syntax c =
  List.exists rs ~f:(fun r ->
      try
        let (_ : int) = Str.search_forward r c 0 in
        true
      with Not_found -> false)

class clean_loc =
  object
    inherit Js_traverse.map

    method! loc _ = N
  end

let clean_loc = new clean_loc

let _clean_loc p = clean_loc#program p

let p_to_string p =
  let buffer = Buffer.create 100 in
  let pp = Pretty_print.to_buffer buffer in
  let _ = Js_output.program pp p in
  Buffer.contents buffer

let () =
  List.iter files ~f:(fun filename ->
      let ic = open_in_bin filename in
      let content = In_channel.input_all ic in
      let add r = r := (filename, content) :: !r in
      close_in ic;
      try
        let p1 = Parse_js.Lexer.of_string ~filename content |> Parse_js.parse in
        (if false
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
            let p1s = p_to_string p1 and p2s = p_to_string p2 in
            if not (String.equal p1s p2s)
            then (
              Printf.printf ">>>>>>> MISMATCH %s <<<<<<<<<<\n" filename;
              Printf.printf "%s\n\n%s\n" p1s p2s)
          with _ -> ());
        add pass
      with Parse_js.Parsing_error loc ->
        if has_unsupported_syntax content
        then add unsupported_syntax
        else fail := (filename, loc, content) :: !fail);
  Printf.printf "Summary:\n";
  Printf.printf "  skip : %d\n" (List.length !unsupported_syntax);
  Printf.printf "  fail : %d\n" (List.length !fail);
  Printf.printf "  pass : %d\n" (List.length !pass);
  let l = !fail in
  if !failure_expected
  then
    List.iter !pass ~f:(fun (f, c) ->
        Printf.printf "succeded to parse %s\n" f;
        Printf.printf "%s\n\n" c)
  else
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
        Printf.printf "\n")
