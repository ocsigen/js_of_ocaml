open Js_of_ocaml_compiler
open Js_of_ocaml_compiler.Stdlib

let read_file f =
  try
    let ic = open_in_bin f in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    Bytes.unsafe_to_string s
  with e ->
    failwith (Printf.sprintf "Cannot read content of %s.\n%s" f (Printexc.to_string e))

let to_stringset utf8_string_set =
  Javascript.IdentSet.fold
    (fun x acc ->
      match x with
      | S { name = Utf8 x; _ } -> StringSet.add x acc
      | V _ -> acc)
    utf8_string_set
    StringSet.empty

let () =
  let () = set_binary_mode_out stdout true in
  match Array.to_list Sys.argv with
  | [] -> assert false
  | _ :: rest ->
      let rest = List.sort_uniq ~compare:String.compare rest in
      let l =
        List.map rest ~f:(fun fname ->
            let name_ident =
              Filename.basename fname
              |> String.split_on_char ~sep:'.'
              |> String.concat ~sep:"_dot_"
              |> String.map ~f:(function
                   | ('a' .. 'z' | 'A' .. 'Z') as i -> i
                   | _ -> '_')
            in
            name_ident, fname, read_file fname)
      in
      List.iter l ~f:(fun (_iname, fname, c) ->
          match Filename.extension fname with
          | ".js" ->
              let p =
                try Parse_js.parse (Parse_js.Lexer.of_string ~filename:fname c)
                with Parse_js.Parsing_error pi ->
                  failwith
                    (Printf.sprintf
                       "cannot parse file %S (l:%d, c:%d)@."
                       fname
                       pi.line
                       pi.col)
              in
              let traverse = new Js_traverse.free in
              let _js = traverse#program p in
              let freenames = to_stringset traverse#get_free in
              let freenames = StringSet.diff freenames Reserved.keyword in
              let freenames = StringSet.diff freenames Reserved.provided in
              if not (StringSet.is_empty freenames)
              then (
                Format.eprintf "warning: free variables in %S@." fname;
                Format.eprintf
                  "vars: %s@."
                  (String.concat ~sep:", " (StringSet.elements freenames)));
              ()
          | _ -> ());
      List.iter l ~f:(fun (n, _, c) ->
          Format.printf "let %s = \"%s\"@." n (String.escaped c))
