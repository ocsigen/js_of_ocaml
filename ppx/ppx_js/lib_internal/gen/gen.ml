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
  print_endline
    {|
open StdLabels
open Migrate_parsetree
open OCaml_406.Ast
open! Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open! Ast_convenience_406
|}

let () =
  let v = split_on_char ~sep:'.' Sys.ocaml_version in
  if v < ["4"; "08"]
  then
    print_endline
      {|
module Ast_mapper = struct
  let rec extension_of_error ({loc; msg; if_highlight; sub} : Location.error) =
    { loc; txt = "ocaml.error" },
    PStr ([Str.eval (Exp.constant (Pconst_string (msg, None)));
           Str.eval (Exp.constant (Pconst_string (if_highlight, None)))] @
          (List.map ~f:(fun ext -> Str.extension (extension_of_error ext)) sub))
end
module Location = struct
  let msg = Location.errorf
end
|}
  else
    print_endline
      {|
module Ast_mapper = struct
  let extension_of_error ({kind; main; sub} : Location.error)  =
    if kind <> Location.Report_error then
      raise (Invalid_argument "extension_of_error: expected kind Report_error");
    let str_of_pp pp_msg = Format.asprintf "%t" pp_msg in
                     let extension_of_sub (sub : Location.msg)  =
      { Location.loc = sub.loc; txt = "ocaml.error" },
      Parsetree.(PStr ([Str.eval (Exp.constant (Pconst_string (str_of_pp sub.txt, None)))]))
    in
    { loc = main.loc; txt = "ocaml.error" },
    PStr (Str.eval (Exp.constant (Pconst_string (str_of_pp main.txt, None))) ::
          List.map ~f:(fun msg -> Str.extension (extension_of_sub msg)) sub)
end
module Location = struct
  let msg = Location.msg
end
|}
