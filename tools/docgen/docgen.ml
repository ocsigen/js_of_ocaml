open StdLabels
open Sexplib

let find ~path ~ext =
  Array.to_list (Sys.readdir path)
  |> List.filter ~f:(fun name -> Filename.check_suffix name ext)
  |> List.map ~f:(Filename.concat path)

let find_mli = find ~ext:".mli"

let find_ml = find ~ext:".ml"

let find_objs = find ~ext:".objs"

let ignore_pp =
  List.filter ~f:(fun name ->
      let name = Filename.chop_extension name in
      not (Filename.check_suffix name ".pp") )

let read_file ~path =
  let ic = open_in path in
  let len = in_channel_length ic in
  really_input_string ic len

let find_package =
  Findlib.init ();
  fun pkg -> Findlib.package_directory pkg

module Dune = struct
  type t =
    { lib_name : string
    ; public_name : string
    ; deps : string list
    ; wrapped : bool }

  let rec find_map ~f l =
    match l with
    | [] -> None
    | x :: xs -> ( match f x with None -> find_map ~f xs | Some x -> Some x )

  let read ~path =
    let sexps = Sexp.load_sexps (Filename.concat path "dune") in
    let lib =
      match
        find_map sexps ~f:(function
            | Sexp.List (Atom "library" :: l) -> Some l
            | _ -> None )
      with
      | Some l -> l
      | None -> failwith "no library found"
    in
    let lib_name =
      match
        find_map lib ~f:(function
            | Sexp.List [Atom "name"; Atom name] -> Some name
            | _ -> None )
      with
      | Some name -> name
      | None -> failwith "no name for library"
    in
    let public_name =
      match
        find_map lib ~f:(function
            | Sexp.List [Atom "public_name"; Atom name] -> Some name
            | _ -> None )
      with
      | Some name -> name
      | None -> failwith "no name for library"
    in
    let deps =
      match
        find_map lib ~f:(function
            | Sexp.List (Atom "libraries" :: l) -> Some l
            | _ -> None )
      with
      | Some l ->
          List.map l ~f:(function Sexp.Atom n -> n | _ -> failwith "complex deps")
      | None -> []
    in
    let wrapped =
      match
        find_map lib ~f:(function
            | Sexp.List [Atom "wrapped"; Atom b] -> Some (bool_of_string b)
            | Sexp.List [Atom "wrapped"; List [Atom "transition"; _]] -> Some true
            | Sexp.List (Atom "wrapped" :: _) -> failwith "invalid wrapped snippet"
            | _ -> None )
      with
      | Some b -> b
      | None -> true
    in
    {lib_name; wrapped; deps; public_name}
end

let ocamldoc ~generator ~output ~intro =
  [["ocamldoc.opt"]; ["-d"; output]; ["-g"; generator]; ["-intro"; intro]]

let ocamldoc_args ~inc ?opn files =
  [ List.map inc ~f:(fun i -> ["-I"; i]) |> List.concat
  ; (match opn with None -> [] | Some opn -> ["-open"; opn])
  ; files ]
  |> List.concat

let paths = ref []

let wikidoc = ref ""

let intro = ref ""

let () =
  Arg.parse
    ["-wikidoc", Set_string wikidoc, ""; "-intro", Set_string intro, ""]
    (fun s -> paths := !paths @ [s])
    "read source"

let _ =
  if !wikidoc = "" then failwith "please provide -wikidoc";
  if not (Sys.file_exists !wikidoc) then failwith "wikidoc not found";
  if !intro = "" then failwith "please provide -intro";
  let ocamldoc = ocamldoc ~generator:!wikidoc ~output:"." ~intro:!intro in
  let args =
    List.map !paths ~f:(fun path ->
        let path =
          if Filename.check_suffix path "dune" then Filename.dirname path else path
        in
        let dune = Dune.read ~path in
        let mli = find_mli ~path |> ignore_pp in
        let opn =
          if dune.wrapped
          then
            let l = find_ml ~path |> ignore_pp in
            match l with
            | [_] | [] -> None
            | _ :: _ :: _ -> Some (String.capitalize_ascii dune.lib_name ^ "__")
          else None
        in
        let objs = find_objs ~path in
        let packages_dir = List.map (dune.public_name :: dune.deps) ~f:find_package in
        ocamldoc_args ~inc:(packages_dir @ objs) ?opn mli )
  in
  let command = List.flatten (ocamldoc @ args) |> String.concat ~sep:" " in
  print_endline command;
  exit (Sys.command command)
