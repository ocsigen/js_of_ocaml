open Pervasives

module Version : sig
  type t
  val parse : string -> t
  val compare : t -> t -> int
end = struct

  type t = int list

  let compint (a : int) b = compare a b

  let rec compare v v' = match v,v' with
    | [x],[y] -> 0
    | [],[] -> 0
    | [],y::_ -> compint 0 y
    | x::_,[] -> compint x 0
    | x::xs,y::ys ->
      match compint x y with
      | 0 -> compare xs ys
      | n -> n

  let split_char sep p =
    let len = String.length p in
    let rec split beg cur =
      if cur >= len then
        if cur - beg > 0
        then [String.sub p beg (cur - beg)]
        else []
      else if p.[cur] = sep then
        String.sub p beg (cur - beg) :: split (cur + 1) (cur + 1)
      else
        split beg (cur + 1) in
    split 0 0

  let parse s =
    match split_char '.' s with
    | [] -> assert false
    | l -> List.map int_of_string l
end

let mkop : string -> (int -> bool) = function
  | "=" -> fun x -> x = 0
  | ">" -> fun x -> x > 0
  | "<" -> fun x -> x < 0
  | "<=" -> fun x -> x <= 0
  | ">=" -> fun x -> x >= 0
  | "<>" -> fun x -> x <> 0
  | _ -> assert false

let log name fmt =
  let len = max 1 (15 - String.length name) in
  let pad = String.make len ' ' in
  Printf.ksprintf (fun s -> Format.eprintf "* %s%s: %s\n" name pad s) fmt

let check_exists ?(o=false) pkg descr =
  let b = try
      let _ = Findlib.package_directory pkg in
      true
    with Fl_package_base.No_such_package(package_name, "") -> false
  in
  let extra =
    if o
    then Printf.sprintf " (optionnal : needed for %s)" descr
    else "" in
  if b
  then begin
    log pkg "OK%s" extra;
    true
  end
  else begin
    log pkg "not found%s" extra; false
  end
let check_version pkg rel version =
  try
    let pkg_version' = Findlib.package_property [] pkg "version" in
    let pkg_version = Version.parse pkg_version' in
    let version_to_match = Version.parse version in
    let op = mkop rel in
    if not (op (Version.compare pkg_version version_to_match))
    then begin
      log pkg "version '%s' doesn't match version constraint : %s%s"
        pkg_version' rel version;
      false
    end
    else begin
      log pkg "OK";
      true
    end
  with Fl_package_base.No_such_package _ ->
    Format.eprintf "* %s : not found" pkg; false


let check_exists_bin ?(o=false) cmd descr =
  try
    let code = Sys.command (Printf.sprintf "type -p %S" cmd) in
    if code = 0
    then (log cmd "OK" ; true)
    else raise Not_found
  with Not_found -> log cmd "not found"; false

let () =
  let mandatory = [
    check_version "lwt" ">=" "2.3.0";
    check_exists_bin "menhir" "";
    check_exists_bin "ocamlfind" ""
  ] in
  let optionnal_install = [
    check_exists ~o:true "graphics" "js_of_ocaml.graphics";
    check_exists ~o:true "deriving" "js_of_ocaml.deriving[.syntax]";
  ] in
  let optionnal_other = [
    check_exists_bin ~o:true "phantomjs" "test";
    check_exists ~o:true "optcomp" "toplevel";
    check_exists ~o:true "ocp-indent" "toplevel";
    check_exists ~o:true "cohttp" "toplevel";
  ] in
  if List.for_all (fun x -> x) mandatory
  then exit 0
  else begin
    Printf.eprintf "One or more requirement are missing\n";
    flush_all();
    exit 1
  end
