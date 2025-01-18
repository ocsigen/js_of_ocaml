module File = struct
  type t =
    | Ml of string
    | Dir of string

  let compare a b = compare a b

  let concat a b =
    match b with
    | Ml x -> Ml (Filename.concat a x)
    | Dir x -> Dir (Filename.concat a x)
end

module FileSet = Set.Make (File)

let src, dst =
  match Sys.argv with
  | [| _; src; dst |] -> src, dst
  | _ -> failwith (Printf.sprintf "%s OCAMLTESTDIR JSOOTESTDIR" Sys.executable_name)

let readdir s =
  Sys.readdir s
  |> Array.to_seq
  |> Seq.filter_map (fun f ->
         if Sys.is_directory (Filename.concat s f)
         then Some (File.Dir f)
         else if String.ends_with ~suffix:".ml" f
         then Some (File.Ml f)
         else None)
  |> FileSet.of_seq

let split a b = FileSet.diff a b, FileSet.inter a b, FileSet.diff b a

let _ignore_ x =
  if String.starts_with ~prefix:"typing-" x
  then `Typing
  else if String.starts_with ~prefix:"tool-" x
  then `Tool
  else if String.starts_with ~prefix:"lib-dynlink-" x
  then `Dynlink
  else
    match x with
    | "lib-either" -> `Expect
    | "lib-array" -> `Expect
    | "lib-bigarray-2" -> `Stubs
    | "lib-digest/blake2b_self_test.ml" -> `Stubs
    | "lib-bigarray-file" -> `Mapfile
    | "lib-lazy" -> `Expect
    | "lib-internalformat" -> `Expect
    | "lib-random/parallel.ml" | "lib-str/parallel.ml" -> `Parallel
    | "lib-hashtbl/compatibility.ml" -> `Old
    | _ -> `No

let () =
  let rec diff f a b path =
    let a0 = Filename.concat a path and b0 = Filename.concat b path in
    let ad = readdir a0 and bd = readdir b0 in
    let missing, common, extra = split ad bd in
    FileSet.iter (fun x -> f (`Missing (File.concat path x))) missing;
    FileSet.iter (fun x -> f (`Extra (File.concat path x))) extra;
    FileSet.iter
      (function
        | Dir x -> diff f a b (Filename.concat path x)
        | Ml x -> f (`Same (Filename.concat path x)))
      common
  in
  diff
    (function
      | `Missing (Dir x | Ml x) -> (
          match _ignore_ x with
          | `Tool | `Typing | `Dynlink | `Expect -> ()
          | `Stubs | `Old -> ()
          | `Parallel -> ()
          | `Mapfile -> ()
          | `No -> Printf.eprintf "missing %s\n" x)
      | `Extra (Dir "effects/double-translation") -> ()
      | `Extra (Ml "testing.ml") -> ()
      | `Extra (Dir x | Ml x) -> Printf.eprintf "extra %s\n" x
      | `Same x -> (
          Sys.command
            (Printf.sprintf
               "patdiff %s %s"
               (Filename.concat src x)
               (Filename.concat dst x))
          |> function
          | 0 -> ()
          | _ -> Printf.eprintf "differ %s\n" x))
    src
    dst
    ""
