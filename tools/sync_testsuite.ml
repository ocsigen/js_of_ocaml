module File = struct
  type t =
    | Ml of string
    | Dir of string
    | Expected of string

  let compare a b = compare a b

  let concat a b =
    match b with
    | Ml x -> Ml (Filename.concat a x)
    | Dir x -> Dir (Filename.concat a x)
    | Expected x -> Expected (Filename.concat a x)
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
         else if String.ends_with ~suffix:".expected" f
         then Some (File.Expected (Filename.chop_suffix f ".expected"))
         else if String.ends_with ~suffix:".reference" f
         then Some (File.Expected (Filename.chop_suffix f ".reference"))
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
    | "afl-instrumentation" (* not relevant *)
    | "arch-power" (* not relevant *)
    | "asmcomp" (* not relevant *)
    | "asmgen" (* not relevant *)
    | "ast-invariants" (* not relevant *)
    | "badly-ordered-deps" (* not relevant *)
    | "basic-multdef" (* What does it test ? *)
    | "c-api" (* not relevant *)
    | "compaction" (* GC *)
    | "compiler-libs" (* not relevant *)
    | "cxx-api" (* not relevant *)
    | "embedded" (* not relevant *)
    | "ephe-c-api" (* not relevant *)
    | "exotic-syntax" (* just syntax *)
    | "flambda" (* not relevant *)
    | "frame-pointers" (* not relevant *)
    | "functors" (* not relevant *)
    | "gc-roots" (* not relevant *)
    | "generalized-open" (* not relevant *)
    | "generated-parse-errors" (* not relevant *)
    | "hidden_includes" (* not relevant *)
    | "int64-unboxing" (* not relevant *)
    | "let-syntax" (* just syntax *)
    | "lexing" (* not relevant *)
    | "link-test" (* not relevant *)
    | "load_path" (* not relevant *)
    | "locale" (* ?? *)
    | "match-exception-warnings" (* not relevant *)
    | "match-side-effects" (* not relevant *)
    | "manual-intf-c" (* not relevant *)
    | "memory-model" (* ?? *)
    | "messages" (* not relevant *)
    | "no-alias-deps" (* not relevant *)
    | "output-complete-obj" (* not relevant *)
    | "parallel" (* ?? *)
    | "parse-errors" (* not relevant *)
    | "parsetree" (* not relevant *)
    | "parsing" (* ?? maybe relevant to test the parsing runtime *)
    | "ppx-attributes" (* not relevant *)
    | "ppx-contexts" (* not relevant *)
    | "reproducibility" (* not relevant *)
    | "required-external" (* ?? *)
    | "runtime-C-exceptions" (* ?? *)
    | "runtime-errors" (* ?? *)
    | "self-contained-toplevel" (* ?? *)
    | "shadow_include" (* not relevant *)
    | "shape-index" (* not relevant *)
    | "shapes" (* not relevant *)
    | "statmemprof" (* not relevant *)
    | "syntactic-arity" (* not relevant *)
    | "translprim" (* not relevant *)
    | "tsan" (* not relevant *)
    | "uid-deps" (* not relevant *)
    | "uids" (* not relevant *)
    | "unboxed-primitive-args" (* ?? *)
    | "unicode" (* not relevant *)
    | "unwind" (* not relevant *)
    | "utils" (* not relevant *)
    | "warnings" (* not relevant *)
    | "weak-ephe-final" (* ?? *)
    | "win-unicode" (* not relevant *)
    | "lf_skiplist" (* not relevant *)
    | "lib-bigarray-2" -> `Ignore
    | "lib-digest/blake2b_self_test.ml" -> `Ignore
    | "lib-bigarray-file" -> `Ignore
    | "lib-random/parallel.ml" | "lib-str/parallel.ml" -> `Ignore
    | "lib-hashtbl/compatibility.ml" -> `Ignore
    | _ -> `No

let () =
  let check_not_ignored path x =
    match _ignore_ (Filename.concat path x) with
    | `No -> ()
    | _ -> failwith (Printf.sprintf "remove %s/%s from ignore" path x)
  in
  let rec diff f a b path =
    let a0 = Filename.concat a path and b0 = Filename.concat b path in
    let ad = readdir a0 and bd = readdir b0 in
    let missing, common, extra = split ad bd in
    FileSet.iter (fun x -> f (`Missing (File.concat path x))) missing;
    FileSet.iter (fun x -> f (`Extra (File.concat path x))) extra;
    FileSet.iter
      (function
        | Dir x ->
            check_not_ignored path x;
            diff f a b (Filename.concat path x)
        | Ml x ->
            check_not_ignored path x;
            f (`Same (Filename.concat path x))
        | Expected x -> f (`Expected (Filename.concat path x)))
      common
  in
  diff
    (function
      | `Missing (Dir x | Ml x) -> (
          match _ignore_ x with
          | `Tool | `Typing | `Dynlink -> ()
          | `Ignore -> ()
          | `No -> Printf.eprintf "missing %s\n" x)
      | `Missing (Expected x) -> (
          match _ignore_ (x ^ ".ml") with
          | `Tool | `Typing | `Dynlink -> ()
          | `Ignore -> ()
          | `No -> Printf.eprintf "missing expected %s\n" x)
      | `Extra (Ml "testing.ml") -> ()
      | `Extra (Ml "expect.ml") -> ()
      | `Extra (Expected x) -> Printf.eprintf "extra expected %s\n" x
      | `Extra (Dir x | Ml x) -> Printf.eprintf "extra %s\n" x
      | `Same x -> (
          Sys.command
            (Printf.sprintf
               "patdiff %s %s"
               (Filename.concat src x)
               (Filename.concat dst x))
          |> function
          | 0 -> ()
          | _ -> Printf.eprintf "differ %s\n" x)
      | `Expected x -> (
          Sys.command
            (Printf.sprintf
               "patdiff %s.reference %s.expected"
               (Filename.concat src x)
               (Filename.concat dst x))
          |> function
          | 0 -> ()
          | _ -> Printf.eprintf "differ %s\n" x))
    src
    dst
    ""
