(* fallback for older version of the stdlib *)
let getenv_opt a =
  try Some (Sys.getenv a) with
  | Not_found -> None

open Sys

let () =
  match getenv "A" with
  | "A" -> ()
  | _ -> assert false

let () =
  match getenv "B" with
  | exception Not_found -> ()
  | _ -> assert false

let () =
  match getenv_opt "A" with
  | Some "A" -> ()
  | _ -> assert false

let () =
  match getenv_opt "B" with
  | None -> ()
  | Some _ -> assert false


let () = assert("\u{00b1}" = (getenv "D"))
