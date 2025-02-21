type value =
  | Bool of bool
  | String of string
  | Version of int * int * int

val f : variables:(string * value) list -> filename:string -> contents:string -> string

type source =
  | File (* Binary file (skipped) *)
  | Contents of string (* Contents to preprocess *)

type input =
  { module_name : string
  ; file : string
  ; source : source
  }

val with_preprocessed_files :
     variables:(string * value) list
  -> inputs:input list
  -> (Binaryen.link_input list -> 'a)
  -> 'a
