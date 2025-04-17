type value =
  | Bool of bool
  | String of string
  | Version of int * int * int

val value_equal : value -> value -> bool

val f : variables:(string * value) list -> filename:string -> contents:string -> string

type source =
  | Binary  (** Binary file (skipped by the preprocessor) *)
  | File  (** Not read yet *)
  | Contents of string  (** File contents to preprocess *)

type input =
  { module_name : string
        (** Name under which the module should be imported (used to fill the [Binary.link_input] record) *)
  ; file : string  (** File originally containing the module *)
  ; source : source
        (** Information about the file, including possibly the already read file contents *)
  }

val with_preprocessed_files :
     variables:(string * value) list
  -> inputs:input list
  -> (Binaryen.link_input list -> 'a)
  -> 'a
