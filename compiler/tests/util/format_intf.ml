module type S = sig
  type ocaml_source

  type js_source

  type ocaml_file

  type js_file

  type cmo_file

  type bc_file

  val read_js : js_file -> js_source

  val read_ocaml : ocaml_file -> ocaml_source

  val write_js : js_source -> js_file

  val write_ocaml : ocaml_source -> ocaml_file

  val js_source_of_string : string -> js_source

  val ocaml_source_of_string : string -> ocaml_source

  val string_of_js_source : js_source -> string

  val string_of_ocaml_source : ocaml_source -> string

  val path_of_ocaml_file : ocaml_file -> string

  val path_of_js_file : js_file -> string

  val path_of_cmo_file : cmo_file -> string

  val path_of_bc_file : bc_file -> string

  val ocaml_file_of_path : string -> ocaml_file

  val js_file_of_path : string -> js_file

  val cmo_file_of_path : string -> cmo_file

  val bc_file_of_path : string -> bc_file
end
