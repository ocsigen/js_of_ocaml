
# Module `Js_of_ocaml_compiler.Source_map`

```ocaml
module Source_content : sig ... end
```
```ocaml
type map = 
  | Gen of {
    gen_line : int;
    gen_col : int;
  }
  | Gen_Ori of {
    gen_line : int;
    gen_col : int;
    ori_source : int;
    ori_line : int;
    ori_col : int;
  }
  | Gen_Ori_Name of {
    gen_line : int;
    gen_col : int;
    ori_source : int;
    ori_line : int;
    ori_col : int;
    ori_name : int;
  }
```
```ocaml
module Offset : sig ... end
```
```ocaml
module Mappings : sig ... end
```
```ocaml
module Standard : sig ... end
```
```ocaml
module Index : sig ... end
```
```ocaml
type t = 
  | Standard of Standard.t
  | Index of Index.t
```
```ocaml
val to_string : t -> string
```
```ocaml
val to_file : ?rewrite_paths:bool -> t -> string -> unit
```
```ocaml
val of_string : ?tmp_buf:Stdlib.Buffer.t -> string -> t
```
```ocaml
val of_file : ?tmp_buf:Stdlib.Buffer.t -> string -> t
```
```ocaml
val invariant : t -> unit
```
```ocaml
val find_in_js_file : string -> t option
```
```ocaml
type info = {
  mappings : Mappings.decoded;
  sources : string list;
  names : string list;
}
```
```ocaml
module Encoding_spec : sig ... end
```