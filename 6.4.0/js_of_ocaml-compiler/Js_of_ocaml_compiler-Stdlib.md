
# Module `Js_of_ocaml_compiler.Stdlib`

```ocaml
val open_in_text : string -> Stdlib.in_channel
```
```ocaml
val open_out_text : string -> Stdlib.out_channel
```
```ocaml
module Deprecated : sig ... end
```
```ocaml
val open_in : string -> Stdlib.in_channel
```
deprecated use open\_int\_text/open\_int\_bin
```ocaml
val open_out : string -> Stdlib.out_channel
```
deprecated use open\_out\_text/open\_out\_bin
```ocaml
module Poly : sig ... end
```
```ocaml
module Int_replace_polymorphic_compare : sig ... end
```
```ocaml
val phys_equal : 'a -> 'a -> bool
```
```ocaml
val (==) : [> `use_phys_equal ]
```
```ocaml
val (!=) : [> `use_phys_equal ]
```
```ocaml
val (<) : int -> int -> bool
```
```ocaml
val (<=) : int -> int -> bool
```
```ocaml
val (<>) : int -> int -> bool
```
```ocaml
val (=) : int -> int -> bool
```
```ocaml
val (>) : int -> int -> bool
```
```ocaml
val (>=) : int -> int -> bool
```
```ocaml
val compare : int -> int -> int
```
```ocaml
val equal : int -> int -> bool
```
```ocaml
val max : int -> int -> int
```
```ocaml
val min : int -> int -> int
```
```ocaml
val fail : bool Stdlib.ref
```
```ocaml
val failwith_ : ('a, unit, string, unit) Stdlib.format4 -> 'a
```
```ocaml
val raise_ : exn -> unit
```
```ocaml
module List : sig ... end
```
```ocaml
val (@) : 'a list -> 'a list -> 'a list
```
```ocaml
module Int32 : sig ... end
```
```ocaml
module Int64 : sig ... end
```
```ocaml
module Option : sig ... end
```
```ocaml
module Float : sig ... end
```
```ocaml
module Float32 : sig ... end
```
```ocaml
module Bool : sig ... end
```
```ocaml
module Char : sig ... end
```
```ocaml
module Uchar : sig ... end
```
```ocaml
module Buffer : sig ... end
```
```ocaml
module Bytes = Stdlib.BytesLabels
```
```ocaml
module String : sig ... end
```
```ocaml
module Utf8_string : sig ... end
```
```ocaml
module Int : sig ... end
```
```ocaml
module Set : sig ... end
```
```ocaml
module IntSet : sig ... end
```
```ocaml
module IntMap : sig ... end
```
```ocaml
module StringSet : sig ... end
```
```ocaml
module StringMap : sig ... end
```
```ocaml
module Utf8_string_set : sig ... end
```
```ocaml
module Utf8_string_map : sig ... end
```
```ocaml
module BitSet : sig ... end
```
```ocaml
module Array : sig ... end
```
```ocaml
module Filename : sig ... end
```
```ocaml
module Fun : sig ... end
```
```ocaml
module In_channel : sig ... end
```
```ocaml
module Seq : sig ... end
```
```ocaml
val split_lines : String.t -> string list
```
```ocaml
val input_lines_read_once : Stdlib.in_channel -> int -> string list
```
```ocaml
val file_lines_bin : string -> string list
```
```ocaml
val file_lines_text : string -> string list
```
```ocaml
module Hashtbl : sig ... end
```
```ocaml
module Lexing : sig ... end
```
```ocaml
val with_async_exns : (unit -> 'a) -> 'a
```