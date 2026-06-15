
# JSON derivation

The `js_of_ocaml-ppx_deriving_json` package provides a PPX deriver for serializing OCaml values to JSON.

**Important**: The serialization format follows js\_of\_ocaml's [internal representation](./runtime-representation.md). It is designed for communication between a js\_of\_ocaml program and a server-side OCaml application, not for interacting with third-party APIs.


## Installation

```
opam install js_of_ocaml-ppx_deriving_json
```

## Usage

Add `[@@deriving json]` to your type definitions:

```ocaml
type person = {
  name : string;
  age : int;
}
[@@deriving json]

(* Serialize to JSON *)
let json_str = person_to_json { name = "Alice"; age = 30 }

(* Deserialize from JSON *)
let alice = person_of_json json_str
```
The `[@@deriving json]` attribute generates:

- `person_of_json` — deserialize from JSON
- `person_to_json` — serialize to JSON
- `person_json` — a `Deriving_Json.t` witness
**Note**: For types named `t`, the prefix is omitted (e.g., `of_json` instead of `t_of_json`).

The `[%to_json: type]` and `[%of_json: type]` syntax also works with any type expression:

```ocaml
let json = [%to_json: int list] [1; 2; 3]
let nums = [%of_json: int list] json
```

## Supported types

The deriver supports:

- Basic types: `int`, `float`, `string`, `bool`
- Options: `'a option`
- Lists and arrays: `'a list`, `'a array`
- Records and variants
- Polymorphic variants
- Recursive types

## See also

- [JSON serialization](./javascript-interop.md#json) — Direct JSON output