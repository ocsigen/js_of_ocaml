
# Runtime representation

This page describes how OCaml values are represented at runtime in JavaScript. Understanding this is useful for debugging, writing JavaScript primitives, and understanding compatibility limitations.


## Overview

Js\_of\_ocaml maps OCaml values to JavaScript values as directly as possible for performance. This differs from the standard OCaml runtime representation.

| --- | --- |
| OCaml type | JavaScript representation |
| `int` | Number (32-bit) |
| `float` | Number (unboxed) |
| `bool` | Number (0 or 1\) |
| `char` | Number (0-255) |
| `string` | Special string object (MlBytes) or JavaScript string |
| `bytes` | Special bytes object (MlBytes) |
| `array` | JavaScript Array with tag |
| `tuple` | JavaScript Array with tag 0 |
| `record` | JavaScript Array with tag 0 |
| `variant` | Number (constant) or Array (with arguments) |
| `int32` | Number (32-bit) |
| `nativeint` | Number (32-bit) |
| `int64` | MlInt64 object |
| `bigarray` | Ml\_Bigarray object |
| `exception` | Array with exception identity |
| `object` | Array with method table and instance variables |

## Integers

OCaml integers are represented as JavaScript numbers. Unlike native OCaml where integers are 31 or 63 bits (with one bit used for tagging), js\_of\_ocaml uses the full 32-bit range.

**Implications**:

- Arithmetic behaves slightly differently at the boundaries
- `max_int` and `min_int` have different values than native OCaml

## Floats

OCaml floats are JavaScript numbers directly.

**Implications**:

- Better performance for float-heavy code
- Polymorphic hash and comparison may give different results than native
- Marshalled floats cannot be unmarshalled by the native OCaml runtime
**Rounding difference**: JavaScript rounds ties away from zero, while native OCaml (libc) rounds ties to even.


## Booleans

OCaml booleans are represented as the numbers 0 (`false`) and 1 (`true`), not JavaScript's `true` and `false`. See [type conversions](./javascript-interop.md#conversions) for converting between them.


## Bytes

OCaml bytes are always represented as MlBytes objects with:

- `t` — tag indicating the encoding
- `l` — length in bytes
- `c` — contents (a JavaScript string or array of bytes)
The contents can be a JavaScript string (more memory efficient) or an array of bytes (for mutation). The runtime converts between these as needed.


## Strings

OCaml strings can be represented as:

- A JavaScript string directly (when `--enable use-js-string` is set, which is the default)
- An MlBytes object (above)
**Important**: Even when represented as a JavaScript string, it encodes a sequence of bytes, not UTF-16 text. Each character's code point represents one byte (0-255). Assuming the bytes are UTF-8 encoded text, use `Js.string` to convert to a native UTF-16 JavaScript string. To convert back, use `Js.to_string`. See [conversions](./javascript-interop.md#conversions).


## Blocks, arrays, records, modules

OCaml heap-allocated values (arrays, tuples, records, variant constructors with arguments) are represented as JavaScript arrays where:

- Index 0 contains the block tag
- Remaining indices contain the fields
For example, `Some 42` becomes `[0, 42]` (tag 0, one field).


## Variants

- **Constant constructors** (no arguments): represented as integers starting from 0
- **Non-constant constructors**: represented as arrays with a tag
```ocaml
type t =
  | A        (* 0 *)
  | B        (* 1 *)
  | C of int (* [0, n] — tag 0, since it's the first non-constant *)
  | D of int (* [1, n] — tag 1 *)
```

## Int32 and Nativeint

`Int32.t` and `Nativeint.t` are represented directly as JavaScript numbers, like regular `int` values.

Note that `nativeint` is 32 bits in js\_of\_ocaml (matching JavaScript's bitwise operations), not the platform's native word size.


## Int64

`Int64.t` values are represented as MlInt64 objects, which store the value as three numbers: two 24-bit integers (lo, mi) and one 16-bit integer (hi), since JavaScript numbers cannot represent the full 64-bit range precisely.


## Bigarray

Bigarrays are represented as Ml\_Bigarray objects, which use JavaScript Typed Arrays for storing the underlying data:

| --- | --- |
| OCaml kind | JavaScript Typed Array |
| `Float32` | `Float32Array` |
| `Float64` | `Float64Array` |
| `Int8_signed` | `Int8Array` |
| `Int8_unsigned` | `Uint8Array` |
| `Int16_signed` | `Int16Array` |
| `Int16_unsigned` | `Uint16Array` |
| `Int32` | `Int32Array` |
| `Int64` | Two `Int32Array`s |
| `Int` | `Int32Array` |
| `Nativeint` | `Int32Array` |
| `Complex32` | `Float32Array` (2 elements per complex) |
| `Complex64` | `Float64Array` (2 elements per complex) |
| `Char` | `Uint8Array` |

## Exceptions

OCaml exceptions are represented as:

- **Without arguments**: the exception identity directly (a unique object per exception type)
- **With arguments**: a JavaScript array where index 1 is the exception identity and remaining indices contain the arguments
For example, `Not_found` is just its identity object, while `Failure "oops"` becomes `[<Failure identity>, "oops"]`.

**Note**: OCaml exceptions are not JavaScript `Error` objects. When an OCaml exception propagates to JavaScript code, it appears as an array, not an `Error`. See [error handling](./errors.md) for how to work with exceptions across the OCaml/JavaScript boundary.


## Objects

OCaml objects (from the object-oriented layer) are implemented using JavaScript arrays. They have nothing to do with JavaScript objects. Use the [PPX syntax](./ppx.md) (`object%js`) to create JavaScript objects.


## Compatibility considerations

Due to representation differences, some operations behave differently:

**Marshalling (`Marshal` module)**:

- Output may differ from native OCaml
- Do not unmarshal js\_of\_ocaml output with native OCaml (especially floats)
- Marshalling between js\_of\_ocaml programs works correctly
**Polymorphic comparison and hashing**:

- Results may differ from native OCaml for floats

## See also

- [Type conversions](./javascript-interop.md#conversions) — Converting between OCaml and JavaScript types
- [JavaScript primitives](./linker.md) — Writing custom primitives