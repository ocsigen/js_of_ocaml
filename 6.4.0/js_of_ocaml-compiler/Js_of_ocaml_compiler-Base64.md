
# Module `Js_of_ocaml_compiler.Base64`

Base64 RFC4648 implementation.

Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation. It is specified in RFC 4648\.

```ocaml
type ('a, 'b) result = 
  | Ok of 'a
  | Error of 'b
```
```ocaml
type alphabet
```
Type of alphabet.

```ocaml
type sub = string * int * int
```
Type of sub-string: `str, off, len`.

```ocaml
val default_alphabet : alphabet
```
A 64-character alphabet specifying the regular Base64 alphabet.

```ocaml
val uri_safe_alphabet : alphabet
```
A 64-character alphabet specifying the URI- and filename-safe Base64 alphabet.

```ocaml
val make_alphabet : string -> alphabet
```
Make a new alphabet.

```ocaml
val length_alphabet : alphabet -> int
```
Returns length of the alphabet, should be 64\.

```ocaml
val alphabet : alphabet -> int array
```
Returns the alphabet.

```ocaml
val decode_exn : 
  ?pad:bool ->
  ?alphabet:alphabet ->
  ?off:int ->
  ?len:int ->
  string ->
  string
```
`decode_exn ?off ?len s` decodes `len` bytes (defaults to `String.length s - off`) of the string `s` starting from `off` (defaults to `0`) that is encoded in Base64 format. Will leave trailing NULLs on the string, padding it out to a multiple of 3 characters. `alphabet` defaults to [`default_alphabet`](./#val-default_alphabet). `pad = true` specifies to check if `s` is padded or not, otherwise, it raises an exception.

Decoder can fail when character of `s` is not a part of `alphabet` or is not `padding` character. If input is not padded correctly, decoder does the best-effort but it does not ensure `decode_exn (encode ~pad:false x) = x`.

raises `Stdlib.Invalid_argument` if s is not a valid Base64 string.
```ocaml
val decode_sub : 
  ?pad:bool ->
  ?alphabet:alphabet ->
  ?off:int ->
  ?len:int ->
  string ->
  (sub, [ `Msg of string ]) result
```
Same as [`decode_exn`](./#val-decode_exn) but it returns a result type instead to raise an exception. Then, it returns a [`sub`](./#type-sub) string. Decoded input `(str, off, len)` will starting to `off` and will have `len` bytes \- by this way, we ensure to allocate only one time result.

```ocaml
val decode : 
  ?pad:bool ->
  ?alphabet:alphabet ->
  ?off:int ->
  ?len:int ->
  string ->
  (string, [ `Msg of string ]) result
```
Same as [`decode_exn`](./#val-decode_exn), but returns an explicit error message [`result`](./#type-result) if it fails.

```ocaml
val encode : 
  ?pad:bool ->
  ?alphabet:alphabet ->
  ?off:int ->
  ?len:int ->
  string ->
  (string, [ `Msg of string ]) result
```
`encode s` encodes the string `s` into base64. If `pad` is false, no trailing padding is added. `pad` defaults to `true`, and `alphabet` to [`default_alphabet`](./#val-default_alphabet).

`encode` fails when `off` and `len` do not designate a valid range of `s`.

```ocaml
val encode_string : ?pad:bool -> ?alphabet:alphabet -> string -> string
```
`encode_string s` encodes the string `s` into base64. If `pad` is false, no trailing padding is added. `pad` defaults to `true`, and `alphabet` to [`default_alphabet`](./#val-default_alphabet).

```ocaml
val encode_sub : 
  ?pad:bool ->
  ?alphabet:alphabet ->
  ?off:int ->
  ?len:int ->
  string ->
  (sub, [ `Msg of string ]) result
```
Same as [`encode`](./#val-encode) but return a [`sub`](./#type-sub)\-string instead a plain result. By this way, we ensure to allocate only one time result.

```ocaml
val encode_exn : 
  ?pad:bool ->
  ?alphabet:alphabet ->
  ?off:int ->
  ?len:int ->
  string ->
  string
```
Same as [`encode`](./#val-encode) but raises an invalid argument exception if we retrieve an error.
