
# Class type `Crypto.subtleCrypto`

```ocaml
method encrypt : algorithm ->
  cryptoKey Js_of_ocaml__.Js.t ->
  bufferSource ->
  Js_of_ocaml__.Typed_array.arrayBuffer Js_of_ocaml__.Js.t
    Js_of_ocaml__.Promise.t
    Js_of_ocaml__.Js.meth
```
```ocaml
method decrypt : algorithm ->
  cryptoKey Js_of_ocaml__.Js.t ->
  bufferSource ->
  Js_of_ocaml__.Typed_array.arrayBuffer Js_of_ocaml__.Js.t
    Js_of_ocaml__.Promise.t
    Js_of_ocaml__.Js.meth
```
```ocaml
method sign : algorithm ->
  cryptoKey Js_of_ocaml__.Js.t ->
  bufferSource ->
  Js_of_ocaml__.Typed_array.arrayBuffer Js_of_ocaml__.Js.t
    Js_of_ocaml__.Promise.t
    Js_of_ocaml__.Js.meth
```
```ocaml
method verify : algorithm ->
  cryptoKey Js_of_ocaml__.Js.t ->
  bufferSource ->
  bufferSource ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```
```ocaml
method digest : algorithm ->
  bufferSource ->
  Js_of_ocaml__.Typed_array.arrayBuffer Js_of_ocaml__.Js.t
    Js_of_ocaml__.Promise.t
    Js_of_ocaml__.Js.meth
```
```ocaml
method generateKey : algorithm ->
  bool Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.js_array
    Js_of_ocaml__.Js.t ->
  cryptoKey Js_of_ocaml__.Js.t Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```
Generate a single key (AES, HMAC, …).

```ocaml
method generateKey_pair : algorithm ->
  bool Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.js_array
    Js_of_ocaml__.Js.t ->
  cryptoKeyPair Js_of_ocaml__.Js.t Js_of_ocaml__.Promise.t
    Js_of_ocaml__.Js.meth
```
Generate a key pair (RSA, EC, …).

```ocaml
method deriveKey : algorithm ->
  cryptoKey Js_of_ocaml__.Js.t ->
  algorithm ->
  bool Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.js_array
    Js_of_ocaml__.Js.t ->
  cryptoKey Js_of_ocaml__.Js.t Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```
```ocaml
method deriveBits : algorithm ->
  cryptoKey Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Typed_array.arrayBuffer Js_of_ocaml__.Js.t
    Js_of_ocaml__.Promise.t
    Js_of_ocaml__.Js.meth
```
Derive bits using the algorithm's default length.

```ocaml
method deriveBits_length : algorithm ->
  cryptoKey Js_of_ocaml__.Js.t ->
  int ->
  Js_of_ocaml__.Typed_array.arrayBuffer Js_of_ocaml__.Js.t
    Js_of_ocaml__.Promise.t
    Js_of_ocaml__.Js.meth
```
`deriveBits_length algorithm baseKey length` derives `length` bits.

```ocaml
method importKey : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  bufferSource ->
  algorithm ->
  bool Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.js_array
    Js_of_ocaml__.Js.t ->
  cryptoKey Js_of_ocaml__.Js.t Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```
Import a key from raw bytes (the `raw`, `spki` and `pkcs8` formats).

```ocaml
method importKey_jwk : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  jsonWebKey ->
  algorithm ->
  bool Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.js_array
    Js_of_ocaml__.Js.t ->
  cryptoKey Js_of_ocaml__.Js.t Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```
Import a key from a `jsonWebKey` (the `jwk` format).

```ocaml
method exportKey : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  cryptoKey Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Typed_array.arrayBuffer Js_of_ocaml__.Js.t
    Js_of_ocaml__.Promise.t
    Js_of_ocaml__.Js.meth
```
Export to raw bytes (the `raw`, `spki` and `pkcs8` formats).

```ocaml
method exportKey_jwk : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  cryptoKey Js_of_ocaml__.Js.t ->
  jsonWebKey Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```
Export to a `jsonWebKey` (the `jwk` format).

```ocaml
method wrapKey : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  cryptoKey Js_of_ocaml__.Js.t ->
  cryptoKey Js_of_ocaml__.Js.t ->
  algorithm ->
  Js_of_ocaml__.Typed_array.arrayBuffer Js_of_ocaml__.Js.t
    Js_of_ocaml__.Promise.t
    Js_of_ocaml__.Js.meth
```
```ocaml
method unwrapKey : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  bufferSource ->
  cryptoKey Js_of_ocaml__.Js.t ->
  algorithm ->
  algorithm ->
  bool Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.js_array
    Js_of_ocaml__.Js.t ->
  cryptoKey Js_of_ocaml__.Js.t Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```