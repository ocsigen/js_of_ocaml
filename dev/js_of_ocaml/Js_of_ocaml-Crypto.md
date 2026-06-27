
# Module `Js_of_ocaml.Crypto`

Web Crypto API.

A code example:

```ocaml
  let hash data =
    Promise.map
      (fun buf -> buf)
      ((Crypto.subtle ())##digest
         (Crypto.algorithm (Js.string "SHA-256"))
         (Crypto.buffer_source_of_array_buffer_view data))
```
Most of `SubtleCrypto` is exposed as object methods, mirroring the JavaScript surface: obtain the `subtle` object with [`subtle`](./#val-subtle) and call its methods with `##`.

see [https://developer.mozilla.org/en-US/docs/Web/API/Web\_Crypto\_API](https://developer.mozilla.org/en-US/docs/Web/API/Web_Crypto_API) 
see [https://w3c.github.io/webcrypto/](https://w3c.github.io/webcrypto/) 

## Common types

```ocaml
type algorithm
```
An `AlgorithmIdentifier`: either a bare name (e.g. `"SHA-256"`) or a parameter object (e.g. `{name: "AES-GCM"; iv: ...}`). Build one with [`algorithm`](./#val-algorithm) or [`algorithm_obj`](./#val-algorithm_obj).

```ocaml
val algorithm : Js.js_string Js.t -> algorithm
```
`algorithm name` is the bare-name form of an algorithm identifier, such as `algorithm (Js.string "SHA-256")`.

```ocaml
val algorithm_obj : < .. > Js.t -> algorithm
```
`algorithm_obj o` uses an arbitrary parameter object as an algorithm identifier. Build `o` with [`Js.Unsafe.obj`](./Js_of_ocaml-Js-Unsafe.md#val-obj) or the `object%js` syntax.

```ocaml
type bufferSource
```
A `BufferSource`: an `ArrayBuffer` or any `ArrayBufferView` (typed array or `DataView`). Build one with [`buffer_source_of_array_buffer`](./#val-buffer_source_of_array_buffer) or [`buffer_source_of_array_buffer_view`](./#val-buffer_source_of_array_buffer_view).

```ocaml
val buffer_source_of_array_buffer : 
  Typed_array.arrayBuffer Js.t ->
  bufferSource
```
```ocaml
val buffer_source_of_array_buffer_view : 
  Typed_array.arrayBufferView Js.t ->
  bufferSource
```

## Key types, usages and formats

```ocaml
module Key_type : sig ... end
```
The `KeyType` of a [`cryptoKey`](./Js_of_ocaml-Crypto-class-type-cryptoKey.md).

```ocaml
module Key_usage : sig ... end
```
A `KeyUsage`: an operation a [`cryptoKey`](./Js_of_ocaml-Crypto-class-type-cryptoKey.md) may be used for.

```ocaml
module Key_format : sig ... end
```
A `KeyFormat` for importing and exporting keys.


## Keys

```ocaml
class type  cryptoKey = object ... end
```
```ocaml
class type  cryptoKeyPair = object ... end
```
```ocaml
type jsonWebKey = Js.Unsafe.any
```
A key in `JsonWebKey` form (RFC 7517\): a plain JSON object whose members carry the key material (e.g. `kty`, `n`, `e` for RSA; `crv`, `x`, `y` for EC). Exposed as [`Js.Unsafe.any`](./Js_of_ocaml-Js-Unsafe.md#type-any): read its members with [`Js.Unsafe.get`](./Js_of_ocaml-Js-Unsafe.md#val-get), build one with [`Js.Unsafe.obj`](./Js_of_ocaml-Js-Unsafe.md#val-obj), and serialise/parse with the `JSON` global.


## Typed algorithm parameters

```ocaml
type params = 
  | RSASSA_PKCS1_v1_5 (* Sign/verify; parameters are just the name. *)
  | RSA_PSS of {
    salt_length : int;
  }
  | RSA_OAEP of {
    label : bufferSource option;
  }
  | ECDSA of {
    hash : algorithm;
  }
  | ECDH of {
    public : cryptoKey Js.t;
  }
  | AES_CTR of {
    counter : bufferSource;
    length : int;
  }
  | AES_CBC of {
    iv : bufferSource;
  }
  | AES_GCM of {
    iv : bufferSource;
    additional_data : bufferSource option;
    tag_length : int option;
  }
  | AES_KW (* Key-wrapping; parameters are just the name. *)
  | HMAC (* Sign/verify; parameters are just the name. *)
  | HKDF of {
    hash : algorithm;
    salt : bufferSource;
    info : bufferSource;
  }
  | PBKDF2 of {
    hash : algorithm;
    salt : bufferSource;
    iterations : int;
  }
  | RSA_keygen of {
    name : Js.js_string Js.t; (* "RSASSA-PKCS1-v1_5", "RSA-PSS" or "RSA-OAEP". *)
    modulus_length : int;
    public_exponent : Typed_array.uint8Array Js.t;
    hash : algorithm;
  } (* RsaHashedKeyGenParams for subtleCrypto.generateKey. *)
  | RSA_import of {
    name : Js.js_string Js.t; (* "RSASSA-PKCS1-v1_5", "RSA-PSS" or "RSA-OAEP". *)
    hash : algorithm;
  } (* RsaHashedImportParams for subtleCrypto.importKey. *)
  | EC_keygen of {
    name : Js.js_string Js.t; (* "ECDSA" or "ECDH". *)
    named_curve : Js.js_string Js.t;
  } (* EcKeyGenParams. *)
  | AES_keygen of {
    name : Js.js_string Js.t; (* "AES-CTR", "AES-CBC", "AES-GCM" or "AES-KW". *)
    length : int;
  } (* AesKeyGenParams. *)
  | HMAC_keygen of {
    hash : algorithm;
    length : int option;
  } (* HmacKeyGenParams. *)
  | SHA_1
  | SHA_256
  | SHA_384
  | SHA_512
  | Other of Js.js_string Js.t (* An algorithm whose name is not recognised. *)
```
A typed view of an [`algorithm`](./#val-algorithm) identifier, with one constructor per WebCrypto algorithm-parameter dictionary. The nullary constructors map to a bare name string; the others map to a parameter object. `hash` fields are themselves [`algorithm`](./#val-algorithm) values (usually a digest name such as [`to_algorithm`](./#val-to_algorithm) of [`SHA_256`](./#type-params.SHA_256)).

```ocaml
val to_algorithm : params -> algorithm
```
Build a JS `AlgorithmIdentifier` from typed [`params`](./#type-params).

```ocaml
val of_algorithm : algorithm -> params
```
Recover typed [`params`](./#type-params) from a JS `AlgorithmIdentifier` by dispatching on its `name`. Key-generation and import parameter objects are told apart from same-named operation parameters by which fields are present (e.g. `modulusLength` vs `hash` for RSA, `namedCurve` for EC, `iv`/`counter` vs `length` for AES). An unrecognised name maps to [`Other`](./#type-params.Other).


## SubtleCrypto

```ocaml
class type  subtleCrypto = object ... end
```

## Crypto

```ocaml
class type  crypto = object ... end
```
```ocaml
val crypto : unit -> crypto Js.t
```
The `crypto` global (`globalThis.crypto`).

```ocaml
val subtle : unit -> subtleCrypto Js.t
```
`(crypto ())##.subtle`. Only available in a secure context.

```ocaml
val is_supported : unit -> bool
```
Whether the `crypto` global is available in the current environment.
