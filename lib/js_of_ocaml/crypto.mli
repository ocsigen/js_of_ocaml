(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** Web Crypto API.

    A code example:
    {@ocaml[
      let hash data =
        Promise.map
          (fun buf -> buf)
          ((Crypto.subtle ())##digest
             (Crypto.algorithm (Js.string "SHA-256"))
             (Crypto.buffer_source_of_array_buffer_view data))
    ]}

    Most of {!SubtleCrypto} is exposed as object methods, mirroring the
    JavaScript surface: obtain the [subtle] object with {!subtle} and call its
    methods with [##].

    @see <https://developer.mozilla.org/en-US/docs/Web/API/Web_Crypto_API>
    @see <https://w3c.github.io/webcrypto/> *)

open Js

(** {1 Common types} *)

type algorithm
(** An [AlgorithmIdentifier]: either a bare name (e.g. ["SHA-256"]) or a
    parameter object (e.g. [{name: "AES-GCM"; iv: ...}]). Build one with
    {!algorithm} or {!algorithm_obj}. *)

val algorithm : js_string t -> algorithm
(** [algorithm name] is the bare-name form of an algorithm identifier, such as
    [algorithm (Js.string "SHA-256")]. *)

val algorithm_obj : < .. > t -> algorithm
(** [algorithm_obj o] uses an arbitrary parameter object as an algorithm
    identifier. Build [o] with {!Js.Unsafe.obj} or the [object%js] syntax. *)

type bufferSource
(** A [BufferSource]: an [ArrayBuffer] or any [ArrayBufferView] (typed array or
    [DataView]). Build one with {!buffer_source_of_array_buffer} or
    {!buffer_source_of_array_buffer_view}. *)

val buffer_source_of_array_buffer : Typed_array.arrayBuffer t -> bufferSource

val buffer_source_of_array_buffer_view : #Typed_array.arrayBufferView t -> bufferSource

(** {1 Key types, usages and formats} *)

(** The [KeyType] of a {!cryptoKey}. *)
module Key_type : sig
  type t =
    | Public
    | Private
    | Secret

  val to_js : t -> Js.js_string Js.t

  val of_js : Js.js_string Js.t -> t
  (** @raise Invalid_argument on an unrecognised string. *)
end

(** A [KeyUsage]: an operation a {!cryptoKey} may be used for. *)
module Key_usage : sig
  type t =
    | Encrypt
    | Decrypt
    | Sign
    | Verify
    | Derive_key
    | Derive_bits
    | Wrap_key
    | Unwrap_key

  val to_js : t -> Js.js_string Js.t

  val of_js : Js.js_string Js.t -> t
  (** @raise Invalid_argument on an unrecognised string. *)

  val list_to_js : t list -> Js.js_string Js.t Js.js_array Js.t

  val list_of_js : Js.js_string Js.t Js.js_array Js.t -> t list
end

(** A [KeyFormat] for importing and exporting keys. *)
module Key_format : sig
  type t =
    | Raw
    | Spki
    | Pkcs8
    | Jwk

  val to_js : t -> Js.js_string Js.t

  val of_js : Js.js_string Js.t -> t
  (** @raise Invalid_argument on an unrecognised string. *)
end

(** {1 Keys} *)

class type cryptoKey = object
  method _type : js_string t readonly_prop
  (** The key type: ["secret"], ["private"] or ["public"]. *)

  method extractable : bool t readonly_prop

  method algorithm : algorithm readonly_prop
  (** The algorithm the key is for; pass to {!of_algorithm} to inspect it. *)

  method usages : js_string t js_array t readonly_prop
end

class type cryptoKeyPair = object
  method publicKey : cryptoKey t readonly_prop

  method privateKey : cryptoKey t readonly_prop
end

type jsonWebKey = Unsafe.any
(** A key in [JsonWebKey] form (RFC 7517): a plain JSON object whose members
    carry the key material (e.g. [kty], [n], [e] for RSA; [crv], [x], [y] for
    EC). Exposed as {!Js.Unsafe.any}: read its members with {!Js.Unsafe.get},
    build one with {!Js.Unsafe.obj}, and serialise/parse with the [JSON] global. *)

(** {1 Typed algorithm parameters} *)

(** A typed view of an {!algorithm} identifier, with one constructor per
    WebCrypto algorithm-parameter dictionary. The nullary constructors map to a
    bare name string; the others map to a parameter object. [hash] fields are
    themselves {!algorithm} values (usually a digest name such as
    {!to_algorithm} of {!SHA_256}). *)
type params =
  | RSASSA_PKCS1_v1_5  (** Sign/verify; parameters are just the name. *)
  | RSA_PSS of { salt_length : int }
  | RSA_OAEP of { label : bufferSource option }
  | ECDSA of { hash : algorithm }
  | ECDH of { public : cryptoKey t }
  | AES_CTR of
      { counter : bufferSource
      ; length : int
      }
  | AES_CBC of { iv : bufferSource }
  | AES_GCM of
      { iv : bufferSource
      ; additional_data : bufferSource option
      ; tag_length : int option
      }
  | AES_KW  (** Key-wrapping; parameters are just the name. *)
  | HMAC  (** Sign/verify; parameters are just the name. *)
  | HKDF of
      { hash : algorithm
      ; salt : bufferSource
      ; info : bufferSource
      }
  | PBKDF2 of
      { hash : algorithm
      ; salt : bufferSource
      ; iterations : int
      }
  | RSA_keygen of
      { name : js_string t  (** ["RSASSA-PKCS1-v1_5"], ["RSA-PSS"] or ["RSA-OAEP"]. *)
      ; modulus_length : int
      ; public_exponent : Typed_array.uint8Array t
      ; hash : algorithm
      }  (** [RsaHashedKeyGenParams] for {!subtleCrypto.generateKey}. *)
  | RSA_import of
      { name : js_string t  (** ["RSASSA-PKCS1-v1_5"], ["RSA-PSS"] or ["RSA-OAEP"]. *)
      ; hash : algorithm
      }  (** [RsaHashedImportParams] for {!subtleCrypto.importKey}. *)
  | EC_keygen of
      { name : js_string t  (** ["ECDSA"] or ["ECDH"]. *)
      ; named_curve : js_string t
      }  (** [EcKeyGenParams]. *)
  | AES_keygen of
      { name : js_string t  (** ["AES-CTR"], ["AES-CBC"], ["AES-GCM"] or ["AES-KW"]. *)
      ; length : int
      }  (** [AesKeyGenParams]. *)
  | HMAC_keygen of
      { hash : algorithm
      ; length : int option
      }  (** [HmacKeyGenParams]. *)
  | SHA_1
  | SHA_256
  | SHA_384
  | SHA_512
  | Other of js_string t  (** An algorithm whose name is not recognised. *)

val to_algorithm : params -> algorithm
(** Build a JS [AlgorithmIdentifier] from typed {!params}. *)

val of_algorithm : algorithm -> params
(** Recover typed {!params} from a JS [AlgorithmIdentifier] by dispatching on its
    [name]. Key-generation and import parameter objects are told apart from
    same-named operation parameters by which fields are present (e.g.
    [modulusLength] vs [hash] for RSA, [namedCurve] for EC, [iv]/[counter] vs
    [length] for AES). An unrecognised name maps to {!Other}. *)

(** {1 SubtleCrypto} *)

class type subtleCrypto = object
  method encrypt :
    algorithm -> cryptoKey t -> bufferSource -> Typed_array.arrayBuffer t Promise.t meth

  method decrypt :
    algorithm -> cryptoKey t -> bufferSource -> Typed_array.arrayBuffer t Promise.t meth

  method sign :
    algorithm -> cryptoKey t -> bufferSource -> Typed_array.arrayBuffer t Promise.t meth

  method verify :
    algorithm -> cryptoKey t -> bufferSource -> bufferSource -> bool t Promise.t meth

  method digest : algorithm -> bufferSource -> Typed_array.arrayBuffer t Promise.t meth

  method generateKey :
    algorithm -> bool t -> js_string t js_array t -> cryptoKey t Promise.t meth
  (** Generate a single key (AES, HMAC, …). *)

  method generateKey_pair :
    algorithm -> bool t -> js_string t js_array t -> cryptoKeyPair t Promise.t meth
  (** Generate a key pair (RSA, EC, …). *)

  method deriveKey :
       algorithm
    -> cryptoKey t
    -> algorithm
    -> bool t
    -> js_string t js_array t
    -> cryptoKey t Promise.t meth

  method deriveBits : algorithm -> cryptoKey t -> Typed_array.arrayBuffer t Promise.t meth
  (** Derive bits using the algorithm's default length. *)

  method deriveBits_length :
    algorithm -> cryptoKey t -> int -> Typed_array.arrayBuffer t Promise.t meth
  (** [deriveBits_length algorithm baseKey length] derives [length] bits. *)

  method importKey :
       js_string t
    -> bufferSource
    -> algorithm
    -> bool t
    -> js_string t js_array t
    -> cryptoKey t Promise.t meth
  (** Import a key from raw bytes (the [raw], [spki] and [pkcs8] formats). *)

  method importKey_jwk :
       js_string t
    -> jsonWebKey
    -> algorithm
    -> bool t
    -> js_string t js_array t
    -> cryptoKey t Promise.t meth
  (** Import a key from a {!jsonWebKey} (the [jwk] format). *)

  method exportKey :
    js_string t -> cryptoKey t -> Typed_array.arrayBuffer t Promise.t meth
  (** Export to raw bytes (the [raw], [spki] and [pkcs8] formats). *)

  method exportKey_jwk : js_string t -> cryptoKey t -> jsonWebKey Promise.t meth
  (** Export to a {!jsonWebKey} (the [jwk] format). *)

  method wrapKey :
       js_string t
    -> cryptoKey t
    -> cryptoKey t
    -> algorithm
    -> Typed_array.arrayBuffer t Promise.t meth

  method unwrapKey :
       js_string t
    -> bufferSource
    -> cryptoKey t
    -> algorithm
    -> algorithm
    -> bool t
    -> js_string t js_array t
    -> cryptoKey t Promise.t meth
end

(** {1 Crypto} *)

class type crypto = object
  method subtle : subtleCrypto t readonly_prop

  method getRandomValues :
    'a 'b 'c.
    ('a, 'b, 'c) Typed_array.typedArray t -> ('a, 'b, 'c) Typed_array.typedArray t meth

  method randomUUID : js_string t meth
end

val crypto : unit -> crypto t
(** The [crypto] global ([globalThis.crypto]). *)

val subtle : unit -> subtleCrypto t
(** [(crypto ())##.subtle]. Only available in a secure context. *)

val is_supported : unit -> bool
(** Whether the [crypto] global is available in the current environment. *)
