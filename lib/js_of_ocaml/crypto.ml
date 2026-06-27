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
open! Import

type algorithm = Js.Unsafe.any

let algorithm (s : Js.js_string Js.t) : algorithm = Js.Unsafe.inject s

let algorithm_obj (o : < .. > Js.t) : algorithm = Js.Unsafe.inject o

type bufferSource = Js.Unsafe.any

let buffer_source_of_array_buffer (b : Typed_array.arrayBuffer Js.t) : bufferSource =
  Js.Unsafe.inject b

let buffer_source_of_array_buffer_view (v : #Typed_array.arrayBufferView Js.t) :
    bufferSource =
  Js.Unsafe.inject v

module Key_type = struct
  type t =
    | Public
    | Private
    | Secret

  let to_js = function
    | Public -> Js.string "public"
    | Private -> Js.string "private"
    | Secret -> Js.string "secret"

  let of_js s =
    match Js.to_string s with
    | "public" -> Public
    | "private" -> Private
    | "secret" -> Secret
    | other -> invalid_arg ("Crypto.Key_type.of_js: " ^ other)
end

module Key_usage = struct
  type t =
    | Encrypt
    | Decrypt
    | Sign
    | Verify
    | Derive_key
    | Derive_bits
    | Wrap_key
    | Unwrap_key

  let to_js = function
    | Encrypt -> Js.string "encrypt"
    | Decrypt -> Js.string "decrypt"
    | Sign -> Js.string "sign"
    | Verify -> Js.string "verify"
    | Derive_key -> Js.string "deriveKey"
    | Derive_bits -> Js.string "deriveBits"
    | Wrap_key -> Js.string "wrapKey"
    | Unwrap_key -> Js.string "unwrapKey"

  let of_js s =
    match Js.to_string s with
    | "encrypt" -> Encrypt
    | "decrypt" -> Decrypt
    | "sign" -> Sign
    | "verify" -> Verify
    | "deriveKey" -> Derive_key
    | "deriveBits" -> Derive_bits
    | "wrapKey" -> Wrap_key
    | "unwrapKey" -> Unwrap_key
    | other -> invalid_arg ("Crypto.Key_usage.of_js: " ^ other)

  let list_to_js l = Js.array (Array.of_list (List.map to_js l))

  let list_of_js a = List.map of_js (Array.to_list (Js.to_array a))
end

module Key_format = struct
  type t =
    | Raw
    | Spki
    | Pkcs8
    | Jwk

  let to_js = function
    | Raw -> Js.string "raw"
    | Spki -> Js.string "spki"
    | Pkcs8 -> Js.string "pkcs8"
    | Jwk -> Js.string "jwk"

  let of_js s =
    match Js.to_string s with
    | "raw" -> Raw
    | "spki" -> Spki
    | "pkcs8" -> Pkcs8
    | "jwk" -> Jwk
    | other -> invalid_arg ("Crypto.Key_format.of_js: " ^ other)
end

class type cryptoKey = object
  method _type : Js.js_string Js.t Js.readonly_prop

  method extractable : bool Js.t Js.readonly_prop

  method algorithm : algorithm Js.readonly_prop

  method usages : Js.js_string Js.t Js.js_array Js.t Js.readonly_prop
end

class type cryptoKeyPair = object
  method publicKey : cryptoKey Js.t Js.readonly_prop

  method privateKey : cryptoKey Js.t Js.readonly_prop
end

type jsonWebKey = Js.Unsafe.any

type params =
  | RSASSA_PKCS1_v1_5
  | RSA_PSS of { salt_length : int }
  | RSA_OAEP of { label : bufferSource option }
  | ECDSA of { hash : algorithm }
  | ECDH of { public : cryptoKey Js.t }
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
  | AES_KW
  | HMAC
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
      { name : Js.js_string Js.t
      ; modulus_length : int
      ; public_exponent : Typed_array.uint8Array Js.t
      ; hash : algorithm
      }
  | RSA_import of
      { name : Js.js_string Js.t
      ; hash : algorithm
      }
  | EC_keygen of
      { name : Js.js_string Js.t
      ; named_curve : Js.js_string Js.t
      }
  | AES_keygen of
      { name : Js.js_string Js.t
      ; length : int
      }
  | HMAC_keygen of
      { hash : algorithm
      ; length : int option
      }
  | SHA_1
  | SHA_256
  | SHA_384
  | SHA_512
  | Other of Js.js_string Js.t

let to_algorithm (p : params) : algorithm =
  let str s : Js.Unsafe.any = Js.Unsafe.inject (Js.string s) in
  let i n : Js.Unsafe.any = Js.Unsafe.inject (n : int) in
  let obj l : algorithm = Js.Unsafe.inject (Js.Unsafe.obj (Array.of_list l)) in
  let opt name f = function
    | None -> []
    | Some v -> [ name, f v ]
  in
  match p with
  | SHA_1 -> algorithm (Js.string "SHA-1")
  | SHA_256 -> algorithm (Js.string "SHA-256")
  | SHA_384 -> algorithm (Js.string "SHA-384")
  | SHA_512 -> algorithm (Js.string "SHA-512")
  | AES_KW -> algorithm (Js.string "AES-KW")
  | HMAC -> algorithm (Js.string "HMAC")
  | RSASSA_PKCS1_v1_5 -> algorithm (Js.string "RSASSA-PKCS1-v1_5")
  | Other name -> algorithm name
  | RSA_PSS { salt_length } -> obj [ "name", str "RSA-PSS"; "saltLength", i salt_length ]
  | RSA_OAEP { label } -> obj (("name", str "RSA-OAEP") :: opt "label" (fun b -> b) label)
  | ECDSA { hash } -> obj [ "name", str "ECDSA"; "hash", hash ]
  | ECDH { public } -> obj [ "name", str "ECDH"; "public", Js.Unsafe.inject public ]
  | AES_CTR { counter; length } ->
      obj [ "name", str "AES-CTR"; "counter", counter; "length", i length ]
  | AES_CBC { iv } -> obj [ "name", str "AES-CBC"; "iv", iv ]
  | AES_GCM { iv; additional_data; tag_length } ->
      obj
        ([ "name", str "AES-GCM"; "iv", iv ]
        @ opt "additionalData" (fun b -> b) additional_data
        @ opt "tagLength" i tag_length)
  | HKDF { hash; salt; info } ->
      obj [ "name", str "HKDF"; "hash", hash; "salt", salt; "info", info ]
  | PBKDF2 { hash; salt; iterations } ->
      obj [ "name", str "PBKDF2"; "hash", hash; "salt", salt; "iterations", i iterations ]
  | RSA_keygen { name; modulus_length; public_exponent; hash } ->
      obj
        [ "name", Js.Unsafe.inject name
        ; "modulusLength", i modulus_length
        ; "publicExponent", Js.Unsafe.inject public_exponent
        ; "hash", hash
        ]
  | RSA_import { name; hash } -> obj [ "name", Js.Unsafe.inject name; "hash", hash ]
  | EC_keygen { name; named_curve } ->
      obj [ "name", Js.Unsafe.inject name; "namedCurve", Js.Unsafe.inject named_curve ]
  | AES_keygen { name; length } ->
      obj [ "name", Js.Unsafe.inject name; "length", i length ]
  | HMAC_keygen { hash; length } ->
      obj (("name", str "HMAC") :: ("hash", hash) :: opt "length" i length)

let of_algorithm (a : algorithm) : params =
  let name_only s =
    match s with
    | "SHA-1" -> SHA_1
    | "SHA-256" -> SHA_256
    | "SHA-384" -> SHA_384
    | "SHA-512" -> SHA_512
    | "AES-KW" -> AES_KW
    | "HMAC" -> HMAC
    | "RSASSA-PKCS1-v1_5" -> RSASSA_PKCS1_v1_5
    | _ -> Other (Js.string s)
  in
  if String.equal (Js.to_string (Js.typeof a)) "string"
  then name_only (Js.to_string (Js.Unsafe.coerce a : Js.js_string Js.t))
  else begin
    let o : < .. > Js.t = Js.Unsafe.coerce a in
    let field k = Js.Unsafe.get o (Js.string k) in
    let has k = Js.Optdef.test (field k : 'a Js.Optdef.t) in
    let str k : Js.js_string Js.t = field k in
    let int_ k : int = field k in
    let alg k : algorithm = field k in
    let buf k : bufferSource = field k in
    let opt_buf k = if has k then Some (buf k) else None in
    let opt_int k = if has k then Some (int_ k) else None in
    let rsa_keygen () =
      RSA_keygen
        { name = str "name"
        ; modulus_length = int_ "modulusLength"
        ; public_exponent = field "publicExponent"
        ; hash = alg "hash"
        }
    in
    let ec_keygen () = EC_keygen { name = str "name"; named_curve = str "namedCurve" } in
    let aes_keygen () = AES_keygen { name = str "name"; length = int_ "length" } in
    let rsa_import () = RSA_import { name = str "name"; hash = alg "hash" } in
    match Js.to_string (str "name") with
    | "SHA-1" -> SHA_1
    | "SHA-256" -> SHA_256
    | "SHA-384" -> SHA_384
    | "SHA-512" -> SHA_512
    | "RSASSA-PKCS1-v1_5" ->
        if has "modulusLength"
        then rsa_keygen ()
        else if has "hash"
        then rsa_import ()
        else RSASSA_PKCS1_v1_5
    | "RSA-PSS" ->
        if has "modulusLength"
        then rsa_keygen ()
        else if has "saltLength"
        then RSA_PSS { salt_length = int_ "saltLength" }
        else if has "hash"
        then rsa_import ()
        else Other (str "name")
    | "RSA-OAEP" ->
        if has "modulusLength"
        then rsa_keygen ()
        else if has "hash"
        then rsa_import ()
        else RSA_OAEP { label = opt_buf "label" }
    | "ECDSA" -> if has "namedCurve" then ec_keygen () else ECDSA { hash = alg "hash" }
    | "ECDH" ->
        if has "namedCurve" then ec_keygen () else ECDH { public = field "public" }
    | "AES-CTR" ->
        if has "counter"
        then AES_CTR { counter = buf "counter"; length = int_ "length" }
        else if has "length"
        then aes_keygen ()
        else Other (str "name")
    | "AES-CBC" ->
        if has "iv"
        then AES_CBC { iv = buf "iv" }
        else if has "length"
        then aes_keygen ()
        else Other (str "name")
    | "AES-GCM" ->
        if has "iv"
        then
          AES_GCM
            { iv = buf "iv"
            ; additional_data = opt_buf "additionalData"
            ; tag_length = opt_int "tagLength"
            }
        else if has "length"
        then aes_keygen ()
        else Other (str "name")
    | "AES-KW" -> if has "length" then aes_keygen () else AES_KW
    | "HMAC" ->
        if has "hash"
        then HMAC_keygen { hash = alg "hash"; length = opt_int "length" }
        else HMAC
    | "HKDF" -> HKDF { hash = alg "hash"; salt = buf "salt"; info = buf "info" }
    | "PBKDF2" ->
        PBKDF2 { hash = alg "hash"; salt = buf "salt"; iterations = int_ "iterations" }
    | _ -> Other (str "name")
  end

class type subtleCrypto = object
  method encrypt :
       algorithm
    -> cryptoKey Js.t
    -> bufferSource
    -> Typed_array.arrayBuffer Js.t Promise.t Js.meth

  method decrypt :
       algorithm
    -> cryptoKey Js.t
    -> bufferSource
    -> Typed_array.arrayBuffer Js.t Promise.t Js.meth

  method sign :
       algorithm
    -> cryptoKey Js.t
    -> bufferSource
    -> Typed_array.arrayBuffer Js.t Promise.t Js.meth

  method verify :
       algorithm
    -> cryptoKey Js.t
    -> bufferSource
    -> bufferSource
    -> bool Js.t Promise.t Js.meth

  method digest :
    algorithm -> bufferSource -> Typed_array.arrayBuffer Js.t Promise.t Js.meth

  method generateKey :
       algorithm
    -> bool Js.t
    -> Js.js_string Js.t Js.js_array Js.t
    -> cryptoKey Js.t Promise.t Js.meth

  method generateKey_pair :
       algorithm
    -> bool Js.t
    -> Js.js_string Js.t Js.js_array Js.t
    -> cryptoKeyPair Js.t Promise.t Js.meth

  method deriveKey :
       algorithm
    -> cryptoKey Js.t
    -> algorithm
    -> bool Js.t
    -> Js.js_string Js.t Js.js_array Js.t
    -> cryptoKey Js.t Promise.t Js.meth

  method deriveBits :
    algorithm -> cryptoKey Js.t -> Typed_array.arrayBuffer Js.t Promise.t Js.meth

  method deriveBits_length :
    algorithm -> cryptoKey Js.t -> int -> Typed_array.arrayBuffer Js.t Promise.t Js.meth

  method importKey :
       Js.js_string Js.t
    -> bufferSource
    -> algorithm
    -> bool Js.t
    -> Js.js_string Js.t Js.js_array Js.t
    -> cryptoKey Js.t Promise.t Js.meth

  method importKey_jwk :
       Js.js_string Js.t
    -> jsonWebKey
    -> algorithm
    -> bool Js.t
    -> Js.js_string Js.t Js.js_array Js.t
    -> cryptoKey Js.t Promise.t Js.meth

  method exportKey :
    Js.js_string Js.t -> cryptoKey Js.t -> Typed_array.arrayBuffer Js.t Promise.t Js.meth

  method exportKey_jwk :
    Js.js_string Js.t -> cryptoKey Js.t -> jsonWebKey Promise.t Js.meth

  method wrapKey :
       Js.js_string Js.t
    -> cryptoKey Js.t
    -> cryptoKey Js.t
    -> algorithm
    -> Typed_array.arrayBuffer Js.t Promise.t Js.meth

  method unwrapKey :
       Js.js_string Js.t
    -> bufferSource
    -> cryptoKey Js.t
    -> algorithm
    -> algorithm
    -> bool Js.t
    -> Js.js_string Js.t Js.js_array Js.t
    -> cryptoKey Js.t Promise.t Js.meth
end

class type crypto = object
  method subtle : subtleCrypto Js.t Js.readonly_prop

  method getRandomValues :
    'a 'b 'c.
       ('a, 'b, 'c) Typed_array.typedArray Js.t
    -> ('a, 'b, 'c) Typed_array.typedArray Js.t Js.meth

  method randomUUID : Js.js_string Js.t Js.meth
end

let crypto_global = Js.Unsafe.global##.crypto

let is_supported () = Js.Optdef.test crypto_global

let crypto () : crypto Js.t = crypto_global

let subtle () : subtleCrypto Js.t = crypto_global##.subtle
