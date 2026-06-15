
# Module `Js_of_ocaml_lwt.Promise`


### Type safety

Native JavaScript promises automatically flatten any thenable returned from a handler or passed to `Promise.resolve`. That means a JavaScript `Promise` can never resolve with another `Promise` as its value, which is unsound at the OCaml type level for, say, `'a t t`.

These bindings work around that by wrapping thenable values in a small container before resolving and unwrapping on the way out; non-thenable values are passed through unchanged so the common case pays no allocation. As a result, `'a t` values always resolve with a value of OCaml type `'a`, even when `'a` is itself `'_ t`.


### Interop with raw JavaScript promises

Use [`of_any`](./#val-of_any) / [`to_any`](./#val-to_any) to interoperate with promises coming from outside this module. A `'a t` obtained via [`of_any`](./#val-of_any) from a foreign promise will not be wrapped, so it is up to the caller to ensure the types line up.

```ocaml
type +'a t = 'a Js_of_ocaml.Promise.t
```
```ocaml
type error = Js_of_ocaml.Promise.error
```
The reason a promise was rejected. JavaScript allows rejecting with any value (not necessarily an `Error`), so [`error`](./#type-error) is opaque; use [`error_to_any`](./#val-error_to_any) to inspect it and [`error_of_any`](./#val-error_of_any) to construct one.


## Errors

```ocaml
val error_of_any : Js_of_ocaml.Js.Unsafe.any -> error
```
```ocaml
val error_to_any : error -> Js_of_ocaml.Js.Unsafe.any
```
```ocaml
val error_of_exn : exn -> error
```
Use an OCaml exception as a rejection reason.


## Building promises

```ocaml
val resolve : 'a -> 'a t
```
A promise already fulfilled with the given value.

```ocaml
val reject : error -> 'a t
```
A promise already rejected with the given reason.

```ocaml
val make : (resolve:('a -> unit) -> reject:(error -> unit) -> unit) -> 'a t
```
`make f` runs `f` synchronously with two callbacks; `f` is expected to eventually invoke either `resolve` or `reject` to settle the promise.

```ocaml
val with_resolvers : unit -> 'a t * ('a -> unit) * (error -> unit)
```
`with_resolvers ()` returns `(p, resolve, reject)` where `p` is a fresh promise that is settled by calling `resolve x` (to fulfill with `x`) or `reject e` (to reject with `e`). Bound to the `Promise.withResolvers()` static method (ES2024).


## Chaining

```ocaml
val then_ : ?on_error:(error -> 'b t) -> ('a -> 'b t) -> 'a t -> 'b t
```
`then_ f p` returns a new promise that, when `p` fulfills with `x`, fulfills (or rejects) like `f x`. If `p` is rejected, the rejection is propagated.

If `~on_error` is supplied, this is the two-callback form of `.then(f, g)` in JavaScript: `on_error` only fires for rejections of `p` itself. If `f` returns a rejected promise, `on_error` does *not* catch it; chain a [`catch`](./#val-catch) afterwards if you need that.

```ocaml
val catch : (error -> 'a t) -> 'a t -> 'a t
```
`catch f p` returns a new promise that, when `p` is rejected with reason `e`, fulfills (or rejects) like `f e`. If `p` is fulfilled, its value is propagated.

```ocaml
val finally : (unit -> unit) -> 'a t -> 'a t
```
`finally f p` returns a new promise that settles the same way as `p`, after invoking `f` for its side effect.

```ocaml
val map : ('a -> 'b) -> 'a t -> 'b t
```
`map f p` is `then_ (fun x -> resolve (f x)) p`.

```ocaml
val bind : ('a -> 'b t) -> 'a t -> 'b t
```
Alias for [`then_`](./#val-then_).


## Combinators

```ocaml
val all : 'a t list -> 'a list t
```
`all ps` resolves with the values of all promises in `ps`, in order, or rejects with the reason of the first promise to reject.

```ocaml
val all_settled : 'a t list -> ('a, error) Stdlib.result list t
```
`all_settled ps` resolves once every promise in `ps` has settled, with a list in the same order: `Ok v` for a fulfilled promise and `Error e` for a rejected one. The returned promise never rejects.

```ocaml
val any : 'a t list -> 'a t
```
`any ps` fulfills as soon as any promise in `ps` fulfills. If every promise rejects, the returned promise rejects with an `AggregateError` whose `errors` property lists the rejection reasons.

```ocaml
val race : 'a t list -> 'a t
```
`race ps` settles like the first promise in `ps` to settle, fulfilled or rejected.


## Unsafe interop

```ocaml
val to_any : 'a t -> Js_of_ocaml.Js.Unsafe.any
```
Expose the underlying JavaScript promise. The resolved value may be the internal wrapper rather than the raw payload; consumers that observe the value should chain a `.then` that unwraps via [`of_any`](./#val-of_any) → [`then_`](./#val-then_) rather than reading `.then` directly from the foreign side.

```ocaml
val of_any : Js_of_ocaml.Js.Unsafe.any -> 'a t
```
Treat a foreign JavaScript promise as a `'a t`. The returned value is only sound if the underlying promise actually resolves with a value of type `'a`; raw foreign values are passed through [`then_`](./#val-then_) unchanged.


## Capability detection

```ocaml
val is_supported : unit -> bool
```
Whether the JavaScript `Promise` global is available in the current environment.

```ocaml
exception Rejected of Js_of_ocaml.Promise.error
```
Raised by the `Lwt.t` returned by [`to_lwt`](./#val-to_lwt) when the underlying `Promise` rejects. The rejection reason is wrapped unchanged — even when it is an OCaml exception (e.g. produced via [`Promise.error_of_exn`](./#val-error_of_exn)), use [`Promise.error_to_any`](./#val-error_to_any) to inspect it.

```ocaml
val to_lwt : 'a Js_of_ocaml.Promise.t -> 'a Lwt.t
```
`to_lwt p` is an Lwt thread that fulfils with the value `p` resolves with, or fails with [`Rejected`](./#exception-Rejected) if `p` is rejected.

```ocaml
val of_lwt : 'a Lwt.t -> 'a Js_of_ocaml.Promise.t
```
`of_lwt t` is a `Promise` that resolves with the value `t` returns, or rejects with `Promise.error_of_exn exn` if `t` fails.
