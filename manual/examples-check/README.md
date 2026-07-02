# Checked documentation examples

This directory checks the code examples embedded in the js_of_ocaml
documentation — both the API doc-comments (`.mli`) and the manual pages
(`.mld`) — so an example that drifts out of sync with the API breaks the build
instead of silently rotting (see issue #1020).

## The convention: checked by default, opt out with a marker

An `{@ocaml[ … ]}` code block is **type-checked by default**. Two metadata
markers on the block opt out:

| Block | Meaning |
|---|---|
| `{@ocaml[ … ]}` | **type-checked** (compiled against the real library) |
| `{@ocaml parse[ … ]}` | **parse-only** — syntax-checked, not type-checked |
| `{@ocaml skip[ … ]}` | **ignored** — not even parsed |
| `{@ocaml prelude[ … ]}` | **setup** prepended to the type unit (see below) |
| `{[ … ]}` | never checked (carries no metadata) |

- Use **`parse`** for an example that references hypothetical/undefined
  identifiers or placeholder types (`type1`, a free `obj`, …) but should still be
  valid OCaml. It catches syntax rot without demanding a self-contained program.
- Use **`skip`** for pseudo-code, elisions (`…`), toplevel/REPL transcripts, or
  fragments that are not OCaml at all.

## Preludes: type-check more without cluttering the docs

A `{@ocaml prelude[ … ]}` block is prepended, at top level, to the type unit,
so the checked examples can refer to whatever it binds (a shared `open`, a helper
value, a placeholder type). This lets an example that would otherwise be
`parse`-only become fully type-checked without spelling out the setup in every
snippet.

In an `.mli`, put the prelude in a **plain `(* … *)` comment** (not a `(** … *)`
doc-comment): odoc does not render plain comments, so the setup stays out of the
published documentation, while this checker still reads the raw text, e.g.

```
(* Shared setup for the checked examples in this file — not rendered by odoc.
{@ocaml prelude[
open Js_of_ocaml
let element = Dom_html.document##.documentElement
]} *)
```

lets the file's examples use `element` (and drop a repeated `open`) without
that boilerplate appearing in the rendered docs. (`.mld` pages have no
invisible-comment syntax, so a prelude there would render; prefer an injected
`--open` or a self-contained example on manual pages.) No `.mli` is wired into
the check today; this is available for when one is.

All of these render **identically** — odoc emits `<pre class="language-ocaml">`
and strips the metadata — so readers never see the marker; it only tells this
checker what to do.

## How it works

`extract_examples` pulls the blocks of a chosen tier out of the listed files and
emits one module per block, with a `# line` directive back to the source:

- **type tier** → compiled against `js_of_ocaml` (+ `js_of_ocaml-ppx`, so
  `##`/`##.`/`object%js` work) as a **library**. Building the `.cma` type-checks
  every module but skips the final link, so an example that declares or uses a
  JS-only `external` still checks. The examples are **never executed** (their
  top-level side effects touch the DOM, which only exists in a browser).
- **parse tier** → `ocamlc -stop-after parsing` (no ppx, no injected opens —
  `##`/`%js` parse as plain OCaml and names are never resolved). Undefined
  identifiers are fine; a real syntax error fails. (Opens are not injected here
  because an `open` before a bare-expression block is itself a syntax error.)

A failure is reported at the original source line, e.g.:

```
File "../javascript-interop.mld", line 493, characters 12-16:
Error: The value args has type Js.Unsafe.any_js_array = Js.Unsafe.top Js.t
       but an expression was expected of type < .. > Js.t
```

## Self-contained vs. injected opens

Checked blocks need their opens in scope. Two styles, chosen per source file in
[`dune`](dune):

- **`.mli` API examples are self-contained** — the block includes its own
  `open Js_of_ocaml`, so the rendered example is copy-pasteable.
- **`.mld` manual pages get their ambient opens injected** (`--open
  Js_of_ocaml`), because a page assumes them throughout; repeating the `open` in
  every small block would be noise.

## Current coverage

The whole manual (`manual/*.mld`) — every page that has `{@ocaml[ … ]}` blocks
is wired in. The API `.mli` doc-comments are not wired in yet.

Pages using the base `js_of_ocaml` library share one type unit and one parse
unit. `manual/ppx-deriving.mld` has its own unit built with the
`ppx_deriving_json` preprocessor and `js_of_ocaml.deriving` runtime.

## Adding files to the check

Add the `.mli`/`.mld` to the `extract_examples` invocations in [`dune`](dune)
(both the type-tier and, if the page has `parse` blocks, the parse-tier rule).
Files needing a different library or ppx (e.g. the deriving page) get their own
generated unit built against that library, following the `examples_deriving`
example.
