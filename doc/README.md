# How the documentation is generated

The Js_of_ocaml documentation published at <https://ocsigen.org/js_of_ocaml/> is
built entirely with **odoc** and themed with the Ocsigen site chrome by
[**wodoc**](https://github.com/ocsigen/wodoc) (an odoc driver). The same odoc
sources are also what ocaml.org renders.

## Sources

| What | Where | Format |
|---|---|---|
| Manual | [`manual/*.mld`](../manual) | odoc pages |
| Manual home / API overview | `manual/index.mld`, `manual/api.mld` | odoc pages |
| API | the `.mli` of every `js_of_ocaml*` package | odoc comments (native `{!…}` refs) |
| Interactive examples | `examples/`, `toplevel/examples/` | built by dune |
| Site configuration (nav, packages, …) | [`doc/wodoc`](wodoc) | wodoc config (S-expression) |

The manual used to be wikicreole (`manual/*.wiki`, rendered by `html_of_wiki`).
It has been converted to `.mld` (`wodoc convert`) so that a single odoc run
produces the manual **and** the API, and so the manual also appears on ocaml.org.
The left navigation, theme and per-page assembly used to be hand-written HTML and
Python scripts; they are now declared in [`doc/wodoc`](wodoc) and produced by
`wodoc build` (the page template, the version selector, the left navigation and
the cross-package reference resolution are all built in).

## Build

```
wodoc build --config doc/wodoc --label dev --out _doc-site/dev \
  --menu https://raw.githubusercontent.com/ocsigen/ocsigen.github.io/master/wodoc/menu.html
```

That single command (no wrapper script) does everything — `--menu` takes the
shared top menu's canonical copy in `ocsigen.github.io` (fetched for you). It:

1. runs `dune build @doc @doc-manual` — odoc HTML for the manual and the API of
   every `js_of_ocaml*` package in one run, plus the interactive examples
   (toplevel, boulderdash, …) into `_build/default/manual/files/`;
2. assembles every page into the Ocsigen site (shared header/menu/drawer, the
   version `<select>`, the left navigation declared in `doc/wodoc`), links the
   cross-package references to the sibling packages (`js_of_ocaml-lwt`, `-tyxml`,
   `-toplevel`), and ships the examples, the version redirect and the `latest`
   symlink.

Output goes to `<outdir>/<label>/` (default `_doc-site/<label>/`), laid out to
match `https://ocsigen.org/js_of_ocaml/<label>/`. Internal links are
version-relative; only the version `<select>` is absolute (`/js_of_ocaml`). The
themed stylesheet is served centrally at `/css/ocsigen-odoc.css` by ocsigen.org.

## Deployment (CI)

[`.github/workflows/doc.yml`](../.github/workflows/doc.yml) builds and publishes
to the project's **`gh-pages`** branch (served at `ocsigen.org/js_of_ocaml/`):

- **push to `master`** → rebuilds and deploys the **`dev`** docs (`dev/`).
- **manual run** (Actions → *Documentation* → *Run workflow*) → builds any
  version. For a release: set *label* to the version (e.g. `6.3.1`), *ref* to the
  tag, and tick *set_latest* to repoint `latest`. A tag cut **before** this
  migration has no `doc/` infra, so the workflow overlays the (version-independent)
  doc sources from `master`.

Each run replaces only its own `<label>/` directory; the other version
directories already on `gh-pages` are preserved. The `js_of_ocaml.yml` CI also
runs `dune build @doc @doc-manual` on PRs as a compile check.
