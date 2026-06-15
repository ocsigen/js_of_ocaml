# Graphics_js tests

Tests for the `js_of_ocaml-lwt.graphics` runtime (`runtime/js/graphics.js`).

These are **not** run by `make tests`. They need a real `<canvas>` —
specifically `getImageData`, to read back rendered pixels — which Node does
not provide. So they are driven manually in a browser. `dune build @all` keeps
them compiling, but nothing here is attached to the `runtest` aliases.

## `test.ml` — self-checks

About 50 assertions covering the full `Graphics_js` API: `plot`/`point_color`
origin, `fill_rect` placement and extent, `fill_poly`/`fill_arc`/`fill_circle`/
`fill_ellipse`, strokes (`lineto`/`draw_rect`/`draw_*`), `curveto`, images
(`make_image`/`dump_image`/`draw_image`/`blit_image`/`get_image`, transparency),
text metrics, context management, and that the unimplemented primitives raise.

Build and run:

```sh
dune build compiler/tests-graphics/test.bc.js
# serve the build dir and open index.html in a browser:
(cd _build/default/compiler/tests-graphics && python3 -m http.server 8000)
# browse http://localhost:8000/  -> results in <pre id="results"> and the console
```

Headless (Chromium), extracting the result block:

```sh
cd _build/default/compiler/tests-graphics
python3 -m http.server 8000 &
chromium --headless --no-sandbox --virtual-time-budget=10000 \
  --dump-dom http://localhost:8000/index.html | grep -A100 'id="results"'
```

The last line is `SUMMARY: N passed, M failed`.

## `compare/` — pixel comparison against native X11 Graphics

Renders one shared scene (`gscene/scene.ml`) two ways and diffs the rasters:

- `native.exe` draws it with the real `graphics` library (X11) and dumps a
  binary PPM.
- `web.bc.js` draws it on a DOM canvas and dumps the raster as hex into a
  `<pre id="dump">`.
- `diff.py` compares them.

The scene is split into two bands (see `gscene/scene.ml`):

- **Exact zone** (`y < 99`): axis-aligned `fill_rect`/`fill_poly`, `plot`s and
  images. The native backend does no anti-aliasing, so these must match the
  canvas output **pixel-for-pixel** — `diff.py` requires 0 differences here.
- **AA zone** (`y >= 99`): curves, strokes and filled curves, whose edges are
  anti-aliased by the browser but hard-edged by X11. Per-pixel differences
  there are expected and tolerated.

Text is included in the AA zone purely for illustration; native and browser
fonts are different engines (X11 bitmap vs browser outline, points vs pixels),
so glyphs are never pixel-comparable — only the geometric primitives are.

Run (needs an X display for the native side and a browser for the web side):

```sh
dune build compiler/tests-graphics/compare/native.exe \
            compiler/tests-graphics/compare/web.bc.js
# native -> PPM
_build/default/compiler/tests-graphics/compare/native.exe native.ppm
# web -> hex (serve web.bc.js with an index.html that loads it, dump #dump)
# then:
python3 compiler/tests-graphics/compare/diff.py native.ppm web.hex
```

`diff.py` prints the exact-zone diff count (must be 0), the AA-zone diff count,
and exits non-zero if the exact zone differs.
