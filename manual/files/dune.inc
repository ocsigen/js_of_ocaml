
(rule
 (alias doc-manual)
 (targets (dir toplevel))
 (deps
  ../../toplevel/examples/lwt_toplevel/index.html
  ../../toplevel/examples/lwt_toplevel/toplevel.js
  ../../toplevel/examples/lwt_toplevel/toplevel.bc.js
  ../../toplevel/examples/lwt_toplevel/test_dynlink.js)
 (action (bash "mkdir -p toplevel && cp -r ../../toplevel/examples/lwt_toplevel/index.html ../../toplevel/examples/lwt_toplevel/toplevel.js ../../toplevel/examples/lwt_toplevel/toplevel.bc.js ../../toplevel/examples/lwt_toplevel/test_dynlink.js toplevel")))

(rule
 (alias doc-manual)
 (targets (dir boulderdash))
 (deps
  ../../examples/boulderdash/boulderdash.bc.js
  ../../examples/boulderdash/index.html
  (source_tree ../../examples/boulderdash/sprites/))
 (action (bash "mkdir -p boulderdash && cp -r ../../examples/boulderdash/boulderdash.bc.js ../../examples/boulderdash/index.html ../../examples/boulderdash/sprites boulderdash")))

(rule
 (alias doc-manual)
 (targets (dir cubes))
 (deps
  ../../examples/cubes/cubes.bc.js
  ../../examples/cubes/index.html)
 (action (bash "mkdir -p cubes && cp -r ../../examples/cubes/cubes.bc.js ../../examples/cubes/index.html cubes")))

(rule
 (alias doc-manual)
 (targets (dir graph_viewer))
 (deps
  ../../examples/graph_viewer/viewer_js.bc.js
  ../../examples/graph_viewer/index.html)
 (action (bash "mkdir -p graph_viewer && cp -r ../../examples/graph_viewer/viewer_js.bc.js ../../examples/graph_viewer/index.html graph_viewer")))

(rule
 (alias doc-manual)
 (targets (dir graphics))
 (deps
  ../../examples/graphics/main.bc.js
  ../../examples/graphics/index.html)
 (action (bash "mkdir -p graphics && cp -r ../../examples/graphics/main.bc.js ../../examples/graphics/index.html graphics")))

(rule
 (alias doc-manual)
 (targets (dir hyperbolic))
 (deps
  ../../examples/hyperbolic/hypertree.bc.js
  ../../examples/hyperbolic/index.html
  (source_tree ../../examples/hyperbolic/icons/)
  (source_tree ../../examples/hyperbolic/thumbnails/))
 (action (bash "mkdir -p hyperbolic && cp -r ../../examples/hyperbolic/hypertree.bc.js ../../examples/hyperbolic/index.html ../../examples/hyperbolic/icons ../../examples/hyperbolic/thumbnails hyperbolic")))

(rule
 (alias doc-manual)
 (targets (dir minesweeper))
 (deps
  ../../examples/minesweeper/main.bc.js
  ../../examples/minesweeper/index.html
  (source_tree ../../examples/minesweeper/sprites/))
 (action (bash "mkdir -p minesweeper && cp -r ../../examples/minesweeper/main.bc.js ../../examples/minesweeper/index.html ../../examples/minesweeper/sprites minesweeper")))

(rule
 (alias doc-manual)
 (targets (dir planet))
 (deps
  ../../examples/planet/planet.bc.js
  ../../examples/planet/index.html
  ../../examples/planet/texture.jpg)
 (action (bash "mkdir -p planet && cp -r ../../examples/planet/planet.bc.js ../../examples/planet/index.html ../../examples/planet/texture.jpg planet")))

(rule
 (alias doc-manual)
 (targets (dir webgl))
 (deps
  ../../examples/webgl/webgldemo.bc.js
  ../../examples/webgl/index.html)
 (action (bash "mkdir -p webgl && cp -r ../../examples/webgl/webgldemo.bc.js ../../examples/webgl/index.html webgl")))

(rule
 (alias doc-manual)
 (targets (dir wiki))
 (deps
  ../../examples/wiki/main.bc.js
  ../../examples/wiki/index.html)
 (action (bash "mkdir -p wiki && cp -r ../../examples/wiki/main.bc.js ../../examples/wiki/index.html wiki")))

(rule
 (alias doc-manual)
 (targets (dir wysiwyg))
 (deps
  ../../examples/wysiwyg/main.bc.js
  ../../examples/wysiwyg/index.html)
 (action (bash "mkdir -p wysiwyg && cp -r ../../examples/wysiwyg/main.bc.js ../../examples/wysiwyg/index.html wysiwyg")))
