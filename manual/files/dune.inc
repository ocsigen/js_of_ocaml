
(rule
 (alias doc-manual)
 (targets (dir toplevel))
 (deps
  ../../toplevel/examples/lwt_toplevel/index.html
  ../../toplevel/examples/lwt_toplevel/toplevel.js
  ../../toplevel/examples/lwt_toplevel/toplevel.bc.js
  ../../toplevel/examples/lwt_toplevel/test_dynlink.js)
 (action (bash "mkdir -p toplevel && cp -rL ../../toplevel/examples/lwt_toplevel/index.html ../../toplevel/examples/lwt_toplevel/toplevel.js ../../toplevel/examples/lwt_toplevel/toplevel.bc.js ../../toplevel/examples/lwt_toplevel/test_dynlink.js toplevel")))

(rule
 (alias doc-manual)
 (targets (dir boulderdash))
 (deps
  ../../examples/boulderdash/boulderdash.bc.js
  ../../examples/boulderdash/index.html
  ../../examples/boulderdash/maps.txt
  (source_tree ../../examples/boulderdash/maps/)
  (source_tree ../../examples/boulderdash/sprites/))
 (action (bash "mkdir -p boulderdash && cp -rL ../../examples/boulderdash/boulderdash.bc.js ../../examples/boulderdash/index.html ../../examples/boulderdash/maps.txt ../../examples/boulderdash/maps ../../examples/boulderdash/sprites boulderdash")))

(rule
 (alias doc-manual)
 (targets (dir cubes))
 (deps
  ../../examples/cubes/cubes.bc.js
  ../../examples/cubes/index.html)
 (action (bash "mkdir -p cubes && cp -rL ../../examples/cubes/cubes.bc.js ../../examples/cubes/index.html cubes")))

(rule
 (alias doc-manual)
 (targets (dir graph_viewer))
 (deps
  ../../examples/graph_viewer/viewer_js.bc.js
  ../../examples/graph_viewer/scene.json
  ../../examples/graph_viewer/index.html)
 (action (bash "mkdir -p graph_viewer && cp -rL ../../examples/graph_viewer/viewer_js.bc.js ../../examples/graph_viewer/scene.json ../../examples/graph_viewer/index.html graph_viewer")))

(rule
 (alias doc-manual)
 (targets (dir graphics))
 (deps
  ../../examples/graphics/main.bc.js
  ../../examples/graphics/index.html)
 (action (bash "mkdir -p graphics && cp -rL ../../examples/graphics/main.bc.js ../../examples/graphics/index.html graphics")))

(rule
 (alias doc-manual)
 (targets (dir hyperbolic))
 (deps
  ../../examples/hyperbolic/hypertree.bc.js
  ../../examples/hyperbolic/index.html
  ../../examples/hyperbolic/image_info.json
  ../../examples/hyperbolic/messages.json
  ../../examples/hyperbolic/tree.json
  (source_tree ../../examples/hyperbolic/icons/)
  (source_tree ../../examples/hyperbolic/thumbnails/))
 (action (bash "mkdir -p hyperbolic && cp -rL ../../examples/hyperbolic/hypertree.bc.js ../../examples/hyperbolic/index.html ../../examples/hyperbolic/image_info.json ../../examples/hyperbolic/messages.json ../../examples/hyperbolic/tree.json ../../examples/hyperbolic/icons ../../examples/hyperbolic/thumbnails hyperbolic")))

(rule
 (alias doc-manual)
 (targets (dir minesweeper))
 (deps
  ../../examples/minesweeper/main.bc.js
  ../../examples/minesweeper/index.html
  (source_tree ../../examples/minesweeper/sprites/))
 (action (bash "mkdir -p minesweeper && cp -rL ../../examples/minesweeper/main.bc.js ../../examples/minesweeper/index.html ../../examples/minesweeper/sprites minesweeper")))

(rule
 (alias doc-manual)
 (targets (dir planet))
 (deps
  ../../examples/planet/planet.bc.js
  ../../examples/planet/index.html
  ../../examples/planet/texture.jpg)
 (action (bash "mkdir -p planet && cp -rL ../../examples/planet/planet.bc.js ../../examples/planet/index.html ../../examples/planet/texture.jpg planet")))

(rule
 (alias doc-manual)
 (targets (dir webgl))
 (deps
  ../../examples/webgl/webgldemo.bc.js
  ../../examples/webgl/monkey.model
  ../../examples/webgl/index.html)
 (action (bash "mkdir -p webgl && cp -rL ../../examples/webgl/webgldemo.bc.js ../../examples/webgl/monkey.model ../../examples/webgl/index.html webgl")))

(rule
 (alias doc-manual)
 (targets (dir webgl2_particles))
 (deps
  ../../examples/webgl2_particles/particles.bc.js
  ../../examples/webgl2_particles/index.html)
 (action (bash "mkdir -p webgl2_particles && cp -rL ../../examples/webgl2_particles/particles.bc.js ../../examples/webgl2_particles/index.html webgl2_particles")))

(rule
 (alias doc-manual)
 (targets (dir wiki))
 (deps
  ../../examples/wiki/main.bc.js
  ../../examples/wiki/index.html)
 (action (bash "mkdir -p wiki && cp -rL ../../examples/wiki/main.bc.js ../../examples/wiki/index.html wiki")))

(rule
 (alias doc-manual)
 (targets (dir wysiwyg))
 (deps
  ../../examples/wysiwyg/main.bc.js
  ../../examples/wysiwyg/index.html)
 (action (bash "mkdir -p wysiwyg && cp -rL ../../examples/wysiwyg/main.bc.js ../../examples/wysiwyg/index.html wysiwyg")))
