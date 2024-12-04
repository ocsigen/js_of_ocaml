
(rule
 (alias doc-manual)
 (deps
  (glob_files *wiki)
  (source_tree files)
  ../toplevel/examples/lwt_toplevel/index.html
  ../toplevel/examples/lwt_toplevel/toplevel.js
  ../toplevel/examples/lwt_toplevel/toplevel.bc.js
  ../toplevel/examples/lwt_toplevel/test_dynlink.js
  ../examples/boulderdash/index.html
  ../examples/boulderdash/boulderdash.bc.js
  (source_tree ../examples/boulderdash/sprites/)
  ../examples/webgl/index.html
  ../examples/webgl/webgldemo.bc.js
  ../examples/graph_viewer/index.html
  ../examples/graph_viewer/viewer_js.bc.js
  ../examples/planet/index.html
  ../examples/planet/texture.jpg
  ../examples/planet/planet.bc.js
  ../examples/wiki/index.html
  ../examples/wiki/main.bc.js
  ../examples/wysiwyg/index.html
  ../examples/wysiwyg/main.bc.js
  ../examples/hyperbolic/index.html
  ../examples/hyperbolic/hypertree.bc.js
  (source_tree ../examples/hyperbolic/icons/)
  (source_tree ../examples/hyperbolic/thumbnails/)
  ../examples/minesweeper/index.html
  ../examples/minesweeper/main.bc.js
  (source_tree ../examples/minesweeper/sprites/)
  ../examples/cubes/index.html
  ../examples/cubes/cubes.bc.js)
 (action (progn
  (bash "mkdir -p files/toplevel")
  (bash "mkdir -p files/boulderdash")
  (bash "mkdir -p files/webgl")
  (bash "mkdir -p files/graph_viewer")
  (bash "mkdir -p files/planet")
  (bash "mkdir -p files/wiki")
  (bash "mkdir -p files/wysiwyg")
  (bash "mkdir -p files/hyperbolic")
  (bash "mkdir -p files/minesweeper")
  (bash "mkdir -p files/cubes")(bash "cp -r ../toplevel/examples/lwt_toplevel/index.html ../toplevel/examples/lwt_toplevel/toplevel.js ../toplevel/examples/lwt_toplevel/toplevel.bc.js ../toplevel/examples/lwt_toplevel/test_dynlink.js files/toplevel")
  (bash "cp -r ../examples/boulderdash/index.html ../examples/boulderdash/boulderdash.bc.js ../examples/boulderdash/sprites files/boulderdash")
  (bash "cp -r ../examples/webgl/index.html ../examples/webgl/webgldemo.bc.js files/webgl")
  (bash "cp -r ../examples/graph_viewer/index.html ../examples/graph_viewer/viewer_js.bc.js files/graph_viewer")
  (bash "cp -r ../examples/planet/index.html ../examples/planet/texture.jpg ../examples/planet/planet.bc.js files/planet")
  (bash "cp -r ../examples/wiki/index.html ../examples/wiki/main.bc.js files/wiki")
  (bash "cp -r ../examples/wysiwyg/index.html ../examples/wysiwyg/main.bc.js files/wysiwyg")
  (bash "cp -r ../examples/hyperbolic/index.html ../examples/hyperbolic/hypertree.bc.js ../examples/hyperbolic/icons ../examples/hyperbolic/thumbnails files/hyperbolic")
  (bash "cp -r ../examples/minesweeper/index.html ../examples/minesweeper/main.bc.js ../examples/minesweeper/sprites files/minesweeper")
  (bash "cp -r ../examples/cubes/index.html ../examples/cubes/cubes.bc.js files/cubes"))))
