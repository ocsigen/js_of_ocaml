
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
  ../examples/boulderdash/boulderdash.js
  (source_tree ../examples/boulderdash/sprites/)
  ../examples/webgl/index.html
  ../examples/webgl/webgldemo.js
  ../examples/graph_viewer/index.html
  ../examples/graph_viewer/viewer_js.js
  ../examples/planet/index.html
  ../examples/planet/texture.jpg
  ../examples/planet/planet.js
  ../examples/wiki/index.html
  ../examples/wiki/main.js
  ../examples/wysiwyg/index.html
  ../examples/wysiwyg/main.js
  ../examples/hyperbolic/index.html
  ../examples/hyperbolic/hypertree.js
  (source_tree ../examples/hyperbolic/icons/)
  (source_tree ../examples/hyperbolic/thumbnails/)
  ../examples/minesweeper/index.html
  ../examples/minesweeper/main.js
  (source_tree ../examples/minesweeper/sprites/)
  ../examples/cubes/index.html
  ../examples/cubes/cubes.js)
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
  (bash "cp -r ../examples/boulderdash/index.html ../examples/boulderdash/boulderdash.js ../examples/boulderdash/sprites files/boulderdash")
  (bash "cp -r ../examples/webgl/index.html ../examples/webgl/webgldemo.js files/webgl")
  (bash "cp -r ../examples/graph_viewer/index.html ../examples/graph_viewer/viewer_js.js files/graph_viewer")
  (bash "cp -r ../examples/planet/index.html ../examples/planet/texture.jpg ../examples/planet/planet.js files/planet")
  (bash "cp -r ../examples/wiki/index.html ../examples/wiki/main.js files/wiki")
  (bash "cp -r ../examples/wysiwyg/index.html ../examples/wysiwyg/main.js files/wysiwyg")
  (bash "cp -r ../examples/hyperbolic/index.html ../examples/hyperbolic/hypertree.js ../examples/hyperbolic/icons ../examples/hyperbolic/thumbnails files/hyperbolic")
  (bash "cp -r ../examples/minesweeper/index.html ../examples/minesweeper/main.js ../examples/minesweeper/sprites files/minesweeper")
  (bash "cp -r ../examples/cubes/index.html ../examples/cubes/cubes.js files/cubes"))))
