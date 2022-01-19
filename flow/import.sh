#!/bin/bash

[ -d flow ] || git clone git@github.com:facebook/flow.git
[ -d ocaml-wtf8 ] || git clone  git@github.com:flowtype/ocaml-wtf8.git
rm -rf parser
mkdir parser

cp flow/src/hack_forked/utils/collections/flow_set.ml parser/
cp flow/src/hack_forked/utils/collections/flow_map.ml parser/
cp flow/src/third-party/sedlex/flow_sedlexing.ml parser/
cp flow/src/third-party/sedlex/flow_sedlexing.mli parser/
cp flow/src/parser/*.ml parser/
cp flow/src/parser/*.mli parser/
rm parser/flow_parser_dot_js.ml
rm parser/flow_parser_js.ml
rm parser/relativeLoc.ml
rm parser/relativeLoc.mli

cat << EOF > parser/dune
(library
 (name flow_parser)
 (libraries sedlex)
 (preprocess (pps sedlex.ppx)))
EOF

cat << EOF > parser/.ocamlformat
disable=true
EOF

cp ocaml-wtf8/src/wtf8.ml parser/
cp ocaml-wtf8/src/wtf8.mli parser/

