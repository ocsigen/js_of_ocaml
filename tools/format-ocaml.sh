#!/bin/bash

echo "# Removing tabs"
find ./ -regex "^\.\(/[a-zA-Z0-9_-.]*\)*\.ml[il]?" -exec sed -i 's/	//g' {} \;

echo "# Whitespace"
SCRIPT=$(readlink -f $0)
SCRIPTPATH=`dirname $SCRIPT`
find ./ -regex "^\.\(/[a-zA-Z0-9_-.]*\)*\.ml[il]?" -exec emacs -batch {} -l ${SCRIPTPATH}/emacs-format-ocaml.el -f emacs-format-function-clean \;

echo "# Indent files"
find ./ -regex "^\.\(/[a-zA-Z0-9_-.]*\)*\.ml[il]?" -exec ocp-indent -i {} \;
