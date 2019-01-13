#!/bin/bash

echo "# Removing tabs"
git ls-files | grep -E "[a-zA-Z0-9_-.]*\.ml[il]?(.in)?$" | xargs sed -i 's/	//g'

echo "# Whitespace"
SCRIPT=$(readlink -f $0)
SCRIPTPATH=`dirname $SCRIPT`
git ls-files | grep -E "[a-zA-Z0-9_-.]*\.ml[il]?(.in)?$" | xargs emacs -batch {} -l ${SCRIPTPATH}/emacs-format-ocaml.el -f emacs-format-function-clean

echo "# Indent files"
git ls-files | grep -E "[a-zA-Z0-9_-.]*\.ml[il]?(.in)?$" | xargs ocp-indent -i {}
