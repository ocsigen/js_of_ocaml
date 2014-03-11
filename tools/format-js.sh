#!/bin/bash

echo "# Removing tabs"
find ./ -regex "^\.\(/[a-zA-Z0-9_-.]*\)*.js" -exec sed -i 's/	//g' {} \;

echo "# Indent files"
SCRIPT=$(readlink -f $0)
SCRIPTPATH=`dirname $SCRIPT`
find ./ -regex "^\.\(/[a-zA-Z0-9_-.]*\)*.js" -exec emacs -batch {} -l ${SCRIPTPATH}/emacs-format-js.el -f emacs-format-js \;
