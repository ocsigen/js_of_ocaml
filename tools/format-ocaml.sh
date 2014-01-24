#!/bin/bash

echo "# Removing tabs"
find ./ -regex "^\.\(/[a-zA-Z0-9_-.]*\)*\.ml[il]?" -exec sed -i 's/	//g' {} \;
echo "# Indent files"
find ./ -regex "^\.\(/[a-zA-Z0-9_-.]*\)*\.ml[il]?" -exec ocp-indent -i {} \;
