#!/bin/bash

set -e -u -o pipefail

echo "#include <stdlib.h>";
echo "#include <stdio.h>";
echo "#define D(f) void f () { fprintf(stderr, \"Unimplemented Javascript primitive %s!\\n\", #f); exit(1); }";
sed -n -e 's/.*external.*"\([^"%]*\)".*/D(\1)/p' *.ml | sort | uniq;
