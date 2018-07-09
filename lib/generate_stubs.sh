set -e -u -o pipefail
shopt -s extglob

echo "#include <stdlib.h>";
echo "#include <stdio.h>";
sed -n -e 's/.*external.*"\([^"%]*\)".*/\1/p' !(*.pp).ml | sort | uniq | \
    while read prim; do
        echo "void ${prim} () {
               fprintf(stderr, \"Unimplemented Javascript primitive ${prim}!\\n\");
               exit(1);
              }";
    done;
