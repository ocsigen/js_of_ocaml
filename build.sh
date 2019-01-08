#!/bin/bash

set -e

docversions() {
    find "$1" -maxdepth 1 -type d -exec basename {} \; | grep -E '[0-9.]+|dev' | sort -Vr
}

find_wikis() {
    find "$1" -name '[a-zA-Z]*.wiki' | sort
}
dv="$(docversions doc)"
dvtmp=$(mktemp)

for v in $dv; do
    echo $v >> $dvtmp;
done

rm -rf _doc
mkdir _doc

export HOW_IN_PROJECT=t

for v in $dv; do
    echo "generating doc for $v"
    cp -r doc/$v _doc/$v
    for wiki in $(cd _doc && find_wikis $v | grep -v menu.wiki); do
	echo ohow --root _doc/$v --project js_of_ocaml --manual manual --api api --assets manual/files --images manual/files --docversions $dvtmp --template how_template/template.wiki _doc/$wiki
	ohow --root _doc/$v --project js_of_ocaml --manual manual --api api --assets manual/files --images manual/files --docversions $dvtmp --template how_template/template.wiki _doc/$wiki
    done
    for wiki in $(cd doc/$v && find_wikis .); do
	rm -f _doc/$v/$wiki
    done
done

HOW_LATEST=$(cat $dvtmp | sort -Vr | grep -v dev | head -n 1)
export HOW_LATEST
ln -s $HOW_LATEST _doc/latest
echo '<!DOCTYPE html><html><head><meta http-equiv="refresh" content="0; URL=latest/manual/overview" /></head><body></body></html>' > _doc/index.html
cp -r toplevel _doc/toplevel
cp -r .nojekyll _doc/.nojekyll
