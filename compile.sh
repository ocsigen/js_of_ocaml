#!/bin/bash

-e

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

export HOW_IN_PROJECT=t

for v in $dv; do
    echo "generating doc for $v"
    mkdir -p _doc/$v
    for wiki in $(cd doc/$v && find_wikis .); do
	mkdir -p $(dirname _doc/$v/$wiki)
	cp doc/$v/$wiki _doc/$v/$wiki
    done
    for wiki in $(cd _doc && find_wikis $v | grep -v menu.wiki); do
	ohow --root _doc/$v --project js_of_ocaml --manual manual --api api --assets manual/files --images manual/files --docversions $dvtmp --template how_template/template.wiki _doc/$wiki
    done
    for wiki in $(cd doc/$v && find_wikis .); do
	rm -f _doc/$v/$wiki
    done
done
