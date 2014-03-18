FILES= 	mlString.js ieee_754.js int64.js md5.js marshal.js \
	lexing.js parsing.js json.js bigarray.js unix.js \
	stdlib.js io.js jslib.js jslib_js_of_ocaml.js \
	internalMod.js gc.js

runtime.js: $(FILES)
	echo "//concat $(FILES)" > runtime.js
	for f in $(FILES) ; do \
		 echo "//# 1 \"$$f\"" >> runtime.js; \
	   cat $$f >> runtime.js; \
	done

clean:
	rm -f runtime.js
