include ../Makefile.filelist

runtime.js: $(JSOO_RUNTIME)
	echo "//concat $(JSOO_RUNTIME)" > runtime.js
	for f in $(JSOO_RUNTIME) ; do \
		 echo "//# 1 \"$$f\"" >> runtime.js; \
	   cat $$f >> runtime.js; \
	done

clean:
	rm -f runtime.js
