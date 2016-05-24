
all: myruntime.js stdlib.js module1.js module2.js

include ../Makefile.common


clean::
	rm -f *.cmo *.cmi *.js *.map

JSFILES=../../runtime/runtime.js ../../runtime/predefined_exceptions.js
MYCOMP=$(COMP) --noruntime --pretty --source-map $(OPTIONS)

myruntime.js:
	$(MYCOMP) --runtime-only $(JSFILES) empty -o myruntime.js
stdlib.js:
	$(MYCOMP) `ocamlc -where`/stdlib.cma -o stdlib.js
module1.js: module1.cmo
	$(MYCOMP) module1.cmo -o module1.js
module2.js: module2.cmo
	$(MYCOMP) module2.cmo -o module2.js

module1.cmo module1.cmi: module1.ml
	ocamlc -g -c module1.ml
module2.cmo module2.cmi: module2.ml module1.cmi
	ocamlc -g -c module2.ml
