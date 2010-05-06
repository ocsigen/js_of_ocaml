
boulderdash.js: boulderdash
	../../compile < boulderdash > boulderdash.js

boulderdash: jSOO.ml js.ml boulderdash.ml stubs.c
	ocamlc -custom -o boulderdash jSOO.ml js.ml boulderdash.ml stubs.c

clean:
	rm -f *.cm[io] *.o boulderdash boulderdash.js
