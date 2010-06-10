
TARGETS=*/

.PHONY: $(TARGETS)

all: $(TARGETS)

$(TARGETS):
	make -C $@

clean:
	for i in $(TARGETS); do make -C $$i clean; done
