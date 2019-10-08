ROOT = ..

MAKEDIRS = $(sort $(wildcard */Makefile))

.PHONY: all compile compile-test compile-test-direct clean clean-test eunit test $(MAKEDIRS)

all: compile

compile: ACTION = all
compile: $(MAKEDIRS)

compile-lean: ACTION = compile-lean
compile-lean: $(MAKEDIRS)

compile-test: ACTION = compile-test
compile-test: $(MAKEDIRS)

compile-test-direct: ACTION = compile-test-direct
compile-test-direct: $(MAKEDIRS)

clean: ACTION = clean
clean: $(MAKEDIRS)

clean-test: ACTION = clean-test
clean-test: $(MAKEDIRS)

eunit: ACTION = eunit
eunit: $(MAKEDIRS)

test: ACTION = test
test: $(MAKEDIRS)

$(MAKEDIRS):
	@$(MAKE) -j1 -C $(@D) $(ACTION)
