ROOT = ..

MAKEDIRS = */Makefile
ERL_LIBS = $(CURDIR)/$(ROOT)/deps/

.PHONY: all compile compile-test clean clean-test eunit test first $(MAKEDIRS)

all: compile

compile: ACTION = all
compile: $(MAKEDIRS)

compile-test: ACTION = compile-test
compile-test: $(MAKEDIRS)

clean: ACTION = clean
clean: $(MAKEDIRS)

clean-test: ACTION = clean-test
clean-test: $(MAKEDIRS)

eunit: ACTION = eunit
eunit: $(MAKEDIRS)

test: ACTION = test
test: $(MAKEDIRS)

first:
	ERL_LIBS=$(ERL_LIBS) $(MAKE) -C whistle/ $(ACTION)

$(MAKEDIRS): first
	ERL_LIBS=$(ERL_LIBS) $(MAKE) -C $(@D)    $(ACTION)
