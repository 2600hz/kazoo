ROOT = ..

MAKEDIRS = */Makefile

.PHONY: all compile clean test $(MAKEDIRS)

all: compile

compile: ACTION = all
compile: kazoo $(MAKEDIRS)

clean: ACTION = clean
clean: $(MAKEDIRS)

clean-test: ACTION = clean-test
clean-test: $(MAKEDIRS)

test: ACTION = test
test: $(MAKEDIRS)

kazoo:
	$(MAKE) -C $(wildcard whistle-*) compile

$(MAKEDIRS):
	$(MAKE) -C $(@D) $(ACTION)
