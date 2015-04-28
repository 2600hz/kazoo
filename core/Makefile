ROOT = ../

MAKEDIRS = */Makefile

.PHONY: all compile clean test $(MAKEDIRS)

all: compile

compile: ACTION = all
compile: kazoo $(MAKEDIRS)

clean: ACTION = clean
clean: $(MAKEDIRS)

test: ACTION = test
test: $(MAKEDIRS)

kazoo:
	$(MAKE) -C whistle-1.0.0 compile

$(MAKEDIRS):
	$(MAKE) -C $(@D) $(ACTION)
