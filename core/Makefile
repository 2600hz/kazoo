ROOT = ../

MAKEDIRS = */Makefile

.PHONY: all compile clean $(MAKEDIRS)

all: compile

compile: ACTION = all
compile: kazoo $(MAKEDIRS)

clean: ACTION = clean
clean: $(MAKEDIRS)

kazoo:
	$(MAKE) -C whistle-1.0.0 compile

$(MAKEDIRS):
	$(MAKE) -C $(@D) $(ACTION)
