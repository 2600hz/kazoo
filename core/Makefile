ROOT = ../
REBAR = $(ROOT)/utils/rebar/rebar

MAKEDIRS = */Makefile

.PHONY: all compile clean $(MAKEDIRS)

all: compile

compile: ACTION = all
compile: kazoo $(MAKEDIRS)
	$(REBAR) compile

clean: ACTION = clean
clean: $(MAKEDIRS)
	$(REBAR) clean

kazoo:
	$(MAKE) -C whistle-1.0.0 compile

$(MAKEDIRS):
	$(MAKE) -C $(@D) $(ACTION)
