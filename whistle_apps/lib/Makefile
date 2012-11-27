ROOT = ../..
REBAR = $(ROOT)/bin/rebar

MAKEDIRS = */Makefile

.PHONY: all compile clean $(MAKEDIRS)

all: compile

compile: ACTION = all
compile: $(MAKEDIRS)
	$(REBAR) compile

clean: ACTION = clean
clean: $(MAKEDIRS)
	$(REBAR) clean

$(MAKEDIRS):
	$(MAKE) -C $(@D) $(ACTION)
