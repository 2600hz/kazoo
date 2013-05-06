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
	rm -rf cowboy-0.8.4/ ranch-0.8.1
	$(REBAR) clean

$(MAKEDIRS):
	$(MAKE) -C $(@D) $(ACTION)
