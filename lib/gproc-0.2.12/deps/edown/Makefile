ROOT = ../../../..
REBAR = $(ROOT)/bin/rebar

.PHONY: all compile clean doc

all: compile

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

doc: compile
	./make_doc
