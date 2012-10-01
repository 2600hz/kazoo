ROOT = ../..
REBAR = $(ROOT)/bin/rebar

all: compile

compile:
	$(REBAR) compile

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean

distclean: clean
	$(REBAR) delete-deps

test:
	$(REBAR) eunit
