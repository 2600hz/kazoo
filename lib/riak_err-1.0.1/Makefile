ROOT = ../..
REBAR = $(ROOT)/bin/rebar

all: app

app: compile

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

test: all
	$(REBAR) eunit

build_plt: all
	$(REBAR) build-plt

analyze: all
	$(REBAR) dialyze

doc: all
	$(REBAR) doc

xref: all
	$(REBAR) xref
