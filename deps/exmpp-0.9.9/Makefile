PROJECT = exmpp
ROOT = ../..
REBAR = $(ROOT)/utils/rebar/rebar

all: app

app: compile

compile: 
	@mkdir -p ebin
	@$(REBAR) compile

deps:
	@rebar get-deps

clean:
	@$(REBAR) clean
