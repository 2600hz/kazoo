ROOT = ../..
REBAR = $(ROOT)/utils/rebar/rebar
PREFIX:=../

all: compile

compile:
	@$(REBAR) compile

deps:
	$(REBAR) get-deps

edoc:
	@$(REBAR) doc

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean
