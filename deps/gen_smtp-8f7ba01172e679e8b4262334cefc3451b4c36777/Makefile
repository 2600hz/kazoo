ROOT = ../..
PROJECT = gen_smtp
REBAR = $(ROOT)/utils/rebar/rebar


all: compile
compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

test:
	$(REBAR) -C rebar.test.config get-deps
	$(REBAR) -C rebar.test.config compile
	ERL_AFLAGS="-s ssl"
	$(REBAR) -C rebar.test.config skip_deps=true eunit

dialyze: FILES = $(shell ls ebin/*.beam | grep -v smtp_rfc822_parse)
dialyze:
	dialyzer $(FILES)

.PHONY: compile clean test dialyze
