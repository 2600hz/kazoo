PROJECT = gen_smtp
ROOT = ../..
REBAR = $(ROOT)/utils/rebar/rebar

FILES = $(shell ls ebin/*.beam | grep -v smtp_rfc822_parse)

all:
	@./rebar compile

compile:
	@./rebar compile

clean:
	@./rebar clean

test:
	./rebar -C rebar.test.config get-deps
	./rebar -C rebar.test.config compile
	ERL_AFLAGS="-s ssl" 
	./rebar -C rebar.test.config skip_deps=true eunit

dialyze:
	dialyzer $(FILES)

.PHONY: compile clean test dialyze
