ERL ?= erl
EBIN_DEPS_DIRS := $(wildcard deps/*/ebin)

all: deps compile

compile:
	@./rebar compile

test:
	@./rebar eunit skip_deps=true

deps:
	@./rebar get-deps

run:
	@$(ERL) -pa ebin/ -pa $(EBIN_DEPS_DIRS)

clean:
	@./rebar clean

.PHONY: test
