ERL ?= erl
APP := webmachine_demo

.PHONY: deps

all: deps
	@../rebar compile

deps:
	@../rebar get-deps

clean:
	@../rebar clean

distclean: clean
	@../rebar delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'
