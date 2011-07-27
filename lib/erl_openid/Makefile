ERL ?= erl
APP := openid

.PHONY: deps
.PHONY: test
.PHONY: mod

all: deps
	@./rebar compile

deps:
	@./rebar get-deps

clean:
	@./rebar clean

mod:
	@./rebar create-mod app=$(APP) modid=$(MOD)

test:
	@./rebar eunit app=$(APP)

distclean: clean
	@./rebar delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'
