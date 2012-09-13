ROOT = ../..
REBAR = $(ROOT)/bin/rebar

ERL ?= erl
APP := openid

.PHONY: deps
.PHONY: test
.PHONY: mod

all: app

app: compile

compile:
	$(REBAR) compile

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean

mod:
	$(REBAR) create-mod app=$(APP) modid=$(MOD)

test:
	$(REBAR) eunit app=$(APP)

distclean: clean
	$(REBAR) delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'
