ROOT = ../..
REBAR = $(ROOT)/bin/rebar
PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

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

build_plt:
	@$(REBAR) build-plt

dialyzer:
	@$(REBAR) dialyze

app:
	@$(REBAR) create template=mochiwebapp dest=$(DEST) appid=$(PROJECT)
