PLTFILE=$(CURDIR)/.deps.plt
APP_DEPS=kernel stdlib eunit tools compiler
ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin

REBAR="./rebar"
ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif

ERL = $(shell which erl)
ifeq ($(ERL),)
$(error "Erlang must be available on this system")
endif

BUILD_PLT_INC=$(shell test -d deps && echo '-r deps')
DIALYZER_INC=$(shell test -d include && echo '-I include') $(shell test -d deps && echo '-I deps')

.PHONY: all rebuild compile doc clean test dialyzer typer get-deps clean-deps \
	shell clean-plt clean-doc distclean

all: get-deps compile

rebuild: distclean get-deps all

get-deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

doc:
	@$(REBAR) -C test.config skip_deps=true doc

clean:
	@$(REBAR) -C test.config skip_deps=true clean

test: get-deps compile
	@$(REBAR) -C test.config compile
	@$(REBAR) -C test.config skip_deps=true eunit

$(PLTFILE):
	- dialyzer --build_plt --apps $(APP_DEPS) $(BUILD_PLT_INC) --output_plt $(PLTFILE)

dialyzer: compile $(PLTFILE)
	@dialyzer --fullpath --plt $(PLTFILE) $(DIALYZER_INC) -pa $(CURDIR)/ebin -c src --src | \
	fgrep -v -f ./dialyzer.ignore-warnings

typer:
	typer --plt $(PLTFILE) $(DIALYZER_INC) -r src

shell:
	@$(ERL) $(ERLFLAGS)

clean-plt:
	@rm -rf $(PLTFILE)

clean-doc:
	@cd doc; ls * | grep -v overview.edoc | xargs rm -f

clean-deps:
	@rm -rvf $(CURDIR)/deps/*

distclean: clean clean-plt clean-doc clean-deps
