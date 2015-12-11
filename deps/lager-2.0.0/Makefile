ROOT = ../..
REBAR = $(ROOT)/utils/rebar/rebar

.PHONY: rel stagedevrel deps test

all: deps compile

compile: deps
	$(REBAR) compile

deps:
	test -d deps || $(REBAR) get-deps

clean:
	$(REBAR) clean

distclean: clean
	$(REBAR) delete-deps

test:
	$(REBAR) compile eunit

##
## Doc targets
##
docs:
	$(REBAR) doc

APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	xmerl webtool snmp public_key mnesia eunit
COMBO_PLT = $(HOME)/.riak_combo_dialyzer_plt

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS)

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS)

dialyzer: compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wunmatched_returns --plt $(COMBO_PLT) ebin | \
	    fgrep -v -f ./dialyzer.ignore-warnings

cleanplt:
	@echo
	@echo "Are you sure?  It takes about 1/2 hour to re-build."
	@echo Deleting $(COMBO_PLT) in 5 seconds.
	@echo
	sleep 5
	rm $(COMBO_PLT)

