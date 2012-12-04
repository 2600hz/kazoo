ROOT = ../../..
REBAR = $(ROOT)/bin/rebar

ERL          ?= erl
ERLC		     ?= erlc
APP          := couchbeam

.PHONY: deps doc

all: compile

compile:
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

doc:
	@rebar doc	

test: all	
	@$(ERLC) -o t/ t/etap.erl
	prove t/*.t

verbose-test: compile	
	@$(ERLC) -o t/ t/etap.erl
	prove -v t/*.t 

cover: all
	COVER=1 prove t/*.t
	@$(ERL) -detached -noshell -eval 'etap_report:create()' -s init stop

clean: 
	@$(REBAR) clean
	@rm -f t/*.beam
	@rm -f doc/*.html doc/*.css doc/edoc-info doc/*.png

distclean: clean
	@$(REBAR) delete-deps
	@rm -rf deps

dialyzer: compile
	@dialyzer -Wno_return -c ebin

