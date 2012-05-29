ERL          ?= erl
ERLC		     ?= erlc
APP          := couchbeam

.PHONY: deps doc

all: deps compile

compile:
	@./rebar compile

deps:
	@./rebar get-deps

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
	@./rebar clean
	@rm -f t/*.beam
	@rm -f doc/*.html doc/*.css doc/edoc-info doc/*.png

distclean: clean
	@./rebar delete-deps
	@rm -rf deps

dialyzer: compile
	@dialyzer -Wno_return -c ebin

