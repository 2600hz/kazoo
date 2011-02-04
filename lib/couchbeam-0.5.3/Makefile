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
	@mkdir -p doc/api
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[{preprocess, true},{includes, ["."]}, {dir, "./doc/api"}]'
	

test: compile	
	@$(ERLC) -o t/ t/etap.erl
	prove t/*.t

cover: all
	COVER=1 prove t/*.t
	@$(ERL) -detached -noshell -eval 'etap_report:create()' -s init stop

clean: 
	@./rebar clean
	@rm -f t/*.beam
	@rm -rf doc/api

distclean: clean
	@./rebar delete-deps

dialyzer: compile
	@dialyzer -Wno_return -c ebin

