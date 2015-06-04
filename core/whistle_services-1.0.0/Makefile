PROJECT = whistle_services
ROOT = ../..

EBINS = $(shell find $(ROOT)/core/whistle-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/lager-* -maxdepth 2 -name ebin -print)
PA = $(foreach EBIN,$(EBINS),-pa $(EBIN))

ERLC_OPTS = -Werror +debug_info +warn_export_all $(PA)
ERL_LIBS = $(subst $(eval) ,:,$(wildcard $(ROOT)/core))

.PHONY: all compile clean

all: compile

MODULES = $(shell ls src/*.erl | sed 's/src\///;s/\.erl/,/' | sed '$$s/.$$//')
BK_MODULES = $(shell ls src/bookkeepers/*.erl | sed 's/src\/bookkeepers\///;s/\.erl/,/' | sed '$$s/.$$//')
SRV_MODULES = $(shell ls src/services/*.erl | sed 's/src\/services\///;s/\.erl/,/' | sed '$$s/.$$//')

compile: ebin/$(PROJECT).app json
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES),$(BK_MODULES),$(SRV_MODULES)\]}/' \
		> ebin/$(PROJECT).app
	-@$(MAKE) ebin/$(PROJECT).app

ebin/$(PROJECT).app: src/*.erl src/bookkeepers/*.erl src/services/*.erl
	@mkdir -p ebin/
	ERL_LIBS=$(ERL_LIBS) erlc -v $(ERLC_OPTS) -o ebin/ -pa ebin/ $?

json: JSON = $(shell find priv -name *.json -print)
json:
	@for jobj in $(JSON); do \
		echo checking $$jobj; \
		python -c 'import sys,json; json.encoder.FLOAT_REPR=str; data=json.load(open("'$$jobj'")); print json.dumps(data, sort_keys=True, indent=4, separators=(",", ": "))' >$$jobj~ && mv $$jobj~ $$jobj; \
	done

compile-test: test/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES),$(BK_MODULES),$(SRV_MODULES)\]}/' \
		> test/$(PROJECT).app
	-@$(MAKE) test/$(PROJECT).app

test/$(PROJECT).app: src/*.erl src/bookkeepers/*.erl src/services/*.erl
	@mkdir -p test/
	ERL_LIBS=$(ERL_LIBS) erlc -v $(ERLC_OPTS) -DTEST -o test/ -pa test/ $?

clean:
	rm -f ebin/*
	rm -f erl_crash.dump

clean-test:
	rm -f test/*.beam test/$(PROJECT).app

test: clean-test compile-test eunit

eunit: compile-test
	erl -noshell $(PA) -pa test -eval "case eunit:test([$(MODULES),$(BK_MODULES),$(SRV_MODULES)], [verbose]) of 'ok' -> init:stop(); _ -> init:stop(1) end."
