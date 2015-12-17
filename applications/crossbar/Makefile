PROJECT = crossbar
ROOT = ../..

EBINS = $(shell find $(ROOT)/core/whistle-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/whistle_number_manager-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/whistle_couch-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/whistle_transactions-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/braintree-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/kazoo_oauth-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/kazoo_bindings-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/lager-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/cowboy-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/ejson-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/rabbitmq_client-* -maxdepth 2 -name ebin -print)
PA = $(foreach EBIN,$(EBINS),-pa $(EBIN))

ERLC_OPTS += -Werror +debug_info +warn_export_all $(PA)
ELIBS = $(ERL_LIBS):$(subst $(eval) ,:,$(wildcard $(ROOT)/deps/rabbitmq_client-*/deps))

.PHONY: all compile clean

all: compile

MODULES = $(shell ls src/*.erl | sed 's/src\///;s/\.erl/,/' | sed '$$s/.$$//')
TEST_MODULES = $(shell ls test/*.erl | sed 's/test\///;s/\.erl/,/' | sed '$$s/.$$//')
CB_MODULES = $(shell ls src/modules/*.erl | sed 's/src\/modules\///;s/\.erl/,/' | sed '$$s/.$$//')
CB_MODULES_V1 = $(shell ls src/modules_v1/*.erl | sed 's/src\/modules_v1\///;s/\.erl/,/' | sed '$$s/.$$//')
CB_MODULES_V2 = $(shell ls src/modules_v2/*.erl | sed 's/src\/modules_v2\///;s/\.erl/,/' | sed '$$s/.$$//')

compile: ebin/$(PROJECT).app json
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES),$(CB_MODULES),$(CB_MODULES_V1),$(CB_MODULES_V2)\]}/' \
		> ebin/$(PROJECT).app
	-@$(MAKE) ebin/$(PROJECT).app

ebin/$(PROJECT).app: src/*.erl src/*/*.erl
	@mkdir -p ebin/
	ERL_LIBS=$(ELIBS) erlc -v $(ERLC_OPTS) -o ebin/ -pa ebin/ $?

json: JSON = $(shell find priv/couchdb -name *.json -print)
json:
	@$(ROOT)/scripts/format-json.sh $(JSON)

compile-test: test/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(TEST_MODULES),$(MODULES),$(CB_MODULES,$(CB_MODULES_V1),$(CB_MODULES_V2))\]}/' \
		> test/$(PROJECT).app
	-@$(MAKE) test/$(PROJECT).app

test/$(PROJECT).app: src/*.erl src/*/*.erl test/*.erl
	@mkdir -p test/
	ERL_LIBS=$(ELIBS) erlc -v $(ERLC_OPTS) -DTEST -o test/ -pa test/ $?

clean:
	rm -f ebin/*
	rm -f erl_crash.dump

clean-test:
	rm -f test/*.beam test/$(PROJECT).app

test: clean-test compile-test eunit

eunit: compile-test
	erl -noshell $(PA) -pa test -eval "case eunit:test([$(TEST_MODULES),$(MODULES),$(CB_MODULES),$(CB_MODULES_V1),$(CB_MODULES_V2)], [verbose]) of 'ok' -> init:stop(); _ -> init:stop(1) end."
