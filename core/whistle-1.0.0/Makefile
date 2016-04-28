PROJECT = whistle
ROOT = ../..

EBINS = $(shell find $(ROOT)/deps/lager-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/whistle-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/whistle_couch-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/couchbeam-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/rabbitmq_client-* -maxdepth 2 -name ebin -print)
PA = $(foreach EBIN,$(EBINS),-pa $(EBIN))

ERLC_OPTS += -Werror +debug_info +warn_export_all $(PA)
ELIBS = $(ERL_LIBS):$(subst $(eval) ,:,$(wildcard $(ROOT)/deps/rabbitmq_client-*/deps))

TEST_EBINS = $(shell find $(ROOT)/deps/mochiweb-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/ejson-* -maxdepth 2 -name ebin -print)
TEST_PA = $(foreach EBIN,$(TEST_EBINS),-pa $(EBIN))

.PHONY: all compile clean clean-test

all: compile

MODULES = $(shell ls src/*.erl | sed 's/src\///;s/\.erl/,/' | sed '$$s/.$$//')
API_MODULES = $(shell ls src/api/*.erl | sed 's/src\/api\///;s/\.erl/,/' | sed '$$s/.$$//')
TEST_MODULES = $(shell ls test/*.erl | sed 's/test\///;s/\.erl/,/' | sed '$$s/.$$//')

compile: ebin/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES),$(API_MODULES)\]}/' \
		> ebin/$(PROJECT).app
	-@$(MAKE) ebin/$(PROJECT).app

ebin/$(PROJECT).app: src/kazoo_transform.erl src/*.erl src/api/*.erl
	@mkdir -p ebin/
	ERL_LIBS=$(ELIBS) erlc -v $(ERLC_OPTS) -o ebin/ -pa ebin/ $(PA) $?

compile-test: test/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES),$(API_MODULES),$(TEST_MODULES)\]}/' \
		> test/$(PROJECT).app
	-@$(MAKE) test/$(PROJECT).app

test/$(PROJECT).app: src/kazoo_transform.erl src/*.erl src/api/*.erl test/*.erl
	@mkdir -p test/
	ERL_LIBS=$(ELIBS) erlc -v $(ERLC_OPTS) $(TEST_PA) -DTEST -o test/ -pa test/ $?

clean:
	rm -f ebin/*
	rm -f erl_crash.dump

clean-test:
	rm -f test/*.beam test/$(PROJECT).app

test: clean-test compile-test eunit

eunit: compile-test
	erl -noshell $(PA) $(TEST_PA) -pa test -eval "case eunit:test([$(MODULES),$(TEST_MODULES)], [verbose]) of 'ok' -> init:stop(); _ -> init:stop(1) end."
